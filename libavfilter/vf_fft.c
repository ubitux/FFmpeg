/*
 * Copyright (c) 2013 Clément Bœsch
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include "libavcodec/avfft.h"
#include "libavutil/eval.h"
#include "libavutil/opt.h"
#include "internal.h"

#define NBITS 4
#define BSIZE (1<<(NBITS))

#define WINDOWING 0
#define WEIGHT    1
#define EXPR      1

#if EXPR
static const char *const var_names[] = { "psd", NULL };
enum { VAR_PSD, VAR_VARS_NB };
#endif

typedef struct {
    const AVClass *class;
#if EXPR
    char *expr_str;
    AVExpr *expr;
    double var_values[VAR_VARS_NB];
#else
    float sigma;
    float th;
#endif

    float *cbuf[2][3];
    int p_linesize;
    float *weights;

    int overlap;
    int step;
    DCTContext *dct, *idct;
    float *block, *tmp_block;

#if WINDOWING
    float *win_fn;
#endif

} FFTFilterContext;

#define OFFSET(x) offsetof(FFTFilterContext, x)
#define FLAGS AV_OPT_FLAG_FILTERING_PARAM|AV_OPT_FLAG_VIDEO_PARAM
static const AVOption fft_options[] = {
#if EXPR
    { "e", "set dct factor expression", OFFSET(expr_str), AV_OPT_TYPE_STRING, {.str=NULL}, .flags = FLAGS },
#else
    { "s", NULL, OFFSET(sigma), AV_OPT_TYPE_FLOAT, {.dbl=0}, 0, 9999, .flags = FLAGS },
#endif
    { "overlap", NULL, OFFSET(overlap), AV_OPT_TYPE_INT, {.i64=15}, 0, (1<<NBITS)-1, .flags = FLAGS },
    { NULL },
};

AVFILTER_DEFINE_CLASS(fft);

static float *dct_block(FFTFilterContext *ctx, const float *src, int src_linesize)
{
    int x, y;
    float *column;
#if WINDOWING
    const float *win_fn = ctx->win_fn;
#endif

    for (y = 0; y < BSIZE; y++) {
        float *line = ctx->block;

#if WINDOWING
        for (x = 0; x < BSIZE; x++)
            line[x] = src[x] * *win_fn++;
#else
        memcpy(line, src, BSIZE * sizeof(*line));
#endif
        src += src_linesize;
        av_dct_calc(ctx->dct, line);

        column = ctx->tmp_block + y;
        for (x = 0; x < BSIZE; x++) {
            *column = *line++;
            column += BSIZE;
        }
    }

    column = ctx->tmp_block;
    for (x = 0; x < BSIZE; x++) {
        av_dct_calc(ctx->dct, column);
        column += BSIZE;
    }

    for (y = 0; y < BSIZE; y++)
        for (x = 0; x < BSIZE; x++)
            ctx->block[y*BSIZE + x] = ctx->tmp_block[x*BSIZE + y];

    return ctx->block;
}

static void idct_block(FFTFilterContext *ctx, float *dst, int dst_linesize)
{
    int x, y;
    float *column = ctx->tmp_block;

    for (y = 0; y < BSIZE; y++)
        for (x = 0; x < BSIZE; x++)
            ctx->tmp_block[x*BSIZE + y] = ctx->block[y*BSIZE + x];

    for (x = 0; x < BSIZE; x++) {
        av_dct_calc(ctx->idct, column);
        column += BSIZE;
    }

    for (y = 0; y < BSIZE; y++) {
        float *line = ctx->block;

        for (x = 0; x < BSIZE; x++)
            ctx->block[x] = ctx->tmp_block[x*BSIZE + y];

        av_dct_calc(ctx->idct, line);
        for (x = 0; x < BSIZE; x++)
#if WEIGHT
            dst[x] += line[x];
#else
            dst[x] = line[x];
#endif
        dst += dst_linesize;
    }
}

#if WINDOWING
static float *get_window_function(int w, int h)
{
    int x, y;
    const float scale = 1. / sqrt(w * h);
    float *t = av_malloc(w * h * sizeof(*t));

    if (!t)
        return NULL;

#define HANN(i, winsize) (.5f * (1 - cos(2*M_PI*(i) / ((winsize)-1))))
    for (y = 0; y < h; y++)
        for (x = 0; x < w; x++)
            t[y*w + x] = HANN(x, w) * HANN(y, h); // * scale;
    return t;
}
#endif

static int config_input(AVFilterLink *inlink)
{
    FFTFilterContext *fft = inlink->dst->priv;
    const int linesize = FFALIGN(inlink->w, 16);
    int i, x, y, bx, by, *iweights;

    fft->p_linesize = linesize;
    for (i = 0; i < 2; i++) {
        fft->cbuf[i][0] = av_malloc(linesize * inlink->h * sizeof(*fft->cbuf[i][0]));
        fft->cbuf[i][1] = av_malloc(linesize * inlink->h * sizeof(*fft->cbuf[i][1]));
        fft->cbuf[i][2] = av_malloc(linesize * inlink->h * sizeof(*fft->cbuf[i][2]));
        if (!fft->cbuf[i][0] || !fft->cbuf[i][1] || !fft->cbuf[i][2])
            return AVERROR(ENOMEM);
    }

    fft->weights = av_malloc(inlink->h * linesize * sizeof(*fft->weights));
    if (!fft->weights)
        return AVERROR(ENOMEM);
    iweights = av_calloc(inlink->h, linesize * sizeof(*iweights));
    if (!iweights)
        return AVERROR(ENOMEM);
    for (y = 0; y < inlink->h - BSIZE + 1; y += fft->step)
        for (x = 0; x < inlink->w - BSIZE + 1; x += fft->step)
            for (by = 0; by < BSIZE; by++)
                for (bx = 0; bx < BSIZE; bx++)
                    iweights[(y + by)*linesize + x + bx]++;
    for (y = 0; y < inlink->h; y++)
        for (x = 0; x < inlink->w; x++)
            fft->weights[y*linesize + x] = 1. / iweights[y*linesize + x];
    av_free(iweights);

    return 0;
}

static av_cold int init(AVFilterContext *ctx)
{
#if EXPR
    int ret;
#endif
    FFTFilterContext *fft = ctx->priv;

#if EXPR
    if (!fft->expr_str) {
        av_log(ctx, AV_LOG_ERROR, "expression is mandatory\n");
        return AVERROR(EINVAL);
    }

    ret = av_expr_parse(&fft->expr, fft->expr_str, var_names,
                        NULL, NULL, NULL, NULL, 0, ctx);
    if (ret < 0)
        return ret;
#else
    fft->th = fft->sigma * 3.;
    fft->th *= (BSIZE / 2.); // FIXME: shouldn't be necessary...
#endif

    fft->dct  = av_dct_init(NBITS, DCT_II);
    fft->idct = av_dct_init(NBITS, DCT_III);
    fft->block     = av_malloc(BSIZE * BSIZE * sizeof(*fft->block));
    fft->tmp_block = av_malloc(BSIZE * BSIZE * sizeof(*fft->tmp_block));

    if (!fft->dct || !fft->idct || !fft->tmp_block || !fft->block)
        return AVERROR(ENOMEM);

    fft->step = BSIZE - fft->overlap;

#if WINDOWING
    fft->win_fn = get_window_function(BSIZE, BSIZE);
    if (!fft->win_fn)
        return AVERROR(ENOMEM);
#endif
    return 0;
}

static int query_formats(AVFilterContext *ctx)
{
    // XXX: add more
    static const enum AVPixelFormat pix_fmts[] = {
        AV_PIX_FMT_RGB24,
        AV_PIX_FMT_NONE
    };
    ff_set_common_formats(ctx, ff_make_format_list(pix_fmts));
    return 0;
}

// TODO: construct a transposed dct3ch based on color_map
// TODO: fixed point?
static const float dct3ch[3][3] = {
    { 0.577350258827209472656250,  0.57735025882720947265625,  0.577350258827209472656250 },
    { 0.707106769084930419921875,  0.00000000000000000000000, -0.707106769084930419921875 },
    { 0.408248305320739746093750, -0.81649661064147949218750,  0.408248305320739746093750 },
};

#define DBG 0

static void color_decorrelation(float **dst, int dst_linesize,
                                const uint8_t *src, int src_linesize, int w, int h)
{
    int x, y;
    float *dstp_r = dst[0];
    float *dstp_g = dst[1];
    float *dstp_b = dst[2];

    for (y = 0; y < h; y++) {
        const uint8_t *srcp = src;

        for (x = 0; x < w; x++) {
            dstp_r[x] = srcp[0] * dct3ch[0][0] + srcp[1] * dct3ch[0][1] + srcp[2] * dct3ch[0][2];
            dstp_g[x] = srcp[0] * dct3ch[1][0] + srcp[1] * dct3ch[1][1] + srcp[2] * dct3ch[1][2];
            dstp_b[x] = srcp[0] * dct3ch[2][0] + srcp[1] * dct3ch[2][1] + srcp[2] * dct3ch[2][2];
            srcp += 3;
        }
        src += src_linesize;
        dstp_r += dst_linesize;
        dstp_g += dst_linesize;
        dstp_b += dst_linesize;
    }
}

static void color_correlation(uint8_t *dst, int dst_linesize,
                              float **src, int src_linesize, int w, int h)
{
    int x, y;
    const float *src_r = src[0];
    const float *src_g = src[1];
    const float *src_b = src[2];

    for (y = 0; y < h; y++) {
        uint8_t *dstp = dst;
        for (x = 0; x < w; x++) {
            dstp[0] = av_clip_uint8(src_r[x] * dct3ch[0][0] + src_g[x] * dct3ch[1][0] + src_b[x] * dct3ch[2][0]);
            dstp[1] = av_clip_uint8(src_r[x] * dct3ch[0][1] + src_g[x] * dct3ch[1][1] + src_b[x] * dct3ch[2][1]);
            dstp[2] = av_clip_uint8(src_r[x] * dct3ch[0][2] + src_g[x] * dct3ch[1][2] + src_b[x] * dct3ch[2][2]);
            dstp += 3;
        }
        src_r += src_linesize;
        src_g += src_linesize;
        src_b += src_linesize;
        dst += dst_linesize;
    }
}

static void filter_plane(AVFilterContext *ctx,
                         float *dst, int dst_linesize,
                         const float *src, int src_linesize,
                         int w, int h)
{
    int x, y, bx, by;
    FFTFilterContext *fft = ctx->priv;
#if WEIGHT
    float *dst0 = dst;
    const float *weights = fft->weights;
#endif

    memset(dst, 0, h * dst_linesize * sizeof(*dst));

    for (y = 0; y < h - BSIZE + 1; y += fft->step) {
        for (x = 0; x < w - BSIZE + 1; x += fft->step) {
            float *ftb = dct_block(fft, src + x, src_linesize);
#if DBG
            av_log(0,0,"INPUT:\n");
            for (by = 0; by < BSIZE; by++) {
                for (bx = 0; bx < BSIZE; bx++)
                    av_log(0,0," %10g", src[(y+by)*src_linesize + x+bx]);
                av_log(0,0,"\n");
            }
            av_log(0,0,"\n");
            av_log(0,0,"OUTPUT:\n");
            for (by = 0; by < BSIZE; by++) {
                for (bx = 0; bx < BSIZE; bx++)
                    av_log(0,0," %10g", ftb[by*BSIZE + bx]);
                av_log(0,0,"\n");
            }
            av_log(0,0,"\n");
#endif

            for (by = 0; by < BSIZE; by++) {
                for (bx = 0; bx < BSIZE; bx++) {
#if EXPR
                    fft->var_values[VAR_PSD] = FFABS(*ftb) * (1 / (3. * BSIZE / 2.));
                    *ftb *= av_expr_eval(fft->expr, fft->var_values, fft);
#else
                    if (FFABS(*ftb) < fft->th)
                        *ftb = 0;
#endif
                    ftb++;
                }
            }
            idct_block(fft, dst + x, dst_linesize);
        }
        src += fft->step * src_linesize;
        dst += fft->step * dst_linesize;
    }

#if WEIGHT
    dst = dst0;
    for (y = 0; y < h; y++) {
        for (x = 0; x < w; x++)
            dst[x] *= weights[x];
        dst += dst_linesize;
        weights += dst_linesize;
    }
#endif
}

static int filter_frame(AVFilterLink *inlink, AVFrame *in)
{
    AVFilterContext *ctx = inlink->dst;
    FFTFilterContext *fft = ctx->priv;
    AVFilterLink *outlink = inlink->dst->outputs[0];
    int direct, plane;
    AVFrame *out;

    if (av_frame_is_writable(in)) {
        direct = 1;
        out = in;
    } else {
        direct = 0;
        out = ff_get_video_buffer(outlink, outlink->w, outlink->h);
        if (!out) {
            av_frame_free(&in);
            return AVERROR(ENOMEM);
        }
        av_frame_copy_props(out, in);
    }

    color_decorrelation(fft->cbuf[0], fft->p_linesize, in->data[0], in->linesize[0], inlink->w, inlink->h);
    for (plane = 0; plane < 3; plane++)
        filter_plane(ctx, fft->cbuf[1][plane], fft->p_linesize,
                          fft->cbuf[0][plane], fft->p_linesize,
                          inlink->w, inlink->h);
    color_correlation(out->data[0], out->linesize[0], fft->cbuf[1], fft->p_linesize, inlink->w, inlink->h);

    if (!direct)
        av_frame_free(&in);

    return ff_filter_frame(outlink, out);
}

static av_cold void uninit(AVFilterContext *ctx)
{
    int i;
    FFTFilterContext *fft = ctx->priv;

    av_dct_end(fft->dct);
    av_dct_end(fft->idct);
    av_free(fft->block);
    av_free(fft->tmp_block);
    av_free(fft->weights);
    for (i = 0; i < 2; i++) {
        av_free(fft->cbuf[i][0]);
        av_free(fft->cbuf[i][1]);
        av_free(fft->cbuf[i][2]);
    }
#if EXPR
    av_expr_free(fft->expr);
#endif
#if WINDOWING
    av_free(fft->win_fn);
#endif
}

static const AVFilterPad fft_inputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_VIDEO,
        .filter_frame = filter_frame,
        .config_props = config_input,
     },
     { NULL }
};

static const AVFilterPad fft_outputs[] = {
     {
         .name = "default",
         .type = AVMEDIA_TYPE_VIDEO,
     },
     { NULL }
};

AVFilter avfilter_vf_fft = {
    .name          = "fft",
    .description   = NULL_IF_CONFIG_SMALL("FIXME"),
    .priv_size     = sizeof(FFTFilterContext),
    .init          = init,
    .uninit        = uninit,
    .query_formats = query_formats,
    .inputs        = fft_inputs,
    .outputs       = fft_outputs,
    .priv_class    = &fft_class,
    .flags         = AVFILTER_FLAG_SUPPORT_TIMELINE,
};

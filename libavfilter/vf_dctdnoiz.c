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

/**
 * A simple, relatively efficient and extremely slow DCT image denoiser.
 * @see http://www.ipol.im/pub/art/2011/ys-dct/
 */

#include "libavutil/eval.h"
#include "libavutil/opt.h"
#include "drawutils.h"
#include "internal.h"

#define NBITS 3
#define BSIZE (1<<(NBITS))

#define SCALE_BITS 12
#define FIX(x) lrint((x) * (1<<SCALE_BITS))
#define DO_SCALE(x) (((x) + (1<<(SCALE_BITS-1))) >> SCALE_BITS)

static const char *const var_names[] = { "c", NULL };
enum { VAR_C, VAR_VARS_NB };

typedef struct DCTdnoizContext {
    const AVClass *class;

    /* coefficient factor expression */
    char *expr_str;
    AVExpr *expr;
    double var_values[VAR_VARS_NB];

    float dct_coeffs[BSIZE*BSIZE];
    int pr_width, pr_height;    // width and height to process
    float sigma;                // used when no expression are st
    int th;                     // threshold (3*sigma, rounded)
    int color_dct[3][3];        // 3x3 DCT for color decorrelation
    int16_t *cbuf16[3];         // 16-bit rgb color buffer
    int32_t *cbuf32[3];         // 32-bit rgb color buffer
    int *weights;               // dct coeff are cumulated with overlapping; these values are used for averaging
    int p_linesize;             // line sizes for color and weights
    int overlap;                // number of block overlapping pixels
    int step;                   // block step increment (BSIZE - overlap)
    void (*filter_freq_func)(struct DCTdnoizContext *s,
                             const int16_t *src, int src_linesize,
                             int32_t *dst, int dst_linesize);
} DCTdnoizContext;

#define OFFSET(x) offsetof(DCTdnoizContext, x)
#define FLAGS AV_OPT_FLAG_FILTERING_PARAM|AV_OPT_FLAG_VIDEO_PARAM
static const AVOption dctdnoiz_options[] = {
    { "sigma",   "set noise sigma constant",               OFFSET(sigma),    AV_OPT_TYPE_FLOAT,  {.dbl=0},            0, 999,          .flags = FLAGS },
    { "s",       "set noise sigma constant",               OFFSET(sigma),    AV_OPT_TYPE_FLOAT,  {.dbl=0},            0, 999,          .flags = FLAGS },
    { "overlap", "set number of block overlapping pixels", OFFSET(overlap),  AV_OPT_TYPE_INT,    {.i64=(1<<NBITS)-1}, 0, (1<<NBITS)-1, .flags = FLAGS },
    { "expr",    "set coefficient factor expression",      OFFSET(expr_str), AV_OPT_TYPE_STRING, {.str=NULL},                          .flags = FLAGS },
    { "e",       "set coefficient factor expression",      OFFSET(expr_str), AV_OPT_TYPE_STRING, {.str=NULL},                          .flags = FLAGS },
    { NULL }
};

AVFILTER_DEFINE_CLASS(dctdnoiz);

static av_always_inline void filter_freq(const float *dct_coeffs,
                                         const int16_t *src, int src_linesize,
                                         int32_t *dst, int dst_linesize,
                                         AVExpr *expr, double *var_values,
                                         int sigma_th)
{
    unsigned i, j, k;
    DECLARE_ALIGNED(32, int16_t, block)[BSIZE * BSIZE];
    DECLARE_ALIGNED(32, float, tmp_block)[BSIZE * BSIZE];

    /* forward DCT */

    for (i = 0; i < BSIZE*BSIZE; i += BSIZE) {
        for (j = 0; j < BSIZE; j++) {
            float tmp = 0;
            for (k = 0; k < BSIZE; k++)
                tmp += dct_coeffs[i + k] * src[k * src_linesize + j];
            tmp_block[i + j] = tmp * BSIZE;
        }
    }

    for (j = 0; j < BSIZE; j++) {
        for (i = 0; i < BSIZE*BSIZE; i += BSIZE) {
            float tmp = 0;
            int16_t *b = &block[i + j];
            for (k = 0; k < BSIZE; k++)
                tmp += tmp_block[i + k] * dct_coeffs[j * BSIZE + k];
            *b = lrint(tmp / BSIZE);

            /* frequency filtering */
            if (expr) {
                var_values[VAR_C] = FFABS(*b);
                *b *= av_expr_eval(expr, var_values, NULL);
            } else {
                if (FFABS(*b) < sigma_th)
                    *b = 0;
            }
        }
    }

    /* inverse DCT */

    for (i = 0; i < BSIZE*BSIZE; i += BSIZE) {
        for (j = 0; j < BSIZE; j++) {
            float tmp = 0;
            for (k = 0; k < BSIZE; k++)
                tmp += block[i + k] * dct_coeffs[k * BSIZE + j];
            tmp_block[i + j] = tmp;
        }
    }

    for (i = 0; i < BSIZE; i++) {
        for (j = 0; j < BSIZE; j++) {
            float tmp = 0;
            for (k = 0; k < BSIZE*BSIZE; k += BSIZE)
                tmp += dct_coeffs[k + i] * tmp_block[k + j];
            dst[i * dst_linesize + j] += lrint(tmp);
        }
    }
}

static void filter_freq_sigma(DCTdnoizContext *s,
                              const int16_t *src, int src_linesize,
                              int32_t *dst, int dst_linesize)
{
    filter_freq(s->dct_coeffs, src, src_linesize, dst, dst_linesize, NULL, NULL, s->th);
}

static void filter_freq_expr(DCTdnoizContext *s,
                             const int16_t *src, int src_linesize,
                             int32_t *dst, int dst_linesize)
{
    filter_freq(s->dct_coeffs, src, src_linesize, dst, dst_linesize, s->expr, s->var_values, 0);
}

static int config_input(AVFilterLink *inlink)
{
    AVFilterContext *ctx = inlink->dst;
    DCTdnoizContext *s = ctx->priv;
    int x, y, bx, by, linesize, *iweights;
    const float dct_3x3[3][3] = {
        { 1./sqrt(3),  1./sqrt(3),  1./sqrt(3) },
        { 1./sqrt(2),           0, -1./sqrt(2) },
        { 1./sqrt(6), -2./sqrt(6),  1./sqrt(6) },
    };
    uint8_t rgba_map[4];

    ff_fill_rgba_map(rgba_map, inlink->format);
    for (y = 0; y < 3; y++)
        for (x = 0; x < 3; x++)
            s->color_dct[y][x] = FIX(dct_3x3[rgba_map[y]][rgba_map[x]]);

    s->pr_width  = inlink->w - (inlink->w - BSIZE) % s->step;
    s->pr_height = inlink->h - (inlink->h - BSIZE) % s->step;
    if (s->pr_width != inlink->w)
        av_log(ctx, AV_LOG_WARNING, "The last %d horizontal pixels won't be denoised\n",
               inlink->w - s->pr_width);
    if (s->pr_height != inlink->h)
        av_log(ctx, AV_LOG_WARNING, "The last %d vertical pixels won't be denoised\n",
               inlink->h - s->pr_height);

    s->p_linesize = linesize = FFALIGN(s->pr_width, 32);

    s->cbuf16[0] = av_malloc(linesize * s->pr_height * sizeof(*s->cbuf16[0]));
    s->cbuf16[1] = av_malloc(linesize * s->pr_height * sizeof(*s->cbuf16[1]));
    s->cbuf16[2] = av_malloc(linesize * s->pr_height * sizeof(*s->cbuf16[2]));
    if (!s->cbuf16[0] || !s->cbuf16[1] || !s->cbuf16[2])
        return AVERROR(ENOMEM);

    s->cbuf32[0] = av_malloc(linesize * s->pr_height * sizeof(*s->cbuf32[0]));
    s->cbuf32[1] = av_malloc(linesize * s->pr_height * sizeof(*s->cbuf32[1]));
    s->cbuf32[2] = av_malloc(linesize * s->pr_height * sizeof(*s->cbuf32[2]));
    if (!s->cbuf32[0] || !s->cbuf32[1] || !s->cbuf32[2])
        return AVERROR(ENOMEM);

    s->weights = av_malloc(s->pr_height * linesize * sizeof(*s->weights));
    if (!s->weights)
        return AVERROR(ENOMEM);
    iweights = av_calloc(s->pr_height, linesize * sizeof(*iweights));
    if (!iweights)
        return AVERROR(ENOMEM);
    for (y = 0; y < s->pr_height - BSIZE + 1; y += s->step)
        for (x = 0; x < s->pr_width - BSIZE + 1; x += s->step)
            for (by = 0; by < BSIZE; by++)
                for (bx = 0; bx < BSIZE; bx++)
                    iweights[(y + by)*linesize + x + bx]++;
    for (y = 0; y < s->pr_height; y++)
        for (x = 0; x < s->pr_width; x++)
            s->weights[y*linesize + x] = FIX(1. / iweights[y*linesize + x]);
    av_free(iweights);

    return 0;
}

static av_cold int init(AVFilterContext *ctx)
{
    unsigned i, j;
    DCTdnoizContext *s = ctx->priv;

    if (s->expr_str) {
        int ret = av_expr_parse(&s->expr, s->expr_str, var_names,
                                NULL, NULL, NULL, NULL, 0, ctx);
        if (ret < 0)
            return ret;
        s->filter_freq_func = filter_freq_expr;
    } else {
        s->filter_freq_func = filter_freq_sigma;
    }

    s->th   = lrint(s->sigma * 3.);
    s->step = BSIZE - s->overlap;

    for (j = 0; j < BSIZE; j++) {
        s->dct_coeffs[j] = sqrt(1. / BSIZE);
        for (i = BSIZE; i < BSIZE*BSIZE; i += BSIZE)
            s->dct_coeffs[i + j] = sqrt(2./BSIZE) * cos(i * (j + 0.5) * M_PI / (float)(BSIZE*BSIZE));
    }

    return 0;
}

static int query_formats(AVFilterContext *ctx)
{
    static const enum AVPixelFormat pix_fmts[] = {
        AV_PIX_FMT_BGR24, AV_PIX_FMT_RGB24,
        AV_PIX_FMT_NONE
    };
    ff_set_common_formats(ctx, ff_make_format_list(pix_fmts));
    return 0;
}

static void color_decorrelation(int dct3ch[3][3], int16_t **dst, int dst_linesize,
                                const uint8_t *src, int src_linesize, int w, int h)
{
    int x, y;
    int16_t *dstp_r = dst[0];
    int16_t *dstp_g = dst[1];
    int16_t *dstp_b = dst[2];

    for (y = 0; y < h; y++) {
        const uint8_t *srcp = src;

        for (x = 0; x < w; x++) {
            dstp_r[x] = DO_SCALE(srcp[0] * dct3ch[0][0] + srcp[1] * dct3ch[0][1] + srcp[2] * dct3ch[0][2]);
            dstp_g[x] = DO_SCALE(srcp[0] * dct3ch[1][0] + srcp[1] * dct3ch[1][1] + srcp[2] * dct3ch[1][2]);
            dstp_b[x] = DO_SCALE(srcp[0] * dct3ch[2][0] + srcp[1] * dct3ch[2][1] + srcp[2] * dct3ch[2][2]);
            srcp += 3;
        }
        src += src_linesize;
        dstp_r += dst_linesize;
        dstp_g += dst_linesize;
        dstp_b += dst_linesize;
    }
}

static void color_correlation(int dct3ch[3][3], uint8_t *dst, int dst_linesize,
                              int16_t **src, int src_linesize, int w, int h)
{
    int x, y;
    const int16_t *src_r = src[0];
    const int16_t *src_g = src[1];
    const int16_t *src_b = src[2];

    for (y = 0; y < h; y++) {
        uint8_t *dstp = dst;

        for (x = 0; x < w; x++) {
            dstp[0] = av_clip_uint8(DO_SCALE(src_r[x] * dct3ch[0][0] + src_g[x] * dct3ch[1][0] + src_b[x] * dct3ch[2][0]));
            dstp[1] = av_clip_uint8(DO_SCALE(src_r[x] * dct3ch[0][1] + src_g[x] * dct3ch[1][1] + src_b[x] * dct3ch[2][1]));
            dstp[2] = av_clip_uint8(DO_SCALE(src_r[x] * dct3ch[0][2] + src_g[x] * dct3ch[1][2] + src_b[x] * dct3ch[2][2]));
            dstp += 3;
        }
        dst += dst_linesize;
        src_r += src_linesize;
        src_g += src_linesize;
        src_b += src_linesize;
    }
}

static void filter_plane(AVFilterContext *ctx,
                         int16_t *data, int data_linesize,
                         int32_t *tmp,  int tmp_linesize,
                         int w, int h)
{
    int x, y;
    DCTdnoizContext *s = ctx->priv;
    const int *weights = s->weights;
    int32_t *tmp0 = tmp;
    int16_t *data0 = data;

    // reset block sums
    memset(tmp, 0, h * tmp_linesize * sizeof(*tmp));

    // block dct sums
    for (y = 0; y < h - BSIZE + 1; y += s->step) {
        for (x = 0; x < w - BSIZE + 1; x += s->step)
            s->filter_freq_func(s, data + x, data_linesize,
                                    tmp + x,  tmp_linesize);
        data += s->step * data_linesize;
        tmp  += s->step *  tmp_linesize;
    }

    // average blocks
    tmp  = tmp0;
    data = data0;
    for (y = 0; y < h; y++) {
        for (x = 0; x < w; x++)
            data[x] = DO_SCALE(tmp[x] * weights[x]);
        data    += data_linesize;
        tmp     +=  tmp_linesize;
        weights += data_linesize;
    }
}

static int filter_frame(AVFilterLink *inlink, AVFrame *in)
{
    AVFilterContext *ctx = inlink->dst;
    DCTdnoizContext *s = ctx->priv;
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

    color_decorrelation(s->color_dct, s->cbuf16, s->p_linesize,
                        in->data[0], in->linesize[0], s->pr_width, s->pr_height);
    for (plane = 0; plane < 3; plane++)
        filter_plane(ctx, s->cbuf16[plane], s->p_linesize,
                          s->cbuf32[plane], s->p_linesize,
                          s->pr_width, s->pr_height);
    color_correlation(s->color_dct, out->data[0], out->linesize[0],
                      s->cbuf16, s->p_linesize, s->pr_width, s->pr_height);

    if (!direct) {
        int y;
        uint8_t *dst = out->data[0];
        const uint8_t *src = in->data[0];
        const int dst_linesize = out->linesize[0];
        const int src_linesize = in->linesize[0];
        const int hpad = (inlink->w - s->pr_width) * 3;
        const int vpad = (inlink->h - s->pr_height);

        if (hpad) {
            uint8_t       *dstp = dst + s->pr_width * 3;
            const uint8_t *srcp = src + s->pr_width * 3;

            for (y = 0; y < s->pr_height; y++) {
                memcpy(dstp, srcp, hpad);
                dstp += dst_linesize;
                srcp += src_linesize;
            }
        }
        if (vpad) {
            uint8_t       *dstp = dst + s->pr_height * dst_linesize;
            const uint8_t *srcp = src + s->pr_height * src_linesize;

            for (y = 0; y < vpad; y++) {
                memcpy(dstp, srcp, inlink->w * 3);
                dstp += dst_linesize;
                srcp += src_linesize;
            }
        }

        av_frame_free(&in);
    }

    return ff_filter_frame(outlink, out);
}

static av_cold void uninit(AVFilterContext *ctx)
{
    DCTdnoizContext *s = ctx->priv;

    av_free(s->weights);
    av_free(s->cbuf16[0]);
    av_free(s->cbuf16[1]);
    av_free(s->cbuf16[2]);
    av_free(s->cbuf32[0]);
    av_free(s->cbuf32[1]);
    av_free(s->cbuf32[2]);
    av_expr_free(s->expr);
}

static const AVFilterPad dctdnoiz_inputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_VIDEO,
        .filter_frame = filter_frame,
        .config_props = config_input,
    },
    { NULL }
};

static const AVFilterPad dctdnoiz_outputs[] = {
    {
        .name = "default",
        .type = AVMEDIA_TYPE_VIDEO,
    },
    { NULL }
};

AVFilter ff_vf_dctdnoiz = {
    .name          = "dctdnoiz",
    .description   = NULL_IF_CONFIG_SMALL("Denoise frames using 2D DCT."),
    .priv_size     = sizeof(DCTdnoizContext),
    .init          = init,
    .uninit        = uninit,
    .query_formats = query_formats,
    .inputs        = dctdnoiz_inputs,
    .outputs       = dctdnoiz_outputs,
    .priv_class    = &dctdnoiz_class,
    .flags         = AVFILTER_FLAG_SUPPORT_TIMELINE_GENERIC,
};

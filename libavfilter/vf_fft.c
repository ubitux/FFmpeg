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
#include "fft2d.h"

static const char *const var_names[] = {
    "x", "y",
    "w", "h",
    "n", "t",
    "re", "im",
    "psd", // power spectral density, re*re + im*im
    NULL
};

enum {
    VAR_X, VAR_Y,
    VAR_W, VAR_H,
    VAR_N, VAR_T,
    VAR_RE, VAR_IM,
    VAR_PSD,
    VAR_VARS_NB
};

typedef struct {
    const AVClass *class;
    char *expr_str;
    AVExpr *expr;
    double var_values[VAR_VARS_NB];
    int nxbits, nybits;
    int blockw, blockh;
    void *blockfft;

    float *cbuf[2][3];
    int p_linesize;
    float *weights;

} FFTFilterContext;

#define OFFSET(x) offsetof(FFTFilterContext, x)
#define FLAGS AV_OPT_FLAG_FILTERING_PARAM|AV_OPT_FLAG_VIDEO_PARAM
static const AVOption fft_options[] = {
    { "e", NULL, OFFSET(expr_str), AV_OPT_TYPE_STRING, {.str=NULL}, .flags = FLAGS },
    { NULL },
};

AVFILTER_DEFINE_CLASS(fft);

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
            t[y*w + x] = HANN(x, w) * HANN(y, h) * scale;
    return t;
}

static int config_input(AVFilterLink *inlink)
{
    //int ret;
    //AVFrame *cframe;
    FFTFilterContext *fft = inlink->dst->priv;
    const int linesize = FFALIGN(inlink->w, 16);
    int i, x, y, bx, by, *iweights;

    //cframe = av_frame_alloc();
    //if (!cframe)
    //    return AVERROR(ENOMEM);
    //cframe->width  = inlink->w;
    //cframe->height = inlink->h;
    //cframe->format = AV_PIX_FMT_GBRP16,
    //ret = av_frame_get_buffer(cframe, 32);
    //if (ret < 0) {
    //    av_frame_free(&cframe);
    //    return ret;
    //}
    //fft->cframe = cframe;

    fft->p_linesize = linesize;
    for (i = 0; i < 2; i++) {
        fft->cbuf[i][0] = av_malloc(linesize * inlink->h * sizeof(*fft->cbuf[i][0]));
        fft->cbuf[i][1] = av_malloc(linesize * inlink->h * sizeof(*fft->cbuf[i][1]));
        fft->cbuf[i][2] = av_malloc(linesize * inlink->h * sizeof(*fft->cbuf[i][2]));
        if (!fft->cbuf[i][0] || !fft->cbuf[i][1] || !fft->cbuf[i][2])
            return AVERROR(ENOMEM);
    }

    fft->weights = av_calloc(inlink->h, linesize * sizeof(*fft->weights));
    if (!fft->weights)
        return AVERROR(ENOMEM);
    iweights = av_calloc(inlink->h, linesize * sizeof(*iweights));
    if (!iweights)
        return AVERROR(ENOMEM);
    for (y = 0; y < inlink->h - fft->blockh + 1; y++)
        for (x = 0; x < inlink->w - fft->blockw + 1; x++)
            for (by = 0; by < fft->blockh; by++)
                for (bx = 0; bx < fft->blockw; bx++)
                    iweights[(y + by)*linesize + x + bx]++;
    for (y = 0; y < inlink->h - fft->blockh + 1; y++)
        for (x = 0; x < inlink->w - fft->blockw + 1; x++)
            fft->weights[y*linesize + x] = 1. / iweights[y*linesize + x];
    av_free(iweights);

    return 0;
}

static av_cold int init(AVFilterContext *ctx)
{
    int ret;
    FFTFilterContext *fft = ctx->priv;
    float *win_fn;


    if (!fft->expr_str) {
        av_log(ctx, AV_LOG_ERROR, "expression is mandatory\n");
        return AVERROR(EINVAL);
    }

    ret = av_expr_parse(&fft->expr, fft->expr_str, var_names,
                        NULL, NULL, NULL, NULL, 0, ctx);
    if (ret < 0)
        return ret;

    // FIXME: make configurable
    fft->nxbits = fft->nybits = 4;

    fft->blockh = 1 << fft->nxbits;
    fft->blockw = 1 << fft->nybits;

    win_fn = get_window_function(fft->blockw, fft->blockh);
    if (!win_fn)
        return AVERROR(ENOMEM);

    fft->blockfft = ff_fft2d_init(fft->nxbits, fft->nybits, NULL /*win_fn*/);
    if (!fft->blockfft)
        return AVERROR(ENOMEM);

    return 0;
}

static int query_formats(AVFilterContext *ctx)
{
    // XXX: add more
    static const enum AVPixelFormat pix_fmts[] = {
        AV_PIX_FMT_GBR24P,
        AV_PIX_FMT_NONE
    };
    ff_set_common_formats(ctx, ff_make_format_list(pix_fmts));
    return 0;
}

// FIXME: fixed point
//static const float dct3ch[3][3] = {
//    {0.5774,  0.5774,  0.5774}, // R
//    {0.7071,       0, -0.7071}, // G
//    {0.4082, -0.8165,  0.4082}, // B
//};

const float dct3ch[3][3] = {
    { 0.577350258827209472656250,  0.57735025882720947265625,  0.577350258827209472656250 },
    { 0.707106769084930419921875,  0.00000000000000000000000, -0.707106769084930419921875 },
    { 0.408248305320739746093750, -0.81649661064147949218750,  0.408248305320739746093750 },
};

//static int color_map[] = {0, 1, 2};

// FIXME: simplify
static int color_map[] = {1, 2, 0};
                       // G  B  R

static void color_decorrelation(float   **dst, int dst_linesize,
                                uint8_t **src, int *src_linesize, int w, int h)
{
    int x, y, p;

    for (p = 0; p < 3; p++) {
        for (y = 0; y < h; y++) {
            for (x = 0; x < w; x++) {
                dst[p][y*dst_linesize + x] =
                                    src[0][y*src_linesize[0] + x] * dct3ch[color_map[p]][color_map[0]]
                                  + src[1][y*src_linesize[1] + x] * dct3ch[color_map[p]][color_map[1]]
                                  + src[2][y*src_linesize[2] + x] * dct3ch[color_map[p]][color_map[2]];
            }
        }
    }
}

static void color_correlation(uint8_t **dst, int *dst_linesize,
                              float   **src, int src_linesize, int w, int h)
{
    int x, y, p;

    for (p = 0; p < 3; p++) {
        for (y = 0; y < h; y++) {
            for (x = 0; x < w; x++) {
                dst[p][y*dst_linesize[p] + x] =
                    av_clip_uint8(  src[0][y*src_linesize + x] * dct3ch[color_map[0]][color_map[p]]
                                  + src[1][y*src_linesize + x] * dct3ch[color_map[1]][color_map[p]]
                                  + src[2][y*src_linesize + x] * dct3ch[color_map[2]][color_map[p]]);
            }
        }
    }
}

static void filter_plane(AVFilterContext *ctx,
                         float *dst, int dst_linesize,
                         const float *src, int src_linesize,
                         int w, int h)
{
    int x, y, bx, by;
    FFTFilterContext *fft = ctx->priv;
    const float *srcp = src;
    float *dstp = dst;
    const float *iweights = fft->weights;

    memset(dstp, 0, h*dst_linesize);

    for (y = 0; y < h - fft->blockh + 1; y++) {
        for (x = 0; x < w - fft->blockw + 1; x++) {
            FFTComplex *cplx =
                ff_fft2d_block(fft->blockfft, srcp + x, src_linesize);

            //av_log(0,0,"filter block at (%d,%d) of size (%d/2+1,%d)\n",
            //       x, y, fft->blockw, fft->blockh);

            av_log(0,0,"INPUT:\n");
            for (by = 0; by < fft->blockh; by++) {
                for (bx = 0; bx < fft->blockw; bx++) {
                    av_log(0,0," %10g", srcp[x + by*fft->blockw + bx]);
                }
                av_log(0,0,"\n");
            }
            av_log(0,0,"\n");

            av_log(0,0,"OUTPUT:\n");
            for (by = 0; by < fft->blockh; by++) {
                fft->var_values[VAR_Y] = by / (fft->blockh - 1);

                for (bx = 0; bx < fft->blockw/2 + 1; bx++) {
                    double f;

                    av_log(0,0," %10g/%10g", cplx->re/8., cplx->im/8.);
                    fft->var_values[VAR_X] = bx / (fft->blockw/2);

                    fft->var_values[VAR_RE]  = cplx->re;
                    fft->var_values[VAR_IM]  = cplx->im;
                    fft->var_values[VAR_PSD] = cplx->re*cplx->re + cplx->im*cplx->im + 1e-16;

                    f = av_expr_eval(fft->expr, fft->var_values, fft);

                    //av_log(0,0,"psd=%f -> f=%f\n", fft->var_values[VAR_PSD], f);

                    //cplx->re *= f;
                    //cplx->im *= f;
                    cplx->re = FFABS(cplx->re / (fft->blockw )) > f ? cplx->re : 0;
                    cplx->im = FFABS(cplx->im / (fft->blockw )) > f ? cplx->im : 0;

                    cplx++;
                }
                av_log(0,0,"\n");
            }
            av_log(0,0,"\n");

            ff_fft2d_block_inv(fft->blockfft, dstp + x, dst_linesize);
        }
        srcp += src_linesize;
        dstp += dst_linesize;
    }

    dstp = dst;
    for (y = 0; y < h; y++) {
        for (x = 0; x < w; x++)
            dstp[x] *= iweights[x];
        dstp += dst_linesize;
        iweights += dst_linesize;
    }
}

static const int order_map[] = {2, 0, 1};

static int filter_frame(AVFilterLink *inlink, AVFrame *in)
{
    AVFilterContext *ctx = inlink->dst;
    FFTFilterContext *fft = ctx->priv;
    AVFilterLink *outlink = inlink->dst->outputs[0];
    int plane;
    //AVFrame *out;

    //out = fft->cframe;
    //out = ff_get_video_buffer(outlink, outlink->w, outlink->h);
    //if (!out) {
    //    av_frame_free(&in);
    //    return AVERROR(ENOMEM);
    //}
    //av_frame_copy_props(out, in);

    fft->var_values[VAR_N] = inlink->frame_count,
    fft->var_values[VAR_T] = in->pts == AV_NOPTS_VALUE ? NAN : in->pts * av_q2d(inlink->time_base),
    fft->var_values[VAR_W] = inlink->w;
    fft->var_values[VAR_H] = inlink->h;


    color_decorrelation(fft->cbuf[0], fft->p_linesize, in->data, in->linesize, inlink->w, inlink->h);
    for (plane = 0; plane < 3; plane++)
        filter_plane(ctx, fft->cbuf[1][order_map[plane]], fft->p_linesize,
                          fft->cbuf[0][order_map[plane]], fft->p_linesize,
                          inlink->w, inlink->h);
    color_correlation(in->data, in->linesize, fft->cbuf[1], fft->p_linesize, inlink->w, inlink->h);

    //av_frame_free(&in);
    //av_frame_free(&out);

    return ff_filter_frame(outlink, in);
    //return ff_filter_frame(outlink, out);
}

static av_cold void uninit(AVFilterContext *ctx)
{
    FFTFilterContext *fft = ctx->priv;
    ff_fft2d_end(fft->blockfft);
}

static const AVFilterPad fft_inputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_VIDEO,
        .filter_frame = filter_frame,
        .config_props = config_input,
        .needs_writable = 1,
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

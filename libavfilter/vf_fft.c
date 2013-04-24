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
} FFTFilterContext;

#define OFFSET(x) offsetof(FFTFilterContext, x)
#define FLAGS AV_OPT_FLAG_FILTERING_PARAM|AV_OPT_FLAG_VIDEO_PARAM
static const AVOption fft_options[] = {
    { "e", NULL, OFFSET(expr_str), AV_OPT_TYPE_STRING, {.str=NULL}, .flags = FLAGS },
    { NULL },
};

AVFILTER_DEFINE_CLASS(fft);

static av_cold int init(AVFilterContext *ctx)
{
    int ret;
    FFTFilterContext *fft = ctx->priv;

    if (!fft->expr_str) {
        av_log(ctx, AV_LOG_ERROR, "expression is mandatory\n");
        return AVERROR(EINVAL);
    }

    ret = av_expr_parse(&fft->expr, fft->expr_str, var_names,
                        NULL, NULL, NULL, NULL, 0, ctx);
    if (ret < 0)
        return ret;

    // FIXME: make configurable
    fft->nxbits = fft->nybits = 6;

    fft->blockh = 1 << fft->nxbits;
    fft->blockw = 1 << fft->nybits;
    fft->blockfft = ff_fft2d_init(fft->nxbits, fft->nybits);
    if (!fft->blockfft)
        return AVERROR(ENOMEM);

    return 0;
}

static int query_formats(AVFilterContext *ctx)
{
    // XXX: add some rgb & yuv
    static const enum AVPixelFormat pix_fmts[] = {AV_PIX_FMT_GRAY8, AV_PIX_FMT_NONE};
    ff_set_common_formats(ctx, ff_make_format_list(pix_fmts));
    return 0;
}

static void filter_plane(AVFilterContext *ctx, uint8_t *data,
                         int linesize, int w, int h)
{
    int x, y, bx, by;
    FFTFilterContext *fft = ctx->priv;

    // FIXME do borders
    for (y = 0; y < h - fft->blockh + 1; y += fft->blockh) {
        for (x = 0; x < w - fft->blockw + 1; x += fft->blockw) {
            FFTComplex *cplx =
                ff_fft2d_block(fft->blockfft, data + x, linesize);

            //av_log(0,0,"filter block at (%d,%d) of size (%d/2+1,%d)\n",
            //       x, y, fft->blockw, fft->blockh);
            for (by = 0; by < fft->blockh; by++) {
                fft->var_values[VAR_Y] = by / (fft->blockh - 1);

                for (bx = 0; bx < fft->blockw/2 + 1; bx++) {
                    double f;

                    fft->var_values[VAR_X] = bx / (fft->blockw/2);

                    fft->var_values[VAR_RE]  = cplx->re;
                    fft->var_values[VAR_IM]  = cplx->im;
                    fft->var_values[VAR_PSD] = hypot(cplx->re, cplx->im) + 1e-16;

                    f = av_expr_eval(fft->expr, fft->var_values, fft);

                    cplx->re *= f;
                    cplx->im *= f;

                    cplx++;
                }
            }

            ff_fft2d_block_inv(fft->blockfft, data + x, linesize);
        }
        data += linesize * fft->blockh;
    }
}

static int filter_frame(AVFilterLink *inlink, AVFrame *in)
{
    AVFilterContext *ctx = inlink->dst;
    FFTFilterContext *fft = ctx->priv;
    AVFilterLink *outlink = inlink->dst->outputs[0];

    fft->var_values[VAR_N] = inlink->frame_count,
    fft->var_values[VAR_T] = in->pts == AV_NOPTS_VALUE ? NAN : in->pts * av_q2d(inlink->time_base),
    fft->var_values[VAR_W] = inlink->w;
    fft->var_values[VAR_H] = inlink->h;

    filter_plane(ctx, in->data[0], in->linesize[0], inlink->w, inlink->h);

    return ff_filter_frame(outlink, in);
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

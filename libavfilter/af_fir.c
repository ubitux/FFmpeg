/*
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include "libavutil/eval.h"
#include "libavutil/opt.h"
#include "avfilter.h"
#include "audio.h"

typedef struct {
    const AVClass *class;
    char *h_str;
    double *htab;
    int hlen;
} FIRContext;

#define OFFSET(x) offsetof(FIRContext, x)
#define FLAGS AV_OPT_FLAG_AUDIO_PARAM|AV_OPT_FLAG_FILTERING_PARAM

static const AVOption fir_options[] = {
    { "h", "", OFFSET(h_str), AV_OPT_TYPE_STRING, {.str="1"}, CHAR_MIN, CHAR_MAX, FLAGS },
    { NULL }
};

AVFILTER_DEFINE_CLASS(fir);

static av_cold void uninit(AVFilterContext *ctx)
{
    FIRContext *fir = ctx->priv;
    av_freep(&fir->htab);
}

static int filter_frame(AVFilterLink *inlink, AVFrame *in)
{
    int ch;
    AVFilterContext *ctx = inlink->dst;
    FIRContext *fir = ctx->priv;
    AVFrame *out;

    out = ff_get_audio_buffer(ctx->outputs[0], in->nb_samples);
    if (!out)
        return AVERROR(ENOMEM);

    for (ch = 0; ch < inlink->channels; ch++) {
        int i;
        double *dst = (double*)out->extended_data[ch];
        const double *src = (const double*)in->extended_data[ch];

        for (i = 0; i < out->nb_samples; i++) {
            int k;
            double sum = 0;

            for (k = 0; k < fir->hlen; k++)
                // FIXME: must do the FFMAX() only for the first frame
                sum += src[FFMAX(0, i-k)] * fir->htab[k];
            dst[i] = sum;
        }
    }

    av_frame_free(&in);
    return ff_filter_frame(ctx->outputs[0], out);
}

static av_cold int init(AVFilterContext *ctx)
{
    FIRContext *fir = ctx->priv;
    char *p = fir->h_str;

    while (*p) {
        const double d = av_strtod(p, &p);
        void *tmp = av_realloc_array(fir->htab, fir->hlen + 1, sizeof(*fir->htab));
        if (!tmp)
            return AVERROR(ENOMEM);
        fir->htab = tmp;
        fir->htab[fir->hlen++] = d;
        if (*p)
            p++;
    }

    if (!fir->hlen) {
        av_log(ctx, AV_LOG_ERROR, "The FIR filter can not be empty\n");
        return AVERROR(EINVAL);
    }

    return 0;
}

static int query_formats(AVFilterContext *ctx)
{
    AVFilterChannelLayouts *layouts;
    AVFilterFormats *formats;
    // TODO: support more
    static const enum AVSampleFormat sample_fmts[] = {
        AV_SAMPLE_FMT_DBLP,
        //AV_SAMPLE_FMT_S16P, AV_SAMPLE_FMT_S32P,
        //AV_SAMPLE_FMT_FLTP, AV_SAMPLE_FMT_DBLP,
        AV_SAMPLE_FMT_NONE
    };

    layouts = ff_all_channel_layouts();
    if (!layouts)
        return AVERROR(ENOMEM);
    ff_set_common_channel_layouts(ctx, layouts);

    formats = ff_make_format_list(sample_fmts);
    if (!formats)
        return AVERROR(ENOMEM);
    ff_set_common_formats(ctx, formats);

    formats = ff_all_samplerates();
    if (!formats)
        return AVERROR(ENOMEM);
    ff_set_common_samplerates(ctx, formats);

    return 0;
}

static const AVFilterPad fir_inputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_AUDIO,
        .filter_frame = filter_frame,
    },
    { NULL }
};

static const AVFilterPad fir_outputs[] = {
    {
        .name          = "default",
        .type          = AVMEDIA_TYPE_AUDIO,
    },
    { NULL }
};

AVFilter ff_af_fir = {
    .name          = "fir",
    .description   = NULL_IF_CONFIG_SMALL("Apply a Finite Impulse Response filter"),
    .priv_size     = sizeof(FIRContext),
    .init          = init,
    .uninit        = uninit,
    .query_formats = query_formats,
    .inputs        = fir_inputs,
    .outputs       = fir_outputs,
    .priv_class    = &fir_class,
};

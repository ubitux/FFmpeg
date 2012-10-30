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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/**
 * @file
 * XXX: mention based on smartblur
 */

#include "libavutil/pixdesc.h"
#include "libswscale/swscale.h"
#include "avfilter.h"
#include "formats.h"
#include "internal.h"

typedef struct {
    struct SwsContext *filter_context;
} FilterParam;

typedef struct {
    int filter_ref;
    FilterParam  luma;
    FilterParam  chroma;
    int          hsub;
    int          vsub;
    unsigned int sws_flags;
} EnhanceContext;

/* XXX: mention matrix are taken from python Imaging */
static const int blur[] = {
    1,  1,  1,  1,  1,
    1,  0,  0,  0,  1,
    1,  0,  0,  0,  1,
    1,  0,  0,  0,  1,
    1,  1,  1,  1,  1,
};
static const int contour[] = {
    -1, -1, -1,
    -1,  8, -1,
    -1, -1, -1,
};
static const int detail[] = {
     0, -1,  0,
    -1, 10, -1,
     0, -1,  0,
};
static const int edge_enhance[] = {
    -1, -1, -1,
    -1, 10, -1,
    -1, -1, -1,
};
static const int edge_enhance_more[] = {
    -1, -1, -1,
    -1,  9, -1,
    -1, -1, -1,
};
static const int emboss[] = {
    -1,  0,  0,
     0,  1,  0,
     0,  0,  0,
};
static const int find_edges[] = {
    -1, -1, -1,
    -1,  8, -1,
    -1, -1, -1,
};
static const int smooth[] = {
    1,  1,  1,
    1,  5,  1,
    1,  1,  1,
};
static const int smooth_more[] = {
    1,  1,  1,  1,  1,
    1,  5,  5,  5,  1,
    1,  5, 44,  5,  1,
    1,  5,  5,  5,  1,
    1,  1,  1,  1,  1,
};
static const int sharpen[] = {
    -2, -2, -2,
    -2, 32, -2,
    -2, -2, -2,
};

#define REF_FILTER(f) {          \
    .name   = AV_STRINGIFY(f),   \
    .coeff  = f,                 \
    .length = FF_ARRAY_ELEMS(f), \
}

static const struct {
    const char *name;
    const int *coeff;
    int length;
} ref_filters[] = {
    REF_FILTER(blur),
    REF_FILTER(contour),
    REF_FILTER(detail),
    REF_FILTER(edge_enhance),
    REF_FILTER(edge_enhance_more),
    REF_FILTER(emboss),
    REF_FILTER(find_edges),
    REF_FILTER(smooth),
    REF_FILTER(smooth_more),
    REF_FILTER(sharpen),
};

static av_cold int init(AVFilterContext *ctx, const char *args)
{
    int i;
    EnhanceContext *enhance = ctx->priv;

    if (!args) {
        av_log(ctx, AV_LOG_ERROR, "requires filter name\n");
        return AVERROR(EINVAL);
    }

    if (!strcmp(args, "custom")) {
        // TODO
    }

    enhance->filter_ref = -1;
    for (i = 0; i < FF_ARRAY_ELEMS(ref_filters); i++) {
        if (!strcmp(args, ref_filters[i].name)) {
            enhance->filter_ref = i;
            break;
        }
    }
    if (enhance->filter_ref < 0) {
        av_log(ctx, AV_LOG_ERROR, "unknown filter '%s', pick one of the following:", args);
        for (i = 0; i < FF_ARRAY_ELEMS(ref_filters); i++)
            av_log(ctx, AV_LOG_ERROR, " %s", ref_filters[i].name);
        av_log(ctx, AV_LOG_ERROR, "\n");
    }

    enhance->sws_flags = SWS_BICUBIC;

    return 0;
}

static av_cold void uninit(AVFilterContext *ctx)
{
    EnhanceContext *enhance = ctx->priv;

    sws_freeContext(enhance->luma.filter_context);
    sws_freeContext(enhance->chroma.filter_context);
}

static int query_formats(AVFilterContext *ctx)
{
    static const enum AVPixelFormat pix_fmts[] = {
        AV_PIX_FMT_YUV444P,      AV_PIX_FMT_YUV422P,
        AV_PIX_FMT_YUV420P,      AV_PIX_FMT_YUV411P,
        AV_PIX_FMT_YUV410P,      AV_PIX_FMT_YUV440P,
        AV_PIX_FMT_GRAY8,
        AV_PIX_FMT_NONE
    };

    ff_set_common_formats(ctx, ff_make_format_list(pix_fmts));

    return 0;
}

static int alloc_sws_context(FilterParam *f, int filter_id, int width, int height, unsigned int flags)
{
    int i;
    SwsVector *vec;
    SwsFilter sws_filter;
    const int *filter = ref_filters[filter_id].coeff;

    vec = sws_allocVec(ref_filters[filter_id].length);
    if (!vec)
        return AVERROR(EINVAL);

    for (i = 0; i < vec->length; i++)
        vec->coeff[i] = filter[i];
    sws_normalizeVec(vec, 1.0);

    sws_filter.lumH = sws_filter.lumV = vec;
    sws_filter.chrH = sws_filter.chrV = NULL;
    f->filter_context = sws_getCachedContext(NULL,
                                             width, height, AV_PIX_FMT_GRAY8,
                                             width, height, AV_PIX_FMT_GRAY8,
                                             flags, &sws_filter, NULL, NULL);

    sws_freeVec(vec);

    if (!f->filter_context)
        return AVERROR(EINVAL);

    return 0;
}

static int config_props(AVFilterLink *inlink)
{
    EnhanceContext *enhance = inlink->dst->priv;
    const AVPixFmtDescriptor *desc = av_pix_fmt_desc_get(inlink->format);

    enhance->hsub = desc->log2_chroma_w;
    enhance->vsub = desc->log2_chroma_h;

    alloc_sws_context(&enhance->luma, enhance->filter_ref,
                      inlink->w, inlink->h, enhance->sws_flags);
    alloc_sws_context(&enhance->chroma, enhance->filter_ref,
                      inlink->w >> enhance->hsub, inlink->h >> enhance->vsub,
                      enhance->sws_flags);

    return 0;
}

static void do_enhance(uint8_t       *dst, const int dst_linesize,
                       const uint8_t *src, const int src_linesize,
                       const int w, const int h,
                       struct SwsContext *filter_context)
{
    /* Declare arrays of 4 to get aligned data */
    const uint8_t* const src_array[4] = {src};
    uint8_t *dst_array[4]             = {dst};
    int src_linesize_array[4] = {src_linesize};
    int dst_linesize_array[4] = {dst_linesize};

    sws_scale(filter_context, src_array, src_linesize_array,
              0, h, dst_array, dst_linesize_array);

}

static int end_frame(AVFilterLink *inlink)
{
    EnhanceContext  *enhance  = inlink->dst->priv;
    AVFilterBufferRef *inpic  = inlink->cur_buf;
    AVFilterLink *outlink     = inlink->dst->outputs[0];
    AVFilterBufferRef *outpic = outlink->out_buf;

    do_enhance(outpic->data[0], outpic->linesize[0],
               inpic->data[0],  inpic->linesize[0],
               inlink->w, inlink->h,
               enhance->luma.filter_context);

    if (inpic->data[2]) {
        const int cw = inlink->w >> enhance->hsub;
        const int ch = inlink->h >> enhance->vsub;

        do_enhance(outpic->data[1], outpic->linesize[1],
                   inpic->data[1],  inpic->linesize[1],
                   cw, ch,
                   enhance->chroma.filter_context);
        do_enhance(outpic->data[2], outpic->linesize[2],
                   inpic->data[2],  inpic->linesize[2],
                   cw, ch,
                   enhance->chroma.filter_context);
    }

    ff_draw_slice(outlink, 0, outlink->h, 1);
    return ff_end_frame(outlink);
}

AVFilter avfilter_vf_enhance = {
    .name          = "enhance",
    .description   = NULL_IF_CONFIG_SMALL("Misc or custom enhancing filters."),
    .priv_size     = sizeof(EnhanceContext),
    .init          = init,
    .uninit        = uninit,
    .query_formats = query_formats,

    .inputs = (const AVFilterPad[]) {
        {
            .name         = "default",
            .type         = AVMEDIA_TYPE_VIDEO,
            .end_frame    = end_frame,
            .config_props = config_props,
            .min_perms    = AV_PERM_READ,
        },
        { .name = NULL }
    },
    .outputs = (const AVFilterPad[]) {
        {
            .name         = "default",
            .type         = AVMEDIA_TYPE_VIDEO,
        },
        { .name = NULL }
    }
};

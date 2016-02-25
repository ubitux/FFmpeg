/*
 * software YUV to RGB converter
 *
 * Copyright (C) 2009 Konstantin Shishkov
 *
 * MMX/MMXEXT template stuff (needed for fast movntq support),
 * 1,4,8bpp support and context / deglobalize stuff
 * by Michael Niedermayer (michaelni@gmx.at)
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

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "config.h"
#include "libswscale/rgb2rgb.h"
#include "libswscale/swscale.h"
#include "libswscale/swscale_internal.h"
#include "libavutil/attributes.h"
#include "libavutil/x86/asm.h"
#include "libavutil/x86/cpu.h"
#include "libavutil/cpu.h"

#if HAVE_INLINE_ASM

#define DITHER1XBPP // only for MMX

/* hope these constant values are cache line aligned */
DECLARE_ASM_CONST(8, uint64_t, mmx_00ffw)   = 0x00ff00ff00ff00ffULL;
DECLARE_ASM_CONST(8, uint64_t, mmx_redmask) = 0xf8f8f8f8f8f8f8f8ULL;
DECLARE_ASM_CONST(8, uint64_t, mmx_grnmask) = 0xfcfcfcfcfcfcfcfcULL;
DECLARE_ASM_CONST(8, uint64_t, pb_e0) = 0xe0e0e0e0e0e0e0e0ULL;
DECLARE_ASM_CONST(8, uint64_t, pb_03) = 0x0303030303030303ULL;
DECLARE_ASM_CONST(8, uint64_t, pb_07) = 0x0707070707070707ULL;
DECLARE_ASM_CONST(8, uint64_t, pw_1024) = 0x0400040004000400ULL;

//MMX versions
#if HAVE_MMX_INLINE && HAVE_6REGS
#undef RENAME
#undef COMPILE_TEMPLATE_MMXEXT
#define COMPILE_TEMPLATE_MMXEXT 0
#define RENAME(a) a ## _mmx
#include "yuv2rgb_template.c"
#endif /* HAVE_MMX_INLINE && HAVE_6REGS */

// MMXEXT versions
#if HAVE_MMXEXT_INLINE && HAVE_6REGS
#undef RENAME
#undef COMPILE_TEMPLATE_MMXEXT
#define COMPILE_TEMPLATE_MMXEXT 1
#define RENAME(a) a ## _mmxext
#include "yuv2rgb_template.c"
#endif /* HAVE_MMXEXT_INLINE && HAVE_6REGS */

#endif /* HAVE_INLINE_ASM */


#if 0
// y_offset >> 6
static void yuv420p_to_rgba_mmxsimu(int w, int h,
                                    uint8_t *dst, int linesize,
                                    const uint8_t *srcY, int linesizeY,
                                    const uint8_t *srcU, int linesizeU,
                                    const uint8_t *srcV, int linesizeV,
                                    const int16_t *table,
                                    int y_offset, int y_coeff)
{
    int x, y;
    const int16_t v2r = table[0];
    const int16_t u2g = table[1];
    const int16_t v2g = table[2];
    const int16_t u2b = table[3];

    for (y = 0; y < h; y++) {
        for (x = 0; x < w; x++) {
            const int16_t  u = srcU[x/2]*(1<<3) - 128*(1<<3);
            const int16_t  v = srcV[x/2]*(1<<3) - 128*(1<<3);
            const int16_t y1 = srcY[x  ]*(1<<3) - y_offset;

            const int16_t ug  = (u2g * u) >> 16;
            const int16_t vg  = (v2g * v) >> 16;
            const int16_t ub  = (u2b * u) >> 16;
            const int16_t vr  = (v2r * v) >> 16;
            const int16_t y1c = (y1 * y_coeff) >> 16;

            const int16_t premult_r = vr;
            const int16_t premult_g = vg + ug;
            const int16_t premult_b =      ub;

            dst[x*4+0] = av_clip_uint8(y1c + premult_r);
            dst[x*4+1] = av_clip_uint8(y1c + premult_g);
            dst[x*4+2] = av_clip_uint8(y1c + premult_b);
            dst[x*4+3] = 0xff;
        }
        dst  += linesize;
        srcY += linesizeY;
        srcU += linesizeU * (y & 1);
        srcV += linesizeV * (y & 1);
    }
}
#endif

struct yuv2rgb_ctx {
    uint8_t *dst;
    const uint8_t *src_y;
    const uint8_t *src_u;
    const uint8_t *src_v;
    int linesize_dst;
    int linesize_y;
    int linesize_u;
    int linesize_v;
    uint64_t *y_coeff;
    uint64_t *y_offset;
    uint64_t *v2r_coeff;
    uint64_t *u2g_coeff;
    uint64_t *v2g_coeff;
    uint64_t *u2b_coeff;
};

void ff_yuv420p_to_rgba_mmx(int w, int h, const struct *yuv2rgb_ctx);

static int yuv420p_to_rgba_wrapper_mmxsimu(SwsContext *c, const uint8_t *src[],
                                           int srcStride[], int srcSliceY, int srcSliceH,
                                           uint8_t *dst[], int dstStride[])
{
    const struct yuv2rgb_ctx ctx = {
        .dst          = dst[0] + srcSliceY * dstStride[0],
        .src          = src[0],
        .src          = src[1],
        .src          = src[2],
        .linesize_dst = dstStride[0],
        .linesize_y   = srcStride[0],
        .linesize_u   = srcStride[1],
        .linesize_v   = srcStride[2],
        .y_coeff      = &c->yCoeff,
        .y_offset     = &c->yOffset,
        .v2r_coeff    = &c->vrCoeff,
        .v2r_coeff    = &c->vrCoeff,
        .u2g_coeff    = &c->ugCoeff,
        .v2g_coeff    = &c->vgCoeff,
        .u2b_coeff    = &c->ubCoeff,
    };
    ff_yuv420p_to_rgba_mmx(c->srcW, srcSliceH, &ctx);
    return srcSliceH;
}

av_cold SwsFunc ff_yuv2rgb_init_x86(SwsContext *c)
{
    int cpu_flags = av_get_cpu_flags();

#if 1
    if (EXTERNAL_MMX(cpu_flags)) {
        if (c->srcFormat == AV_PIX_FMT_YUV420P && c->dstFormat == AV_PIX_FMT_RGBA)
            return yuv420p_to_rgba_wrapper_mmxsimu;
            //return ff_yuv420p_to_rgba_mmx;
    }
#endif

#if HAVE_MMX_INLINE && HAVE_6REGS

#if HAVE_MMXEXT_INLINE
    if (INLINE_MMXEXT(cpu_flags)) {
        switch (c->dstFormat) {
        case AV_PIX_FMT_RGB24:
            return yuv420_rgb24_mmxext;
        case AV_PIX_FMT_BGR24:
            return yuv420_bgr24_mmxext;
        }
    }
#endif

    if (INLINE_MMX(cpu_flags)) {
        switch (c->dstFormat) {
            case AV_PIX_FMT_RGB32:
                if (c->srcFormat == AV_PIX_FMT_YUVA420P) {
#if HAVE_7REGS && CONFIG_SWSCALE_ALPHA
                    return yuva420_rgb32_mmx;
#endif
                    break;
                } else
                    return yuv420_rgb32_mmx;
            case AV_PIX_FMT_BGR32:
                if (c->srcFormat == AV_PIX_FMT_YUVA420P) {
#if HAVE_7REGS && CONFIG_SWSCALE_ALPHA
                    return yuva420_bgr32_mmx;
#endif
                    break;
                } else {
                    av_log(0,0,"-> yuv420_bgr32_mmx\n");
                    return yuv420_bgr32_mmx;
                }
            case AV_PIX_FMT_RGB24:
                return yuv420_rgb24_mmx;
            case AV_PIX_FMT_BGR24:
                return yuv420_bgr24_mmx;
            case AV_PIX_FMT_RGB565:
                return yuv420_rgb16_mmx;
            case AV_PIX_FMT_RGB555:
                return yuv420_rgb15_mmx;
        }
    }
#endif /* HAVE_MMX_INLINE  && HAVE_6REGS */

    return NULL;
}

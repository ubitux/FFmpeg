;******************************************************************************
;* YUV to RGB
;*
;* Copyright (C) 2001-2007 Michael Niedermayer
;* Copyright (C) 2009 Konstantin Shishkov
;*
;* This file is part of FFmpeg.
;*
;* FFmpeg is free software; you can redistribute it and/or
;* modify it under the terms of the GNU Lesser General Public
;* License as published by the Free Software Foundation; either
;* version 2.1 of the License, or (at your option) any later version.
;*
;* FFmpeg is distributed in the hope that it will be useful,
;* but WITHOUT ANY WARRANTY; without even the implied warranty of
;* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;* Lesser General Public License for more details.
;*
;* You should have received a copy of the GNU Lesser General Public
;* License along with FFmpeg; if not, write to the Free Software
;* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;******************************************************************************

%include "libavutil/x86/x86util.asm"

SECTION_RODATA

cextern pw_255
cextern pb_3

pb_7:        times 8 db 0x07
pb_e0:       times 8 db 0xe0
mmx_redmask: times 8 db 0xf8
mmx_grnmask: times 8 db 0xfc

SECTION .text

%if 0
%macro DECLARE_YUV_TO_RGB_FUNCS 3 ; ins, src_fmt, depth
cglobal yuv420_to_rgb24_%1, 0,0,0, w, h, \
                                    dst, dst_stride, \
                                    src_y, src_stride_y, \
                                    src_u, src_stride_u, \
                                    src_v, src_stride_v, \

    pxor m4, m4
    movq (%5, %0, 2), %%mm6
    movd    (%2, %0), %%mm0
    movd    (%3, %0), %%mm1
.loop:
    /* convert Y, U, V into Y1', Y2', U', V' */
    movq      m7, m6
    punpcklbw m0, m4
    punpcklbw m1, m4
    pand     m6, [pw_255]
    psrlw    m7, 8
    psllw    m0, 3
    psllw    m1, 3
    psllw    m6, 3
    psllw    m7, 3

    psubsw   m0, dither

    psubsw   "U_OFFSET"(%4), %%mm0
    psubsw   "V_OFFSET"(%4), %%mm1
    psubw    "Y_OFFSET"(%4), %%mm6
    psubw    "Y_OFFSET"(%4), %%mm7

     /* multiply by coefficients */
    movq      %%mm0, %%mm2
    movq      %%mm1, %%mm3
    pmulhw   "UG_COEFF"(%4), %%mm2
    pmulhw   "VG_COEFF"(%4), %%mm3
    pmulhw   "Y_COEFF" (%4), %%mm6
    pmulhw   "Y_COEFF" (%4), %%mm7
    pmulhw   "UB_COEFF"(%4), %%mm0
    pmulhw   "VR_COEFF"(%4), %%mm1
    paddsw    %%mm3, %%mm2
    /* now: mm0 = UB, mm1 = VR, mm2 = CG */
    /*      mm6 = Y1, mm7 = Y2 */

    /* produce RGB */
    movq      m3, m7
    movq      m5, m7
    paddsw    m3, m0
    paddsw    m5, m1
    paddsw    m7, m2
    paddsw    m0, m6
    paddsw    m1, m6
    paddsw    m2, m6

    /* pack and interleave even/odd pixels */
    packuswb  m0, m1
    packuswb  m3, m5
    packuswb  m2, m2
    movq      m1, m0
    packuswb  m7, m7
    punpcklbw m0, m3
    punpckhbw m1, m3
    punpcklbw m2, m7

    movq 8 (%5, %0, 2), %%mm6
    movd 4 (%3, %0),    %%mm1
    movd 4 (%2, %0),    %%mm0

    add imageq, %3*8
    add indexd, 4
    js   .loop

    emms

        : "+r" (index), "+r" (image)                              \
        : "r" (pu - index), "r" (pv - index), "r"(&c->redDither), \
          "r" (py - 2*index)                                      \
          NAMED_CONSTRAINTS_ADD(mmx_00ffw,pb_03,pb_07,mmx_redmask,pb_e0) \
          RGB_PACK24_B_OPERANDS                                   \
        : "memory"                                                \
        );                                                        \
    }                                                             \


#define YUV2RGB_LOOP(depth)                                          \
    h_size = (c->dstW + 7) & ~7;                                     \
    if (h_size * depth > FFABS(dstStride[0]))                        \
        h_size -= 8;                                                 \
                                                                     \
    vshift = c->srcFormat != AV_PIX_FMT_YUV422P;                        \
                                                                     \
    __asm__ volatile ("pxor %mm4, %mm4\n\t");                        \
    for (y = 0; y < srcSliceH; y++) {                                \
        uint8_t *image    = dst[0] + (y + srcSliceY) * dstStride[0]; \
        const uint8_t *py = src[0] +               y * srcStride[0]; \
        const uint8_t *pu = src[1] +   (y >> vshift) * srcStride[1]; \
        const uint8_t *pv = src[2] +   (y >> vshift) * srcStride[2]; \
        x86_reg index = -h_size / 2;                                 \

%endmacro


%macro DECLARE_ALLYUV_TO_RGB_FUNCS 1 ; ins
INIT_MMX %1
DECLARE_YUV420P_FUNCS  %1, yuv420p
DECLARE_YUVA420P_FUNCS %1, yuva420p
DECLARE_YUV422P_FUNCS  %1, yuv422p
%endmacro



static inline int RENAME(yuv420_rgb24)(SwsContext *c, const uint8_t *src[],
                                       int srcStride[],
                                       int srcSliceY, int srcSliceH,
                                       uint8_t *dst[], int dstStride[])

cglobal yuv420_to_rgb24_%1, 2,6,16, dst, stride, mstride, dst2, stride3, mstride3

    int y, h_size, vshift;

    YUV2RGB_LOOP(3)

        YUV2RGB_INITIAL_LOAD
        YUV2RGB
        RGB_PACK24(REG_BLUE, REG_RED)

    YUV2RGB_ENDLOOP(3)
    YUV2RGB_OPERANDS
    YUV2RGB_ENDFUNC

%endmacro

DECLARE_YUV2RGB_FUNCS mmx
DECLARE_YUV2RGB_FUNCS mmxext
%endif

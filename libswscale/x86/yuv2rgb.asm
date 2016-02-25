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

%if ARCH_X86_64
%define pointer resq
%else
%define pointer resd
%endif

struc yuv2rgb_ctx
    .dst:                   pointer 1
    .src_y:                 pointer 1
    .src_u:                 pointer 1
    .src_v:                 pointer 1
    .linesize_dst:          resd 1
    .linesize_y:            resd 1
    .linesize_u:            resd 1
    .linesize_v:            resd 1
    .y_coeff:               pointer 1
    .y_offset:              pointer 1
    .v2r_coeff:             pointer 1
    .u2g_coeff:             pointer 1
    .v2g_coeff:             pointer 1
    .u2b_coeff:             pointer 1
endstruc

SECTION_RODATA

cextern pw_255
cextern pb_3

pb_7:        times 8 db 0x07
pb_e0:       times 8 db 0xe0
pw_00ff:     times 4 dw 0x00ff
pw_1024:     times 4 dw 1024
mmx_redmask: times 8 db 0xf8
mmx_grnmask: times 8 db 0xfc

SECTION .text

INIT_MMX mmx
cglobal yuv420p_to_rgba, 0,0,0
        pxor        m4, m4

.loop:
        ; YUV to RGB
        mova        m6, [r2+yuv2rgb_ctx.src_y]              ; Y0 Y1 Y2 Y3 Y4 Y5 Y6 Y7
        movh        m0, [r2+yuv2rgb_ctx.src_u]              ; U0 U1 U2 U3
        movh        m1, [r2+yuv2rgb_ctx.src_v]              ; V0 V1 V2 V3
        mova        m7, m6
        punpcklbw   m0, m4                                  ; .. U0 .. U1 .. U2 .. U3
        punpcklbw   m1, m4                                  ; .. V0 .. V1 .. V2 .. V3
        pand        m6, [pw_00ff]                           ; .. Y1 .. Y3 .. Y5 .. Y7
        psrlw       m7, 8                                   ; .. Y0 .. Y2 .. Y3 .. Y4
        psllw       m0, 3                                   ; U scaled by 1<<3
        psllw       m1, 3                                   ; V scaled by 1<<3
        psllw       m6, 3                                   ; Y part 1 scaled by 1<<3
        psllw       m7, 3                                   ; Y part 2 scaled by 1<<3
        psubsw      m0, [pw_1024]                           ; U offset (128) scaled by 1<<3
        psubsw      m1, [pw_1024]                           ; V offset (128) scaled by 1<<3
        psubw       m6, [r2+yuv2rgb_ctx.y_offset]           ; luma offset already scaled by 1<<3
        psubw       m7, [r2+yuv2rgb_ctx.y_offset]
        pmulhw      m2, m0, [r2+yuv2rgb_ctx.u2g_coeff]      ; ug  = (u * u2g) >> 16
        pmulhw      m3, m1, [r2+yuv2rgb_ctx.v2g_coeff]      ; vg  = (v * v2g) >> 16
        pmulhw      m6,     [r2+yuv2rgb_ctx.y_coeff]        ; y1c = (y1 * y_coeff) >> 16
        pmulhw      m7,     [r2+yuv2rgb_ctx.y_coeff]        ; y2c = (y2 * y_coeff) >> 16
        pmulhw      m0,     [r2+yuv2rgb_ctx.u2b_coeff]      ; ub  = (u * u2b) >> 16         (blue)
        pmulhw      m1,     [r2+yuv2rgb_ctx.v2r_coeff]      ; vr  = (v * v2r) >> 16         (red)
        paddsw      m2, m3                                  ; ug + vg                       (green)
        paddsw      m3, m7, m0                              ; m3 = y2c + blue
        paddsw      m5, m7, m1                              ; m5 = y2c + red
        paddsw      m7, m2                                  ; m7 = y2c + green
        paddsw      m0, m6                                  ; m0 = y1c + blue
        paddsw      m1, m6                                  ; m1 = y1c + red
        paddsw      m2, m6                                  ; m2 = y1c + green

        ; RGB pack interleave
        packuswb    m0, m1
        packuswb    m3, m5
        packuswb    m2, m2
        mova        m1, m0
        packuswb    m7, m7
        punpcklbw   m0, m3
        punpckhbw   m1, m3
        punpcklbw   m2, m7

        pcmpeqd     m3, m3                                  ; set alpha to 0xff

        ; RGB pack 32
        punpckhbw   m5, m1, m2
        punpcklbw   m1, m2
        punpckhbw   m6, m0, m3
        punpcklbw   m0, m3
        mova        [r2+yuv2rgb_ctx.dst],    m1
        mova        [r2+yuv2rgb_ctx.dst+8],  m2
        mova        [r2+yuv2rgb_ctx.dst+16], m5
        mova        [r2+yuv2rgb_ctx.dst+24], m3

        sub         r2, 1
        jne       .loop
        RET

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

#include "libavutil/aarch64/asm.S"

function ff_add_bytes_l2_aarch64, export=1
1:
        ld1         {v2.16B}, [x1], #16
        ld1         {v3.16B}, [x2], #16
        add         v4.16B, v2.16B, v3.16B
        st1         {v4.16B}, [x0], #16
        subs        w3, w3, #16
        b.gt        1b
        ret
endfunc

function ff_add_paeth_prediction_aarch64, export=1
    ret
endfunc

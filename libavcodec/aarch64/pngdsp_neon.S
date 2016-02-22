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
1:      ld1         {v0.16B}, [x1], #16
        ld1         {v1.16B}, [x2], #16
        add         v2.16B, v0.16B, v1.16B
        st1         {v2.16B}, [x0], #16
        subs        w3, w3, #16
        b.gt        1b
        ret
endfunc

/*
static void add_png_paeth_prediction_c(uint8_t *dst, uint8_t *src, uint8_t *top,
                                       int w, int bpp)
{
    int i;
    for (i = 0; i < w; i++) {
        int a, b, c, p, pa, pb, pc;

        a = dst[i - bpp];
        b = top[i];
        c = top[i - bpp];

        p  = b - c;
        pc = a - c;

        pa = abs(p);
        pb = abs(pc);
        pc = abs(p + pc);

        if (pa <= pb && pa <= pc)
            p = a;
        else if (pb <= pc)
            p = b;
        else
            p = c;
        dst[i] = p + src[i];
    }
}
*/

// XXX thefuckimdoing, simplify this shit
.macro repack dst reg1 reg2
        uqrshrn     \reg1\().8B, \reg1\().8H, #8
        uqrshrn     \reg2\().8B, \reg2\().8H, #8
        mov         \dst\().D[0], \reg1\().D[0]
        mov         \dst\().D[1], \reg2\().D[0]
.endm

/*
 * x0 dst
 * x1 src
 * x2 top
 * w3 w
 * w4 bpp
 */

/*
display $v7.b.u
display $v6.b.u
display $v5.b.u
display $v31.b.u
display $v30.b.u
display $v29.b.u
display $v28.b.u
display $v27.b.u
display $v26.b.u

display $v18.b.u
display $v17.b.u
display $v16.b.u


display $v19.h.s
display $v18.h.s
display $v17.h.s
display $v16.h.s
display $v2.b.u
display $v1.b.u
display $v0.b.u

display $v23.h.u
display $v22.h.u
display $v21.h.u
display $v20.h.u

display $v18.b.u
display $v17.b.u
display $v16.b.u
display $v2.b.u
display $v1.b.u
display $v0.b.u
*/

function ff_add_paeth_prediction_aarch64, export=1
        sub         x5, x0, w4, UXTW            // dst - bpp
        sub         x6, x2, w4, UXTW            // top - bpp
        and w3, w3, #0xfffffff0 // XXX
1:      ld1         {v0.16B}, [x5], #16         // a
        ld1         {v1.16B}, [x2], #16         // b
        ld1         {v2.16B}, [x6], #16         // c
        usubl       v16.8H, v1.8B,  v2.8B       // p  = b - c
        usubl2      v17.8H, v1.16B, v2.16B
        usubl       v18.8H, v0.8B,  v2.8B       // pc = a - c
        usubl2      v19.8H, v0.16B, v2.16B
        abs         v20.8H, v16.8H              // pa = abs(p)
        abs         v21.8H, v17.8H
        abs         v22.8H, v18.8H              // pb = abs(pc)
        abs         v23.8H, v19.8H
        add         v24.8H, v16.8H, v18.8H      // pc = p+pc
        add         v25.8H, v17.8H, v19.8H
        abs         v24.8H, v24.8H              // pc = abs(pc)
        abs         v25.8H, v25.8H
        cmge        v26.8H, v22.8H, v20.8H      // pb >= pa (16b)
        cmge        v27.8H, v23.8H, v21.8H
        cmge        v28.8H, v24.8H, v20.8H      // pc >= pa (16b)
        cmge        v29.8H, v25.8H, v21.8H
        cmge        v30.8H, v24.8H, v22.8H      // pc >= pb (16b)
        cmge        v31.8H, v25.8H, v23.8H
        repack      v5, v26, v27                // pb >= pa (8b)
        repack      v6, v28, v29                // pc >= pa (8b)
        repack      v7, v30, v31                // pc >= pb (8b)
        and         v16.16B, v5.16B, v6.16B     //   pb >= pa && pc >= pa                  <- A mask
        not         v19.16B, v16.16B            // !(pb >= pa && pc >= pa)
        and         v17.16B, v19.16B, v7.16B    // !(pb >= pa && pc >= pa) &&   pc >= pb   <- B mask
        bic         v18.16B, v19.16B, v7.16B    // !(pb >= pa && pc >= pa) && !(pc >= pb)  <- C mask
        and         v0.16B, v0.16B, v16.16B     // case p = a
        and         v1.16B, v1.16B, v17.16B     // case p = b
        and         v2.16B, v2.16B, v18.16B     // case p = c
        orr         v4.16B, v0.16B, v1.16B
        orr         v4.16B, v4.16B, v2.16B
        ld1         {v3.16B}, [x1], #16         // src (TODO: preload at the beginning?)
        add         v3.16B, v3.16B, v4.16B
        st1         {v3.16B}, [x0], #16
        subs        w3, w3, #16
        b.gt        1b
        ret
endfunc

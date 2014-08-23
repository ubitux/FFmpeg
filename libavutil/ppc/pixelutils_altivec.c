/*
 * This file is part of FFmpeg.
 *
 * Copyright (c) 2002 Brian Foley
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

#include "util_altivec.h"
#include "pixelutils_altivec.h"

int ff_pixelutils_sad_8x8_altivec(const uint8_t *src1, ptrdiff_t stride1,
                                  const uint8_t *src2, ptrdiff_t stride2)
{
    int i, s;
    const vector unsigned int zero =
        (const vector unsigned int) vec_splat_u32(0);
    const vector unsigned char permclear =
        (vector unsigned char)
        { 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 0, 0, 0, 0, 0 };
    vector unsigned char perm1 = vec_lvsl(0, src1);
    vector unsigned char perm2 = vec_lvsl(0, src2);
    vector unsigned int sad = (vector unsigned int) vec_splat_u32(0);
    vector signed int sumdiffs;

    for (i = 0; i < 8; i++) {
        /* Read potentially unaligned pixels into t1 and t2.
         * Since we're reading 16 pixels, and actually only want 8,
         * mask out the last 8 pixels. The 0s don't change the sum. */
        vector unsigned char src1l = vec_ld(0, src1);
        vector unsigned char src1r = vec_ld(7, src1);
        vector unsigned char src2l = vec_ld(0, src2);
        vector unsigned char src2r = vec_ld(7, src2);
        vector unsigned char t1 = vec_and(vec_perm(src1l, src1r, perm1),
                                          permclear);
        vector unsigned char t2 = vec_and(vec_perm(src2l, src2r, perm2),
                                          permclear);

        /* Calculate a sum of abs differences vector. */
        vector unsigned char t3 = vec_max(t1, t2);
        vector unsigned char t4 = vec_min(t1, t2);
        vector unsigned char t5 = vec_sub(t3, t4);

        /* Add each 4 pixel group together and put 4 results into sad. */
        sad = vec_sum4s(t5, sad);

        src1 += stride1;
        src2 += stride2;
    }

    /* Sum up the four partial sums, and put the result into s. */
    sumdiffs = vec_sums((vector signed int) sad, (vector signed int) zero);
    sumdiffs = vec_splat(sumdiffs, 3);
    vec_ste(sumdiffs, 0, &s);

    return s;
}

int ff_pixelutils_sad_16x16_altivec(const uint8_t *src1, ptrdiff_t stride1,
                                    const uint8_t *src2, ptrdiff_t stride2)
{
    int i, s;
    const vector unsigned int zero =
        (const vector unsigned int) vec_splat_u32(0);
    vector unsigned char perm = vec_lvsl(0, src2);
    vector unsigned int sad = (vector unsigned int) vec_splat_u32(0);
    vector signed int sumdiffs;

    for (i = 0; i < 16; i++) {
        /* Read potentially unaligned pixels into t1 and t2. */
        vector unsigned char src2l = vec_ld(0,  src2);
        vector unsigned char src2r = vec_ld(15, src2);
        vector unsigned char t1 = vec_ld(0, src1);
        vector unsigned char t2 = vec_perm(src2l, src2r, perm);

        /* Calculate a sum of abs differences vector. */
        vector unsigned char t3 = vec_max(t1, t2);
        vector unsigned char t4 = vec_min(t1, t2);
        vector unsigned char t5 = vec_sub(t3, t4);

        /* Add each 4 pixel group together and put 4 results into sad. */
        sad = vec_sum4s(t5, sad);

        src1 += stride1;
        src2 += stride2;
    }

    /* Sum up the four partial sums, and put the result into s. */
    sumdiffs = vec_sums((vector signed int) sad, (vector signed int) zero);
    sumdiffs = vec_splat(sumdiffs, 3);
    vec_ste(sumdiffs, 0, &s);

    return s;
}

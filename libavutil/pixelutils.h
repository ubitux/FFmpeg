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

#ifndef AVUTIL_PIXELUTILS_H
#define AVUTIL_PIXELUTILS_H

#include <stddef.h>
#include <stdint.h>
#include "common.h"

/**
 * Sum of abs(src1[x] - src2[x])
 */
typedef int (*av_pixelutils_sad_fn)(const uint8_t *src1, ptrdiff_t stride1,
                                    const uint8_t *src2, ptrdiff_t stride2);

/**
 * Get a potentially optimized pointer to a Sum-of-absolute-differences
 * function (see the av_pixelutils_sad_fn prototype).
 *
 * @param w_bits  1<<w_bits is the requested width of the block size
 * @param h_bits  1<<h_bits is the requested height of the block size
 * @param aligned If set to 2, the returned sad function will assume src1 and
 *                src2 addresses are aligned on the block size.
 *                If set to 1, the returned sad function will assume src1 is
 *                aligned on the block size.
 *                If set to 0, the returned sad function assume no particular
 *                alignment.
 * @param log_ctx context used for logging, can be NULL
 *
 * @return a pointer to the SAD function or NULL in case of error (because of
 *         invalid parameters)
 */
av_pixelutils_sad_fn av_pixelutils_get_sad_fn(int w_bits, int h_bits,
                                              int aligned, void *log_ctx);

/**
 * Compute the SAD of two large buffers block per block with the specified
 * block SAD function.
 *
 * @param sadfn SAD function which can be obtained with av_pixelutils_get_sad_fn
 * @param w     buffers width to process (the latest sad function call will be
 *              on w-bsize+1)
 * @param h     buffers height to process (the latest sad function call will be
 *              on h-bsize+1)
 * @param bsize block size (width and height, e.g: 8 for a 8x8 block)
 *
 * @return SAD or -1 in case libavutil is not compiled with the pixelutils.
 *
 * @note The function will not compute w % bsize remaining bytes on the right
 * if any, even if the linesize is large enough to allow such read (this
 * linesize can stay uninitialized). Same thing for the h % bsize remaining
 * lines at the bottom.
 */
int64_t av_pixelutils_bdiff(const uint8_t *s1, ptrdiff_t stride1,
                            const uint8_t *s2, ptrdiff_t stride2,
                            av_pixelutils_sad_fn sadfn,
                            int w, int h, int bsize);

#endif /* AVUTIL_PIXELUTILS_H */

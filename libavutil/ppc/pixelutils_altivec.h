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

#ifndef AVUTIL_PPC_PIXELUTILS_ALTIVEC_H
#define AVUTIL_PPC_PIXELUTILS_ALTIVEC_H

#include <stdint.h>

int ff_pixelutils_sad_8x8_altivec(const uint8_t *src1, ptrdiff_t stride1,
                                  const uint8_t *src2, ptrdiff_t stride2);

int ff_pixelutils_sad_16x16_altivec(const uint8_t *src1, ptrdiff_t stride1,
                                    const uint8_t *src2, ptrdiff_t stride2);

#endif /* AVUTIL_PPC_PIXELUTILS_ALTIVEC_H */

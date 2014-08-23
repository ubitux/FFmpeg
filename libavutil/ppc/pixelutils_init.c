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

#include "config.h"
#include "libavutil/attributes.h"
#include "libavutil/cpu.h"
#include "libavutil/ppc/cpu.h"
#include "pixelutils.h"
#include "pixelutils_altivec.h"

void ff_pixelutils_sad_init_ppc(av_pixelutils_sad_fn *sad, int aligned)
{
    if (PPC_ALTIVEC(av_get_cpu_flags())) {
        /* XXX: aligned? */
        sad[2] = ff_pixelutils_sad_8x8_altivec;
        sad[3] = ff_pixelutils_sad_16x16_altivec;
    }
}

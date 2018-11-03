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

#include "common.h"
#include "subtitlefmt.h"

static const char * const subtitle_fmt_names[AV_SUBTITLE_FMT_NB] = {
    [AV_SUBTITLE_FMT_BITMAP] = "bitmap",
    [AV_SUBTITLE_FMT_TEXT]   = "text",
};

const char *av_get_subtitle_fmt_name(enum AVSubtitleFormat subtitle_fmt)
{
    if (subtitle_fmt < 0 || subtitle_fmt >= AV_SUBTITLE_FMT_NB)
        return NULL;
    return subtitle_fmt_names[subtitle_fmt];
}

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

#ifndef AVUTIL_SUBTITLEFMT_H
#define AVUTIL_SUBTITLEFMT_H

/**
 * Subtitle formats
 */
enum AVSubtitleFormat {
    AV_SUBTITLE_FMT_NONE = -1,
    AV_SUBTITLE_FMT_BITMAP,
    AV_SUBTITLE_FMT_TEXT,
    AV_SUBTITLE_FMT_NB  ///< Number of subtitle formats. DO NOT USE if linking dynamically
};

const char *av_get_subtitle_fmt_name(enum AVSubtitleFormat subtitle_fmt);

#endif /* AVUTIL_SUBTITLEFMT_H */

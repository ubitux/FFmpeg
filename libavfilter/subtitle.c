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

#include "libavutil/common.h"

#include "subtitle.h"
#include "avfilter.h"
#include "internal.h"

AVFrame *ff_null_get_subtitle_buffer(AVFilterLink *link, int nb_rects)
{
    return ff_get_subtitle_buffer(link->dst->outputs[0], nb_rects);
}

AVFrame *ff_default_get_subtitle_buffer(AVFilterLink *link, int nb_rects)
{
    AVFrame *frame = av_frame_alloc();
    int ret;

    if (!frame)
        return NULL;

    frame->sub_nb_rects = nb_rects;
    frame->format       = link->format;
    ret = av_frame_get_buffer(frame, 0);
    if (ret < 0) {
        av_frame_free(&frame);
        return NULL;
    }

    return frame;
}

AVFrame *ff_get_subtitle_buffer(AVFilterLink *link, int nb_samples)
{
    AVFrame *ret = NULL;

    if (link->dstpad->get_subtitle_buffer)
        ret = link->dstpad->get_audio_buffer(link, nb_samples);

    if (!ret)
        ret = ff_default_get_audio_buffer(link, nb_samples);

    return ret;
}

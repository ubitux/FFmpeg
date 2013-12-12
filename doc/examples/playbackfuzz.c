/*
 * Copyright (c) 2012 Stefano Sabatini
 * Copyright (c) 2013 Clément Bœsch
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#include <libavutil/imgutils.h>
#include <libavutil/timestamp.h>
#include <libavformat/avformat.h>
#include <libswresample/swresample.h>
#include <libswscale/swscale.h>

static AVFormatContext *fmt_ctx = NULL;
static AVCodecContext *video_dec_ctx = NULL, *audio_dec_ctx;
static AVStream *video_stream = NULL, *audio_stream = NULL;
static const char *src_filename = NULL;

static uint8_t *video_dst_data[4] = {NULL};
static int      video_dst_linesize[4];
static int video_dst_bufsize;

static int video_stream_idx = -1, audio_stream_idx = -1;
static AVFrame *frame = NULL;
static AVPacket pkt;
static int video_frame_count = 0;
static int audio_frame_count = 0;

static uint32_t seed_pos  = 0x012345;
static uint32_t seed_doit = 0x543210;
static const int max_seeks = 50;

static struct SwsContext *sws_ctx;
static struct SwrContext *swr_ctx;

static const int                 sws_w         = 320;
static const int                 sws_h         = 240;
static const enum AVPixelFormat  sws_pixfmt    = AV_PIX_FMT_RGB24;

static const int64_t             swr_chl       = AV_CH_LAYOUT_STEREO;
static const enum AVSampleFormat swr_samplefmt = AV_SAMPLE_FMT_S16P;
static const int                 swr_rate      = 44100;

static int decode_packet(int *got_frame, int cached)
{
    int ret = 0;
    int decoded = pkt.size;

    *got_frame = 0;

    if (pkt.stream_index == video_stream_idx) {
        ret = avcodec_decode_video2(video_dec_ctx, frame, got_frame, &pkt);
        if (ret < 0) {
            fprintf(stderr, "Error decoding video frame\n");
            return ret;
        }
        decoded = FFMIN(ret, pkt.size);

        if (*got_frame) {
            int64_t ts = av_frame_get_best_effort_timestamp(frame);
            ret = sws_scale(sws_ctx, (const uint8_t **)frame->data, frame->linesize,
                            0, frame->height, video_dst_data, video_dst_linesize);
            if (ret < 0) {
                fprintf(stderr, "Unable to scale frame\n");
                return ret;
            }
            printf("[V:%04d] [%s] decoded and scaled frame\n", video_frame_count++, av_ts2str(ts));

        }
    } else if (pkt.stream_index == audio_stream_idx) {
        ret = avcodec_decode_audio4(audio_dec_ctx, frame, got_frame, &pkt);
        if (ret < 0) {
            fprintf(stderr, "Error decoding audio frame\n");
            return ret;
        }
        decoded = FFMIN(ret, pkt.size);

        if (*got_frame) {
            uint8_t **output;
            int out_samples, nb_out;
            int64_t ts = av_frame_get_best_effort_timestamp(frame);

            out_samples = av_rescale_rnd(swr_get_delay(swr_ctx, swr_rate) + frame->nb_samples,
                                         frame->sample_rate, swr_rate, AV_ROUND_UP);
            ret = av_samples_alloc_array_and_samples(&output, NULL,
                                                     av_get_channel_layout_nb_channels(swr_chl),
                                                     out_samples, swr_samplefmt, 0);
            if (ret < 0) {
                fprintf(stderr, "Unable to allocate resampled buffer\n");
                return ret;
            }

            nb_out = swr_convert(swr_ctx, output, out_samples,
                                 (const unsigned char **)frame->extended_data,
                                 frame->nb_samples);
            printf("[A:%04d] [%s] decoded and converted %d to %d samples, got %d\n",
                   audio_frame_count++, av_ts2str(ts),
                   frame->nb_samples, out_samples, nb_out);

            av_freep(&output[0]);
            av_freep(&output);
        }
    }

    av_frame_unref(frame);

    return decoded;
}

static int open_codec_context(int *stream_idx,
                              AVFormatContext *fmt_ctx, enum AVMediaType type)
{
    int ret;
    AVStream *st;
    AVCodecContext *dec_ctx = NULL;
    AVCodec *dec = NULL;
    AVDictionary *opts = NULL;

    ret = av_find_best_stream(fmt_ctx, type, -1, -1, NULL, 0);
    if (ret < 0) {
        fprintf(stderr, "Could not find %s stream in input file '%s'\n",
                av_get_media_type_string(type), src_filename);
        return ret;
    } else {
        *stream_idx = ret;
        st = fmt_ctx->streams[*stream_idx];

        dec_ctx = st->codec;
        dec = avcodec_find_decoder(dec_ctx->codec_id);
        if (!dec) {
            fprintf(stderr, "Failed to find %s codec\n",
                    av_get_media_type_string(type));
            return ret;
        }

        av_dict_set(&opts, "refcounted_frames", "1", 0);
        if ((ret = avcodec_open2(dec_ctx, dec, &opts)) < 0) {
            fprintf(stderr, "Failed to open %s codec\n",
                    av_get_media_type_string(type));
            return ret;
        }
    }

    return 0;
}

int main (int argc, char **argv)
{
    int ret = 0, got_frame, nb_seek = 0;
    const char *src_filename;

    if (argc != 2) {
        printf("Usage: %s <file>\n", argv[0]);
        return 0;
    }
    src_filename = argv[1];

    av_register_all();

    if (avformat_open_input(&fmt_ctx, src_filename, NULL, NULL) < 0) {
        fprintf(stderr, "Could not open source file %s\n", src_filename);
        return 1;
    }

    if (avformat_find_stream_info(fmt_ctx, NULL) < 0) {
        fprintf(stderr, "Could not find stream information\n");
        return 1;
    }

    if (fmt_ctx->duration <= 0) {
        fprintf(stderr, "Format has no duration, can't randomly seek\n");
        return 1;
    }

    if (open_codec_context(&video_stream_idx, fmt_ctx, AVMEDIA_TYPE_VIDEO) >= 0) {
        video_stream = fmt_ctx->streams[video_stream_idx];
        video_dec_ctx = video_stream->codec;

        ret = av_image_alloc(video_dst_data, video_dst_linesize,
                             sws_w, sws_h, sws_pixfmt, 1);
        if (ret < 0) {
            fprintf(stderr, "Could not allocate raw video buffer\n");
            goto end;
        }
        video_dst_bufsize = ret;

        sws_ctx = sws_getContext(video_dec_ctx->width, video_dec_ctx->height,
                                 video_dec_ctx->pix_fmt,
                                 sws_w, sws_h, sws_pixfmt,
                                 SWS_BILINEAR, NULL, NULL, NULL);
        if (!sws_ctx) {
            fprintf(stderr, "Could not allocate scaling context\n");
            ret = AVERROR(EINVAL);
            goto end;
        }
    }

#if 0
    if (open_codec_context(&audio_stream_idx, fmt_ctx, AVMEDIA_TYPE_AUDIO) >= 0) {
        audio_stream = fmt_ctx->streams[audio_stream_idx];
        audio_dec_ctx = audio_stream->codec;

        swr_ctx = swr_alloc_set_opts(NULL, swr_chl, swr_samplefmt, swr_rate,
                                     audio_dec_ctx->channel_layout,
                                     audio_dec_ctx->sample_fmt,
                                     audio_dec_ctx->sample_rate,
                                     0, NULL);
        if (!swr_ctx) {
            fprintf(stderr, "Could not allocate resampler context\n");
            ret = AVERROR(ENOMEM);
            goto end;
        }

        if ((ret = swr_init(swr_ctx)) < 0) {
            fprintf(stderr, "Failed to initialize the resampling context\n");
            goto end;
        }
    }
#endif

    av_dump_format(fmt_ctx, 0, src_filename, 0);

    if (!audio_stream && !video_stream) {
        fprintf(stderr, "Could not find audio or video stream in the input, aborting\n");
        ret = 1;
        goto end;
    }

    frame = av_frame_alloc();
    if (!frame) {
        fprintf(stderr, "Could not allocate frame\n");
        ret = AVERROR(ENOMEM);
        goto end;
    }

    for (;;) {
        av_init_packet(&pkt);
        pkt.data = NULL;
        pkt.size = 0;

        while (av_read_frame(fmt_ctx, &pkt) >= 0) {
            AVPacket orig_pkt = pkt;
            do {
                seed_doit = seed_doit*1664525+1013904223;
                if (seed_doit < UINT32_MAX/100) {
                    int64_t ts, mints, maxts;
                    const char *direction;

                    if (nb_seek == max_seeks)
                        goto flush;
                    nb_seek++;

                    seed_pos = seed_pos*1664525+1013904223;
                    ts = seed_pos / (float)(1LL<<32) * fmt_ctx->duration;

                    if (seed_doit & 1) {
                        direction = "backward";
                        mints = INT64_MIN;
                        maxts = ts;
                    } else {
                        direction = "forward";
                        mints = ts;
                        maxts = INT64_MAX;
                    }
                    printf("seek (%d/%d) %s at ~%.1f%%\n", nb_seek, max_seeks,
                           direction, ts / (float)fmt_ctx->duration * 100.);
                    ret = avformat_seek_file(fmt_ctx, -1, mints, ts, maxts, 0);
                    if (ret < 0)
                        printf("seek failed (%s)\n", av_err2str(ret));
                    avcodec_flush_buffers(video_dec_ctx);

#if 0
                    if ((ret = swr_init(swr_ctx)) < 0) {
                        fprintf(stderr, "Failed to reset the resampling context\n");
                        goto end;
                    }
#endif

                    av_init_packet(&pkt);
                    pkt.data = NULL;
                    pkt.size = 0;
                    break;
                }
                ret = decode_packet(&got_frame, 0);
                if (ret < 0)
                    break;
                pkt.data += ret;
                pkt.size -= ret;
            } while (pkt.size > 0);
            av_free_packet(&orig_pkt);
        }

        printf(" ==> seek back to the beginning\n");
        ret = avformat_seek_file(fmt_ctx, -1, INT64_MIN, 0, INT64_MAX, 0);
        if (ret < 0) {
            printf("Unable to seek to start position\n");
            break;
        }
    }

flush:
    pkt.data = NULL;
    pkt.size = 0;
    do {
        decode_packet(&got_frame, 1);
    } while (got_frame);

end:
    swr_free(&swr_ctx);
    avcodec_close(video_dec_ctx);
    avcodec_close(audio_dec_ctx);
    avformat_close_input(&fmt_ctx);
    av_frame_free(&frame);
    av_free(video_dst_data[0]);

    return ret < 0;
}

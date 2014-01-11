#include <libavutil/timestamp.h>
#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>

static AVFrame *outframe;
static AVCodec *encoder;

static int decode_packet(AVCodecContext *avctx, AVPacket *pkt,
                         int *got_output, const char *prefix)
{
    int i, size;
    AVSubtitle subtitle;

    *got_output = 0;

    size = avcodec_decode_subtitle2(avctx, &subtitle, got_output, pkt);
    if (size < 0 || !*got_output)
        return size;

    printf("sub @ %s: [%u->%u] [%d rects]\n",
           av_ts2str(subtitle.pts),
           subtitle.start_display_time, subtitle.end_display_time,
           subtitle.num_rects);

    for (i = 0; i < subtitle.num_rects; i++) {
        int ret;
        AVPacket pkt2 = {0};
        AVFormatContext *fmt = NULL;
        char fname[256];
        AVSubtitleRect *rect = subtitle.rects[i];
        int got_packet = 0;
        AVStream *st;

        outframe->width  = rect->w;
        outframe->height = rect->h;
        memcpy(outframe, &rect->pict, sizeof(AVPicture));

        snprintf(fname, sizeof(fname), "%s-st%d-ts%s-rect%d-%dx%d-X%d-Y%d.png",
                 prefix, pkt->stream_index, av_ts2str(subtitle.pts), i,
                 rect->w, rect->h, rect->x, rect->y);

        printf("saving %s\n", fname);

        ret = avformat_alloc_output_context2(&fmt, NULL, "image2", fname);
        if (ret < 0)
            return ret;

        st = avformat_new_stream(fmt, encoder);
        if (!st) {
            avformat_free_context(fmt);
            return AVERROR(ENOMEM);
        }
        st->codec->width  = rect->w;
        st->codec->height = rect->h;
        st->codec->pix_fmt = outframe->format;
        st->codec->time_base.den = 25;
        st->codec->time_base.num = 1;
        ret = avcodec_open2(st->codec, encoder, NULL);
        if (ret < 0) {
            fprintf(stderr, "Could not open PNG codec: %s\n", av_err2str(ret));
            goto end;
        }

        ret = avcodec_encode_video2(st->codec, &pkt2, outframe, &got_packet);
        if (ret < 0) {
            fprintf(stderr, "encode fails %s\n", av_err2str(ret));
            goto end;
        }

        if (got_packet) {
            if ((ret = avformat_write_header(fmt, NULL))       < 0 ||
                (ret = av_interleaved_write_frame(fmt, &pkt2)) < 0 ||
                (ret = av_write_trailer(fmt))                  < 0) {
                goto end;
            }
        }

end:
        av_free_packet(&pkt2);
        avcodec_close(st->codec);
        avformat_free_context(fmt);
        if (ret < 0) {
            size = ret;
            break;
        }
    }

    avsubtitle_free(&subtitle);
    return size;
}

static AVCodecContext *get_avctx(AVStream **subst, int nb_subst, AVPacket *pkt)
{
    int i;

    for (i = 0; i < nb_subst; i++)
        if (subst[i]->index == pkt->stream_index)
            return subst[i]->codec;
    return NULL;
}

int main (int ac, char **av)
{
    AVPacket pkt;
    AVFormatContext *fmt_ctx = NULL;
    AVCodec *dec = NULL;
    int ret = 0, got_frame, i;
    AVStream *subst[256];
    int nb_subst = 0;

    if (ac != 3) {
        fprintf(stderr, "Usage: %s <input> <output-prefix>\n", av[0]);
        return 0;
    }

    av_register_all();

    ret = avformat_open_input(&fmt_ctx, av[1], NULL, NULL);
    if (ret < 0) {
        fprintf(stderr, "Unable to open '%s': %s\n", av[1], av_err2str(ret));
        return 1;
    }
    ret = avformat_find_stream_info(fmt_ctx, NULL);
    if (ret < 0) {
        fprintf(stderr, "Unable to find stream information: %s\n", av_err2str(ret));
        return 1;
    }
    dec = avcodec_find_decoder(AV_CODEC_ID_DVB_SUBTITLE);
    if (!dec) {
        fprintf(stderr, "FFmpeg is not built with DVB subtitle decoding\n");
        return 1;
    }
    if (dec->capabilities & CODEC_CAP_DELAY)
        fprintf(stderr, "warning: flushing will not happen\n");

    //av_dump_format(fmt_ctx, 0, av[1], 0);

    for (i = 0; i < fmt_ctx->nb_streams; i++) {
        AVStream *st = fmt_ctx->streams[i];
        if (st->codec->codec_id != AV_CODEC_ID_DVB_SUBTITLE)
            continue;
        ret = avcodec_open2(st->codec, dec, NULL);
        if (ret < 0) {
            fprintf(stderr, "Failed to open codec: %s\n", av_err2str(ret));
            return 1;
        }
        subst[nb_subst++] = st;
    }

    printf("%d DVB sub stream%s detected\n",
           nb_subst, nb_subst > 1 ? "s" : "");
    if (!nb_subst)
        return 0;

    encoder = avcodec_find_encoder(AV_CODEC_ID_PNG);
    if (!encoder) {
        fprintf(stderr, "Could not find PNG encoder\n");
        return 1;
    }

    outframe = av_frame_alloc();
    if (!outframe)
        return 1;
    outframe->format = AV_PIX_FMT_PAL8;

    av_init_packet(&pkt);
    pkt.data = NULL;
    pkt.size = 0;

    while (av_read_frame(fmt_ctx, &pkt) >= 0) {
        AVPacket orig_pkt = pkt;
        AVCodecContext *avctx = get_avctx(subst, nb_subst, &pkt);
        if (avctx) {
            do {
                ret = decode_packet(avctx, &pkt, &got_frame, av[2]);
                if (ret < 0)
                    break;
                pkt.data += ret;
                pkt.size -= ret;
            } while (pkt.size > 0);
        }
        av_free_packet(&orig_pkt);
    }

    av_frame_free(&outframe);

    for (i = 0; i < nb_subst; i++)
        avcodec_close(subst[i]->codec);
    avformat_close_input(&fmt_ctx);
    return 0;
}

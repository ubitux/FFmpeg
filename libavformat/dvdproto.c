/*
 * Copyright (c) 2009 Erik Van Grunderbeeck <erik <at> arawix.com>
 * Copyright (c) 2012 Stefano Sabatini
 *
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

#include <dvdnav/dvdnav.h>
#include <dvdnav/dvd_types.h>

#define DEBUG 1

#include "libavutil/avstring.h"
#include "libavutil/bswap.h"
#include "libavformat/avformat.h"
#include "libavformat/avio.h"
#include "libavformat/url.h"

#define	DVD_PROTO_PREFIX "dvd:"

#define	MAX_ATTACH_AUDIO_LANGUAGES	8
#define	MAX_ATTACH_BUTTONS             36
#define	MAX_ATTACH_SUB_LANGUAGES       32

typedef enum {
    DVDEffectUnknown,
    DVDEffectButtons,
    DVDEffectButtonsHighlight,
    DVDEffectWait,
    DVDEffectCLUT,
    DVDEffectClearQueue,
    DVDEffectResetIFO,
    DVDEffectClearNOP,
} DVDEffectType;

/* contains the map the dvd, buffer and event data */
typedef struct {
    dvdnav_t   *nav_data;
    uint8_t     cache_buf[DVD_VIDEO_LB_LEN];
    uint8_t    *read_buf;
    uint8_t    *next_buf;
    int         read_buf_left;
    char *language;
} DVDContext;

typedef struct {
    int x, y, w, h;
} DVDAttachmentButton;

typedef struct {
    int64_t pts;     ///< pts of this context
    uint16_t type;
    uint16_t wait;

    //	when buttons
    uint16_t highlight_index;
    uint16_t button_count;
    DVDAttachmentButton buttons[MAX_ATTACH_BUTTONS];

    //	when color-palette
    uint32_t rgba_palette[16];

    //	expected size of the video pictures (allows for skip of scan-stream)
    uint16_t video_width, video_height;
    int8_t video_aspect;                 ///< 0 means 4:3, 2 means 16:9

    uint8_t current_vts;
    uint8_t nb_audio_languages;
    uint8_t nb_subtitle_languages;

    uint16_t audio_languages   [MAX_ATTACH_AUDIO_LANGUAGES];
    uint16_t audio_flags       [MAX_ATTACH_AUDIO_LANGUAGES];
    uint8_t  audio_channels    [MAX_ATTACH_AUDIO_LANGUAGES];
    uint8_t  audio_mode        [MAX_ATTACH_AUDIO_LANGUAGES];
    uint16_t subtitle_languages[MAX_ATTACH_SUB_LANGUAGES];
    uint16_t subtitle_flags    [MAX_ATTACH_SUB_LANGUAGES];

    // when angle change
    uint8_t current_angle;
    uint8_t max_angle;

    // size of current title in pts ticks. divide by 90000 to get time in seconds
    uint64_t duration;

    // flags that describe actions allowed for current chapter
    uint32_t flags;
} DVDParseContext;

/* use libdvdnav's read ahead cache? */
#define DVD_READ_CACHE 0

/* which is the default language for menus/audio/subpictures? */
#define DVD_LANGUAGE "en"

/*
 * Send protocol data back to the client on the callback.
 *
 * The pts time in the structure is the time the client is supposed to
 * handle the callback message.
 * This is done separately, not in the stream since the package manager
 * in libav* expects a stream that is a constant state machine, where
 * more packages are never inserted back in time. We can actually do
 * that, since after a seek things like subtitle, and cell change
 * information will be sent.
 */
static void dvd_protocol_send(URLContext *h, DVDParseContext *pctx)
{
    /* if (h->ff_protocol_cb) { */
    /*     (h->ff_protocol_cb)(pctx, h->ff_cb_userdata); */
    /* } */
}

static int dvd_protocol_build_packet_reset(URLContext *h, DVDContext *dvd,
                                           dvdnav_vts_change_event_t *event)
{
    int i, count;
    int32_t title, part;
    uint64_t *times;
    dvdnav_status_t status;
    audio_attr_t audio_attr;
    subp_attr_t subp_attr;
    DVDParseContext *pctx;
    int32_t current_angle, max_angle;

    if (event->new_domain == DVD_READ_TITLE_VOBS) {
        /* In general, this signals that we should re-read the
         * language and sub-title information from the DVD. This is
         * the best method I could find, since not all DVDs send an
         * IFO change domain (usually just sent at beginning).
         */
        pctx = (DVDParseContext *)av_mallocz(sizeof(DVDParseContext));
        if (!pctx)
            return AVERROR(ENOMEM);

        pctx->type = DVDEffectResetIFO;
        pctx->pts = dvdnav_get_current_time(dvd->nav_data);

        /* dvdnav_get_video_resolution(dvd->nav_data, pctx->video_width, pctx->video_height); */
        pctx->video_aspect = dvdnav_get_video_aspect(dvd->nav_data);

        // store vts index
        pctx->current_vts = (uint8_t)event->new_vtsN;

        /* todo: add protocol options to list contained list of things */
        // get all audio languages, theres always max 8
        memset(&pctx->audio_languages, 0, sizeof(uint16_t)*MAX_ATTACH_AUDIO_LANGUAGES);
        memset(&pctx->audio_flags    , 0, sizeof(uint16_t)*MAX_ATTACH_AUDIO_LANGUAGES);
        for (i = 0; i < MAX_ATTACH_AUDIO_LANGUAGES && status == DVDNAV_STATUS_OK; i++) {
            status = dvdnav_get_audio_attr(dvd->nav_data, i, &audio_attr);

            pctx->audio_languages[i] = av_bswap16(audio_attr.lang_code);
            pctx->audio_flags    [i] = audio_attr.code_extension;
            pctx->audio_channels [i] = audio_attr.channels;
            pctx->audio_mode     [i] = audio_attr.application_mode;
            if (!audio_attr.lang_code)
                break;
        }
        pctx->nb_audio_languages = i;

        count = 0;
        memset(&pctx->subtitle_languages, 0, sizeof(uint16_t)*MAX_ATTACH_SUB_LANGUAGES);
        memset(&pctx->subtitle_languages, 0, sizeof(uint16_t)*MAX_ATTACH_SUB_LANGUAGES);
        for (i = 0; i < MAX_ATTACH_SUB_LANGUAGES && status == DVDNAV_STATUS_OK; i++) {
            // get the attributes, make sure we dont get the same stuff twice (seen that happen)
            status = dvdnav_get_spu_attr(dvd->nav_data, i, &subp_attr);

            pctx->subtitle_languages[count] = av_bswap16(subp_attr.lang_code);
            /* from the documentation at http://dvd.sourceforge.net/dvdinfo/sprm.html
               1=normal, 2=large, 3=children, 5=normal captions, 6=large captions, 7=childrens captions,
               9=forced, 13=director comments, 14=large director comments, 15=director comments for children */
            pctx->subtitle_flags[i] = subp_attr.code_extension;
            if (!subp_attr.lang_code)
                break;
            ++count;
        }
        pctx->nb_subtitle_languages = count;

        // get time of vobs. might be a better way?
        title = 0;
        part  = 0;
        times = NULL;
        if (dvdnav_current_title_info(dvd->nav_data, &title, &part) == DVDNAV_STATUS_OK) {
            dvdnav_describe_title_chapters(dvd->nav_data, title, &times, &pctx->duration);
            av_freep(&times);
        }

        // get angle info
        current_angle = max_angle = 0;
        if (dvdnav_get_angle_info(dvd->nav_data, &current_angle, &max_angle) == DVDNAV_STATUS_OK) {
            pctx->current_angle = (uint8_t)current_angle;
            pctx->max_angle     = (uint8_t)max_angle;
        }

        dvd_protocol_send(h, pctx);
        return 1;
    }

    return 0;
}

/**
 * Build a navigation packet signaling new button selection data.
 */
static int dvd_protocol_build_packet_nav(URLContext *h, DVDContext *dvd)
{
    DVDParseContext *pctx;
    pci_t *pci = dvdnav_get_current_nav_pci(dvd->nav_data);
    int i;
    int button_count = pci->hli.hl_gi.btn_ns;
    if (button_count == 0)
        return 0;

    pctx = (DVDParseContext *)av_mallocz(sizeof(DVDParseContext));
    if (!pctx)
        return AVERROR(ENOMEM);

    pctx->type = DVDEffectButtons;
    pctx->pts = dvdnav_get_current_time(dvd->nav_data);
    pctx->button_count = button_count;
    pctx->button_count = FFMIN(pctx->button_count, MAX_ATTACH_BUTTONS);

    for (i = 0; i < button_count; i++) {
        btni_t *btni = &(pci->hli.btnit[i]);
        pctx->buttons[i] = (DVDAttachmentButton) { .x = btni->x_start,
                                                   .y = btni->y_start,
                                                   .w = btni->x_end - btni->x_start,
                                                   .h = btni->y_end - btni->y_start };
    }

    dvd_protocol_send(h, pctx);
    return 1;
}

/**
 * Build a navigation packet signaling new button highlight data.
 */
static int dvd_protocol_build_packet_highlight(URLContext *h, DVDContext *dvd,
                                               uint32_t index)
{
    DVDParseContext *pctx = (DVDParseContext *)av_mallocz(sizeof(DVDParseContext));
    if (!pctx)
        return AVERROR(ENOMEM);

    pctx->type = DVDEffectButtonsHighlight;
    pctx->pts = dvdnav_get_current_time(dvd->nav_data);
    pctx->highlight_index = index > 0 ? index-1 : 0;

    pctx->buttons[0] = (DVDAttachmentButton){0};
    dvd_protocol_send(h, pctx);

    return 1;
}

/**
 * Build a navigation packet signaling a new color table.
 */
static int dvd_protocol_build_packet_clut(URLContext *h, DVDContext *dvd, uint32_t *data_color)
{
    DVDParseContext *pctx = (DVDParseContext *)av_mallocz(sizeof(DVDParseContext));
    if (!pctx)
        return AVERROR(ENOMEM);

    pctx->type = DVDEffectCLUT;
    pctx->pts = dvdnav_get_current_time(dvd->nav_data);
    memcpy(pctx->rgba_palette, data_color, 16 * sizeof(uint32_t));
    dvd_protocol_send(h, pctx);

    return 1;
}

/**
 * Build a navigation packet signaling a wait (e.g. on still).
 */
static int dvd_protocol_build_packet_wait(URLContext *h, DVDContext *dvd, uint32_t time)
{
    DVDParseContext *pctx = (DVDParseContext *)av_mallocz(sizeof(DVDParseContext));
    if (!pctx)
        return AVERROR(ENOMEM);

    pctx->type = DVDEffectWait;
    pctx->pts = dvdnav_get_current_time(dvd->nav_data);
    pctx->wait = time;
    dvd_protocol_send(h, pctx);
    return 1;
}

static void identify_chapters(URLContext *h, uint32_t title)
{
    DVDContext *dvd;
    uint64_t *parts = NULL, duration = 0;
    uint32_t n, i, t;

    if (!h || !h->priv_data)
        return;
    dvd = (DVDContext *)(h->priv_data);

    n = dvdnav_describe_title_chapters(dvd->nav_data, title, &parts, &duration);
    if (parts) {
        t = duration / 90;
        av_log(h, AV_LOG_INFO, "ID_DVD_TITLE_%d_LENGTH=%d.%03d\n", title, t / 1000, t % 1000);
        av_log(h, AV_LOG_INFO, "ID_DVD_TITLE_%d_CHAPTERS=%d\n", title, n);
        av_log(h, AV_LOG_INFO, "TITLE %u, CHAPTERS: ", title);

        for (i = 0; i < n; i++) {
            t = parts[i] /  90000;
            av_log(h, AV_LOG_INFO, "%02d:%02d:%02d,", t/3600, (t/60)%60, t%60);
        }
        av_free(parts);
        av_log(h, AV_LOG_INFO, "\n");
    }
}

static int dvd_open(URLContext *h, const char *filename, int flags)
{
    DVDContext *dvd;

    const char *diskname = filename;
    av_strstart(filename, DVD_PROTO_PREFIX, &diskname);

    if (!(dvd = av_mallocz(sizeof(DVDContext))))
        return AVERROR(ENOMEM);

    dvd->language = av_strdup(DVD_LANGUAGE);
    dvd->read_buf = dvd->cache_buf;

    if (dvdnav_open(&dvd->nav_data, diskname) != DVDNAV_STATUS_OK) {
        av_freep(&dvd);
        return AVERROR(EIO);
    }

    /* set read ahead cache usage */
    if (dvdnav_set_readahead_flag(dvd->nav_data, DVD_READ_CACHE) != DVDNAV_STATUS_OK) {
        dvdnav_close(dvd->nav_data);
        av_freep(&dvd);
        return AVERROR(EACCES);
    }

    /* set the language */
    if ((dvdnav_menu_language_select (dvd->nav_data, dvd->language) != DVDNAV_STATUS_OK)  ||
        (dvdnav_audio_language_select(dvd->nav_data, dvd->language) != DVDNAV_STATUS_OK)  ||
        (dvdnav_spu_language_select  (dvd->nav_data, dvd->language) != DVDNAV_STATUS_OK)) {
        av_log(h, AV_LOG_ERROR, "Error selecting language '%s': %s\n",
               dvd->language, dvdnav_err_to_string(dvd->nav_data));
        dvdnav_close(dvd->nav_data);
        av_freep(&dvd);
        return AVERROR(EACCES);
    }

    /* set the PGC positioning flag to have position information relatively to the
     * current chapter (seek will seek in the chapter) */
    if (dvdnav_set_PGC_positioning_flag(dvd->nav_data, 0) != DVDNAV_STATUS_OK) {
        av_log(h, AV_LOG_ERROR, "Error setting PGC positioning flags: %s\n",
               dvdnav_err_to_string(dvd->nav_data));
        dvdnav_close(dvd->nav_data);
        av_freep(&dvd);
        return AVERROR(EACCES);
    }

    h->priv_data = dvd;
    identify_chapters(h, 0);
    return 0;
}

static int dvd_close(URLContext *h)
{
    DVDContext *dvd = (DVDContext *)h->priv_data;
    if (dvd) {
        av_freep(&dvd->language);
        dvdnav_close(dvd->nav_data);
    }

    return 0;
}

static int dvd_read(URLContext *h, unsigned char *buf_out, int size)
{
    DVDContext *dvd = (DVDContext *)h->priv_data;
    dvdnav_still_event_t *still_event;
    int res, event, read_len, min_len_read;

    if (!dvd)
        return AVERROR(EFAULT);

    if (dvd->read_buf_left > 0) {
        min_len_read = FFMIN(dvd->read_buf_left, size);
        memcpy(buf_out, dvd->next_buf, min_len_read);
        dvd->read_buf_left -= min_len_read;
        dvd->next_buf      += min_len_read;
        return min_len_read;

    } else if (dvd->next_buf) {
#ifdef DVD_READ_CACHE
        dvdnav_free_cache_block(dvd->nav_data, dvd->read_buf);
#endif
        dvd->next_buf = NULL;
    }

reread_block:
    read_len = 0;
    event = 0;
#ifdef DVD_READ_CACHE
    res = dvdnav_get_next_cache_block(dvd->nav_data, &dvd->read_buf, &event, &read_len);
#else
    res = dvdnav_get_next_block(dvd->nav_data, dvd->read_buf, &event, &read_len);
#endif
    if (res == DVDNAV_STATUS_ERR)
        return AVERROR(EIO);

    switch (event) {
    case DVDNAV_BLOCK_OK:
        dvd->next_buf = dvd->read_buf;
        dvd->read_buf_left += read_len;
        min_len_read = FFMIN(dvd->read_buf_left, size);
        memcpy(buf_out, dvd->next_buf, min_len_read);
        dvd->read_buf_left -= min_len_read;
        dvd->next_buf      += min_len_read;
        av_dlog(h, "dvd_read: DVDNAV_BLOCK_OK -> read len %d\n", min_len_read);
        return min_len_read;
        break;

    case DVDNAV_NAV_PACKET:
        av_dlog(h, "dvd_read: DVDNAV_NAV_PACKET\n");
        dvd_protocol_build_packet_nav(h, dvd);
#ifdef DVD_READ_CACHE
        dvdnav_free_cache_block(dvd->nav_data, dvd->read_buf);
#endif
        goto reread_block;
        break;

    case DVDNAV_HIGHLIGHT:
    {
        dvdnav_highlight_event_t *highlight_event;

        av_dlog(h, "dvd_read: DVDNAV_HIGHLIGTH\n");
        highlight_event = (dvdnav_highlight_event_t *)dvd->read_buf;
        dvd_protocol_build_packet_highlight(h, dvd, (uint32_t)highlight_event->buttonN);
#ifdef DVD_READ_CACHE
        dvdnav_free_cache_block(dvd->nav_data, dvd->read_buf);
#endif
        goto reread_block;
        break;
    }

    case DVDNAV_SPU_CLUT_CHANGE:
        av_dlog(h, "dvd_read: DVDNAV_SPU_CLUT_CHANGE\n");
        dvd_protocol_build_packet_clut(h, dvd, (uint32_t *)dvd->read_buf);
#ifdef DVD_READ_CACHE
        dvdnav_free_cache_block(dvd->nav_data, dvd->read_buf);
#endif
        goto reread_block;
        break;

    case DVDNAV_STILL_FRAME:
        av_dlog(h, "dvd_read: DVDNAV_SPU_STILL_FRAME\n");
        /* We have reached a still frame. Wait the amount of time
         * specified by the still's length while still handling user
         * input to make menus and other interactive stills work.  A
         * length of 0xff means an indefinite still which has to be
         * skipped indirectly by some user interaction. */
        dvdnav_still_skip(dvd->nav_data);

        still_event = (dvdnav_still_event_t *)dvd->read_buf;
        dvd_protocol_build_packet_wait(h, dvd, still_event->length);
#ifdef DVD_READ_CACHE
        dvdnav_free_cache_block(dvd->nav_data, dvd->read_buf);
#endif
        goto reread_block;
        break;

    case DVDNAV_WAIT:
        av_dlog(h, "dvd_read: DVDNAV_WAIT\n");
        /* We have reached a point in DVD playback, where timing is
         * critical.  Player application with internal fifos can
         * introduce state inconsistencies, because libdvdnav is
         * always the fifo's length ahead in the stream compared to
         * what the application sees.  Such applications should wait
         * until their fifos are empty when they receive this type of
         * event. */
        dvd_protocol_build_packet_wait(h, dvd, 0);
#ifdef DVD_READ_CACHE
        dvdnav_free_cache_block(dvd->nav_data, dvd->read_buf);
#endif
        goto reread_block;
        break;

    case DVDNAV_VTS_CHANGE:
    {
        dvdnav_vts_change_event_t *vts_event;

        av_dlog(h, "dvd_read: DVDNAV_VTS_CHANGE\n");
        /* Some status information like video aspect and video scale
         * permissions do not change inside a VTS. Therefore this
         * event can be used to query such information only when
         * necessary and update the decoding/displaying accordingly.
         * We will send information if a VTS change happens, so the
         * end-user can re-parse any IFO files if needed for language
         * and sub-title information */
        vts_event = (dvdnav_vts_change_event_t *)dvd->read_buf;
        dvd_protocol_build_packet_reset(h, dvd, vts_event);
#ifdef DVD_READ_CACHE
        dvdnav_free_cache_block(dvd->nav_data, dvd->read_buf);
#endif
        goto reread_block;
        break;
    }

    case DVDNAV_CELL_CHANGE:
        av_dlog(h, "dvd_read: DVDNAV_CELL_CHANGE\n");
        /* Some status information like the current Title and Part
         * numbers do not change inside a cell. Therefore this event
         * can be used to query such information only when necessary
         * and update the decoding/displaying accordingly. */
#ifdef DVD_READ_CACHE
        dvdnav_free_cache_block(dvd->nav_data, dvd->read_buf);
#endif
        goto reread_block;
        break;

    case DVDNAV_HOP_CHANNEL:
        av_dlog(h, "dvd_read: DVDNAV_HOP_CHANNEL\n");
        /* This event is issued whenever a non-seamless operation has
         * been executed.  Applications with fifos should drop the
         * fifos content to speed up responsiveness.  this is usual
         * send after a search, or when the dvd vm machine jumped
         * cells.  In general this also means drop all queues
         * (e.g. subtitle being displayed with no timeout, and close
         * audio channels for a switch).  We will not send this, as
         * the packet queue is then not deterministic anymore
         * e.g. after a seek below, the result will be a packet like
         * this inserted, that messes up the queue. */
#ifdef DVD_READ_CACHE
        dvdnav_free_cache_block(dvd->nav_data, dvd->read_buf);
#endif
        goto reread_block;
        break;

    case DVDNAV_SPU_STREAM_CHANGE:
        av_dlog(h, "dvd_read: DVDNAV_SPU_STREAM_CHANGE\n");
        // send information to switch audio channels
#ifdef DVD_READ_CACHE
        dvdnav_free_cache_block(dvd->nav_data, dvd->read_buf);
#endif
        goto reread_block;
        break;

    case DVDNAV_AUDIO_STREAM_CHANGE:
        av_dlog(h, "dvd_read: DVDNAV_AUDIO_STREAM_CHANGE\n");
        // send information to switch audio channels
#ifdef DVD_READ_CACHE
        dvdnav_free_cache_block(dvd->nav_data, dvd->read_buf);
#endif
        goto reread_block;
        break;

    case DVDNAV_NOP:
        av_dlog(h, "dvd_read: DVDNAV_NOP\n");
#ifdef DVD_READ_CACHE
        dvdnav_free_cache_block(dvd->nav_data, dvd->read_buf);
#endif
        goto reread_block;
        break;

    case DVDNAV_STOP:
        av_dlog(h, "dvd_read: DVDNAV_STOP\n");
    default:
        /* Playback should end here. */
        break;
    }

    // we stop
    if (dvd->next_buf) {
#ifdef DVD_READ_CACHE
        dvdnav_free_cache_block(dvd->nav_data, dvd->read_buf);
#endif
        dvd->next_buf = NULL;
    }

    return 0;
}

/**
 * Seek in the DVD. Only used for chapters seek, not seeking in the
 * VOB (PTS seek) itself.
 */
static int64_t dvd_read_seek(URLContext *h, int stream_index, int64_t pos, int flags)
{
    av_unused dvdnav_status_t res;
    av_unused int64_t loop;
    DVDContext *dvd = (DVDContext *)h->priv_data;

    if (!dvd)
        return AVERROR(EFAULT);

    if ((flags & (AVSEEK_FLAG_CHAPTER|AVSEEK_FLAG_BACKWARD)) == (AVSEEK_FLAG_CHAPTER|AVSEEK_FLAG_BACKWARD)) {
        for (loop = 0; loop < pos; loop++)
            res = dvdnav_prev_pg_search(dvd->nav_data);
    } else if ((flags & AVSEEK_FLAG_CHAPTER) == AVSEEK_FLAG_CHAPTER) {
        for (loop = 0; loop < pos; loop++)
            res = dvdnav_next_pg_search(dvd->nav_data);
    } else if (flags & AVSEEK_FLAG_DIRECT) {
        // time 90000 to dvd time
        res = dvdnav_time_search(dvd->nav_data, pos * 90000);
    }

    return 0;
}

#define BLOCK_SIZE 2048

/**
 * Seek in the DVD. Used for seeking in the VOB itself.
 */
static int64_t dvd_seek(URLContext *h, int64_t pos, int whence)
{
    DVDContext *dvd = (DVDContext *)h->priv_data;
    dvdnav_status_t res;
    uint32_t block_pos, len;

    if (!dvd)
        return AVERROR(EFAULT);

    /* get current position */
    res = dvdnav_get_position(dvd->nav_data, &block_pos, &len);
    if (res != DVDNAV_STATUS_OK)
        return AVERROR(EIO);

    switch (whence) {
    case SEEK_SET:
    case SEEK_CUR:
    case SEEK_END:
        if (whence == SEEK_END && block_pos == -1)
            return len * BLOCK_SIZE;
        res = dvdnav_sector_search(dvd->nav_data, pos / BLOCK_SIZE, whence);
        return pos;
        break;

    case AVSEEK_SIZE:
        return len * BLOCK_SIZE;

    case AVSEEK_CHAPTER:
        /* set the PGC positioning flag to have position information
         * relatively to whole dvd */
        if (dvdnav_set_PGC_positioning_flag(dvd->nav_data, 1) != DVDNAV_STATUS_OK)
            return AVERROR(EIO);

        res = dvdnav_sector_search(dvd->nav_data, pos, SEEK_CUR);
        /* set the PGC positioning flag to have position information
         * relatively to whole chapter */
        if (dvdnav_set_PGC_positioning_flag(dvd->nav_data, 0) != DVDNAV_STATUS_OK)
            return AVERROR(EIO);
    }

    av_log(h, AV_LOG_ERROR, "Unsupported whence operation %d\n", whence);
    return AVERROR(EINVAL);
}


URLProtocol ff_dvd_protocol = {
    .name                = "dvd",
    .url_close           = dvd_close,
    .url_open            = dvd_open,
    .url_read            = dvd_read,
    .url_read_seek       = dvd_read_seek,
    .url_seek            = dvd_seek,
    .priv_data_size      = sizeof(DVDContext),
};

/*
 * Copyright (c) 2000 Fabrice Bellard
 * Copyright (c) 2003 Tinic Uro
 * Copyright (c) 2013 Clément Bœsch
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

#define DEBUG

#include "libavutil/avassert.h"
#include "libavutil/channel_layout.h"
#include "libavutil/imgutils.h"
#include "libavutil/intreadwrite.h"
#include "libavutil/bprint.h"
#include "libswscale/swscale.h"
#include "swf.h"

enum spread_mode {
    SPREAD_MODE_PAD     = 0,
    SPREAD_MODE_REFLECT = 1,
    SPREAD_MODE_REPEAT  = 2,
};

enum interp_mode {
    INTERP_MODE_NORMAL = 0,
    INTERP_MODE_LINEAR = 1,
};

struct grad_record {
    uint8_t ratio;
    uint8_t rgba[4];
};

struct gradient {
    enum spread_mode spread;
    enum interp_mode interpolation;
    int nb_gradients;
    struct grad_record records[16];
    uint16_t focal_point; // focal gradient only
};

enum fillstyletype {
    FILL_SOLID                      = 0x00,
    FILL_LINEAR_GRADIANT            = 0x10,
    FILL_RADIAL_GRADIANT            = 0x12,
    FILL_FOCAL_RADIAL_GRADIANT      = 0x13,
    FILL_REPEATING_BITMAP           = 0x40,
    FILL_CLIPPED_BITMAP             = 0x41,
    FILL_NOSMOOTH_REPEATING_BITMAP  = 0x42,
    FILL_NOSMOOTH_CLIPPED_BITMAP    = 0x43,
};

struct matrix {
    int scale:1;
    int rotate:1;
    uint32_t scale_x, scale_y;
    uint32_t rotate_skew_0, rotate_skew_1;
    int translate_x, translate_y;
};

struct cxform {
    int add_terms:1;
    int mul_terms:1;
    int is_alpha:1;
    int addr, addg, addb, adda;
    int mulr, mulg, mulb, mula;
};

struct fillstyle {
    enum fillstyletype type;
    uint8_t rgba[4];
    struct matrix matrix; // bitmap or gradient matrix according to the container
    struct gradient gradient;
    uint16_t bitmap_id;
};

struct fillstylearray {
    int style_count;
    struct fillstyle *styles;
};

enum cap_style {
    CAP_ROUND  = 0,
    CAP_NONE   = 1,
    CAP_SQUARE = 2,
};

enum join_style {
    JOIN_ROUND = 0,
    JOIN_BEVEL = 1,
    JOIN_MITER = 2,
};

enum linestyle_flags {
    LSTL_HASFILL   = 1<<3,
    LSTL_NOHSCALE  = 1<<2,
    LSTL_NOVSCALE  = 1<<1,
    LSTL_PIXELHINT = 1<<0,
};

struct linestyle {
    uint16_t width; // in twips
    uint8_t rgba[4];

    /* linestyle2 only */
    enum cap_style cap_style;
    enum cap_style end_cap_style;
    enum join_style join_style;
    uint8_t flags:4;
    uint8_t noclose:1;
    uint16_t miter_limit_factor;
    struct fillstyle fillstyle;
};

struct linestylearray {
    int style_count;
    struct linestyle *styles;
};

enum state_style {
    SS_MOVETO     = 1<<0,
    SS_FILLSTYLE0 = 1<<1,
    SS_FILLSTYLE1 = 1<<2,
    SS_LINESTYLE  = 1<<3,
    SS_NEWSTYLES  = 1<<4,
};

struct shape_record {
    int is_edge:1;

    /* edge */
    int straight:1;
    int general_line:1;
    int vertical_line:1;
    int delta_x, delta_y;

    /* style change */
    int style_change;
    int move_delta_x, move_delta_y;
    int fill_style_0, fill_style_1;
    int line_style;
    struct fillstylearray fillstyles;
    struct linestylearray linestyles;
};

struct shape {
    struct shape_record *records;
    int nb_records;

    /* ShapeWithStyle only */
    int has_style;
    struct fillstylearray fillstyles;
    struct linestylearray linestyles;
};

struct rect { // in twips (1/20)
    int xmin, xmax;
    int ymin, ymax;
};

struct lossless_bitmap_char { // TAG_DEFINEBITSLOSSLESS*
    uint8_t *data[4];
    int linesizes[4];
    enum AVPixelFormat pix_fmt;
    int w, h;
};

struct audio_def { // TAG_DEFINESOUND
    uint8_t *data;
    int nb_samples;
    int skip_samples;
    int size;
};

struct lossy_char {
    uint8_t *data;
    uint8_t *alpha;
    int size;
    int alpha_size;
};

struct shape_char { // TAG_DEFINESHAPE*
    int id;
    struct rect bounds;
    struct shape data;
};

struct swf_dict_entry {
    int tag_id;
    void *content;
};

struct swf_disp_entry {
    uint8_t flags;
    int ch_id;
    struct matrix matrix;
    struct cxform cxform;
    int ratio;
    char *name;
    int clip_depth;
    //TODO: clipaction
};

struct audio_specs {
    enum AVCodecID codec_id;
    int rate;
    int64_t channel_layout;
    int size;
};

struct swftag;

struct tag_prop {
    const char *name;
    int (*handle)(AVFormatContext *s, AVIOContext *pb, AVPacket *pkt,
                  const struct swftag *tag);
};

struct swftag {
    int id;
    int len;
    int version;
    const struct tag_prop *prop;
};

typedef struct {
    int version;

#if CONFIG_ZLIB
    AVIOContext *zpb;
#define ZBUF_SIZE 4096
    uint8_t *zbuf_in;
    uint8_t *zbuf_out;
    z_stream zstream;
#endif
    uint8_t background[3];

    struct swf_dict_entry *dict;
    int dict_nb_entries;

    struct swf_disp_entry *display_list;
    int disp_nb_entries;

    int video_pkt_size;
} SWFDecContext;

static void parse_audio_specs(struct audio_specs *specs, uint8_t audio_info)
{
    enum swf_codec_id {
        SWF_AUDIO_PCM_NATIVE =  0,
        SWF_AUDIO_ADPCM      =  1,
        SWF_AUDIO_MP3        =  2,
        SWF_AUDIO_PCM_LE     =  3,
        SWF_AUDIO_NELLY16    =  4,
        SWF_AUDIO_NELLY8     =  5,
        SWF_AUDIO_NELLY      =  6,
        SWF_AUDIO_SPEEX      = 11,
    };
    const int codec_id = audio_info >> 4;
    static const int audio_rates[4] = {5500, 11000, 22000, 44000};
    static const enum AVCodecID audio_codecs[16] = {
        [SWF_AUDIO_PCM_NATIVE] = AV_NE(AV_CODEC_ID_PCM_S16BE, AV_CODEC_ID_PCM_S16LE),
        [SWF_AUDIO_ADPCM     ] = AV_CODEC_ID_ADPCM_SWF,
        [SWF_AUDIO_MP3       ] = AV_CODEC_ID_MP3,
        [SWF_AUDIO_PCM_LE    ] = AV_CODEC_ID_PCM_S16LE,
        [SWF_AUDIO_NELLY16   ] = AV_CODEC_ID_NELLYMOSER,
        [SWF_AUDIO_NELLY8    ] = AV_CODEC_ID_NELLYMOSER,
        [SWF_AUDIO_NELLY     ] = AV_CODEC_ID_NELLYMOSER,
        [SWF_AUDIO_SPEEX     ] = AV_CODEC_ID_SPEEX,
    };

    specs->codec_id = audio_codecs[codec_id];

    specs->rate = audio_rates[audio_info>>2 & 3];
    switch (codec_id) {
    case SWF_AUDIO_NELLY16: specs->rate = 16000; break;
    case SWF_AUDIO_NELLY8:  specs->rate =  8000; break;
    case SWF_AUDIO_SPEEX:   specs->rate = 16000; break;
    }

    specs->size = (audio_info>>1 & 1) ? 16 : 8;
    if (codec_id != SWF_AUDIO_PCM_NATIVE &&
        codec_id != SWF_AUDIO_ADPCM      &&
        codec_id != SWF_AUDIO_PCM_LE)
        specs->size = 16;

    if (specs->size == 8 &&
        (codec_id == SWF_AUDIO_PCM_NATIVE || codec_id == SWF_AUDIO_PCM_LE))
        specs->codec_id = AV_CODEC_ID_PCM_U8;

    specs->channel_layout = audio_info & 1 ? AV_CH_LAYOUT_STEREO
                                           : AV_CH_LAYOUT_MONO;
    if (specs->codec_id == AV_CODEC_ID_NELLYMOSER ||
        specs->codec_id == AV_CODEC_ID_SPEEX)
        specs->channel_layout = AV_CH_LAYOUT_MONO;
}

#ifdef DEBUG
static void dlog_audio_specs(void *logctx, const struct audio_specs *a)
{
    char chl[64];

    av_get_channel_layout_string(chl, sizeof(chl), -1, a->channel_layout);
    av_dlog(logctx, "codec=%s rate=%d size=%d chl=%s",
            avcodec_get_name(a->codec_id), a->rate, a->size, chl);
}
#else
#define dlog_audio_specs(a, b)
#endif

static int swf_probe(AVProbeData *p)
{
    if ((p->buf[0] == 'F' || p->buf[0] == 'C') && p->buf[1] == 'W' &&
        p->buf[2] == 'S')
        return AVPROBE_SCORE_MAX;
    return 0;
}

#if CONFIG_ZLIB
static int zlib_refill(void *opaque, uint8_t *buf, int buf_size)
{
    AVFormatContext *s = opaque;
    SWFDecContext *swf = s->priv_data;
    z_stream *z = &swf->zstream;
    int ret;

retry:
    if (!z->avail_in) {
        int n = avio_read(s->pb, swf->zbuf_in, ZBUF_SIZE);
        if (n < 0)
            return n;
        z->next_in  = swf->zbuf_in;
        z->avail_in = n;
    }

    z->next_out  = buf;
    z->avail_out = buf_size;

    ret = inflate(z, Z_NO_FLUSH);
    if (ret < 0)
        return AVERROR(EINVAL);
    if (ret == Z_STREAM_END)
        return AVERROR_EOF;

    if (buf_size - z->avail_out == 0)
        goto retry;

    return buf_size - z->avail_out;
}
#endif

/**
 * Get bits auto-byte-reader.
 *
 * Sometimes we can't know the total number of bits required in advance
 * (typically with the SWF matrix type layout), and the AVIO context will not
 * allow to seek back if the SWF is compressed. This means we can't rely on
 * lavc:get_bits() (we can't avio_read() a large enough buffer and seek back
 * when done), so we use this trivial and slow internal implementation, which
 * will avio_read() the appropriate amount, only when necessary.
 *
 * FIXME: we could eventually read the whole tag in advance
 */

struct swf_bits {
    uint8_t last;
    int nbitread;
    AVIOContext *pb;
};

static inline void swf_get_bits_init(struct swf_bits *s, AVIOContext *pb)
{
    s->last = 0;
    s->nbitread = 8;
    s->pb = pb;
}

static inline uint32_t swf_get_bits(struct swf_bits *s, int nbits)
{
    uint32_t res = 0;

    if (!nbits)
        return 0;
    av_assert1(nbits <= 32);

    while (nbits > 0) {
        int n, shift;
        uint8_t chr;

        if (s->nbitread == 8) {
            s->last = avio_r8(s->pb);
            s->nbitread = 0;
        }
        chr = s->last;

        n = FFMIN(nbits, 8 - s->nbitread);
        shift = 8 - (s->nbitread + n);
        chr = (chr >> shift) & ((1<<n) - 1);
        res = res << n | chr;
        s->nbitread += n;
        nbits -= n;
    }

    return res;
}

static char *parse_string(AVIOContext *pb)
{
    char c, *str;
    AVBPrint b;

    av_bprint_init(&b, 0, AV_BPRINT_SIZE_UNLIMITED);
    do {
        c = avio_r8(pb);
        av_bprint_chars(&b, c, 1);
    } while (c);
    if (av_bprint_finalize(&b, &str) < 0)
        return NULL;
    return str;
}

static void parse_rect(struct rect *r, AVIOContext *pb)
{
    int nbits;
    struct swf_bits b;

    swf_get_bits_init(&b, pb);
    nbits = swf_get_bits(&b, 5);
    r->xmin = swf_get_bits(&b, nbits);
    r->xmax = swf_get_bits(&b, nbits);
    r->ymin = swf_get_bits(&b, nbits);
    r->ymax = swf_get_bits(&b, nbits);
}

#ifdef DEBUG
static void dlog_matrix(void *logctx, const struct matrix *m)
{
    if (m->scale)
        av_dlog(logctx, "[sx=%f sy=%f", m->scale_x / 256., m->scale_y / 256.);
    if (m->rotate)
        av_dlog(logctx, "%cr0=%d r1=%d",
                m->scale ? '/' : '[',
                m->rotate_skew_0, m->rotate_skew_1);
    av_dlog(logctx, "%ctx=%d ty=%d]",
            m->scale || m->rotate ? '/' : '[',
            m->translate_x, m->translate_y);
}
#else
#define dlog_matrix(a, b)
#endif

static void parse_matrix(struct matrix *m, AVIOContext *pb)
{
    int nbits;
    struct swf_bits b;

    memset(m, 0, sizeof(*m));
    swf_get_bits_init(&b, pb);
    m->scale = swf_get_bits(&b, 1);
    if (m->scale) {
        nbits = swf_get_bits(&b, 5);
        m->scale_x = swf_get_bits(&b, nbits);
        m->scale_y = swf_get_bits(&b, nbits);
    }
    m->rotate = swf_get_bits(&b, 1);
    if (m->rotate) {
        nbits = swf_get_bits(&b, 5);
        m->rotate_skew_0 = swf_get_bits(&b, nbits);
        m->rotate_skew_1 = swf_get_bits(&b, nbits);
    }
    nbits = swf_get_bits(&b, 5);
    m->translate_x = swf_get_bits(&b, nbits);
    m->translate_y = swf_get_bits(&b, nbits);
}

#ifdef DEBUG
static void dlog_cxform(void *logctx, const struct cxform *c)
{
    if (c->add_terms) {
        av_dlog(logctx, " add:[%d %d %d", c->addr, c->addg, c->addb);
        if (c->is_alpha)
            av_dlog(logctx, " %d", c->adda);
        av_dlog(logctx, "%s", c->mul_terms ? "]/" : "]");
    }
    if (c->mul_terms) {
        av_dlog(logctx, "mul:[%d %d %d", c->mulr, c->mulg, c->mulb);
        if (c->is_alpha)
            av_dlog(logctx, " %d", c->mula);
        av_dlog(logctx, "]");
    }
}
#else
#define dlog_cxform(a, b)
#endif

static void parse_cxform(struct cxform *c, AVIOContext *pb, int alpha)
{
    int nbits;
    struct swf_bits b;

    memset(c, 0, sizeof(*c));
    swf_get_bits_init(&b, pb);
    c->add_terms = swf_get_bits(&b, 1);
    c->mul_terms = swf_get_bits(&b, 1);
    c->is_alpha  = alpha;
    nbits = swf_get_bits(&b, 4);
    if (c->mul_terms) {
        c->mulr = swf_get_bits(&b, nbits);
        c->mulg = swf_get_bits(&b, nbits);
        c->mulb = swf_get_bits(&b, nbits);
        if (alpha)
            c->mula = swf_get_bits(&b, nbits);
    }
    if (c->add_terms) {
        c->addr = swf_get_bits(&b, nbits);
        c->addg = swf_get_bits(&b, nbits);
        c->addb = swf_get_bits(&b, nbits);
        if (alpha)
            c->adda = swf_get_bits(&b, nbits);
    }
}

static void inline parse_color(uint8_t *rgba, AVIOContext *pb, int tagid)
{
    if (tagid == TAG_DEFINESHAPE3) {
        avio_read(pb, rgba, 4);
    } else if (tagid == TAG_DEFINESHAPE ||
               tagid == TAG_DEFINESHAPE2) {
        avio_read(pb, rgba, 3);
        rgba[3] = 0xff;
    }
}

static void parse_gradient(struct gradient *g, AVIOContext *pb, int tagid, int focal)
{
    int i;
    uint8_t c = avio_r8(pb);

    g->spread        = c >> 6;
    g->interpolation = c >> 4 & 3;
    g->nb_gradients  = c & 0xf;
    for (i = 0; i < g->nb_gradients; i++) {
        g->records[i].ratio = avio_r8(pb);
        parse_color(g->records[i].rgba, pb, tagid);
    }
    if (focal)
        g->focal_point = avio_rl16(pb);
}

#ifdef DEBUG
static void dlog_gradient(void *logctx, const struct gradient *g)
{
    int i;
    static const char *const spread[] = {"pad", "reflect", "repeat", "?"};
    static const char *const interp[] = {"normal", "linear", "?", "?"};

    av_dlog(logctx, "spread:%s interp:%s focal:%f / %d grad:[",
            spread[g->spread], interp[g->interpolation],
            g->focal_point / 256., g->nb_gradients);
    for (i = 0; i < g->nb_gradients; i++) {
        const struct grad_record *r = &g->records[i];
        av_dlog(logctx, " %02X%02X%02X%02X (r=%d)",
                r->rgba[0], r->rgba[1], r->rgba[2], r->rgba[3], r->ratio);
    }
    av_dlog(logctx, "]");
}
#else
#define dlog_gradient(a, b)
#endif

static void parse_fillstyle(struct fillstyle *s, AVIOContext *pb, int tagid)
{
    memset(s, 0, sizeof(*s));

    s->type = avio_r8(pb);
    if (s->type == 0) {
        parse_color(s->rgba, pb, tagid);
    } else if (s->type & 0x10) {
        parse_matrix(&s->matrix, pb);
        parse_gradient(&s->gradient, pb, tagid, s->type == FILL_FOCAL_RADIAL_GRADIANT);
    } else if (s->type & 0x40) {
        s->bitmap_id = avio_rl16(pb);
        parse_matrix(&s->matrix, pb);
    }
}

static void parse_fillstylearray(struct fillstylearray *a, AVIOContext *pb, int tagid)
{
    int i;

    a->style_count = avio_r8(pb);
    if (a->style_count == 0xff)
        a->style_count = avio_rl16(pb);
    a->styles = av_malloc(a->style_count * sizeof(*a->styles));
    if (!a->styles)
        return;
    for (i = 0; i < a->style_count; i++)
        parse_fillstyle(a->styles + i, pb, tagid);
}

#ifdef DEBUG
static void dlog_fillstylearray(void *logctx, const struct fillstylearray *a)
{
    int i;

    for (i = 0; i < a->style_count; i++) {
        const struct fillstyle *s = &a->styles[i];

        av_dlog(logctx, "    [%d/%d] ", i + 1, a->style_count);
        if (s->type == 0) {
            av_dlog(logctx, "color=#%02X%02X%02X%02X",
                    s->rgba[0], s->rgba[1], s->rgba[2], s->rgba[3]);
        } else if (s->type & 0x10) {
            av_dlog(logctx, "MATRIX:");
            dlog_matrix(logctx, &s->matrix);
            av_dlog(logctx, " GRADIENT:");
            dlog_gradient(logctx, &s->gradient);
        } else if (s->type & 0x40) {
            av_dlog(logctx, "bmpid:%d | MATRIX:", s->bitmap_id);
            dlog_matrix(logctx, &s->matrix);
        } else {
            av_dlog(logctx, "???");
        }
        av_dlog(logctx, "\n");
    }
}
#else
#define dlog_fillstylearray(a, b)
#endif

static void free_fillstylearray(struct fillstylearray *a)
{
    av_freep(&a->styles);
}

static void parse_linestyle(struct linestyle *s, AVIOContext *pb, int tagid)
{
    memset(s, 0, sizeof(*s));

    s->width = avio_rl16(pb);
    if (tagid == TAG_DEFINESHAPE4) {
        uint8_t c = avio_r8(pb);
        s->cap_style  = c >> 6;
        s->join_style = c >> 4 & 3;
        s->flags      = c & 0xf;

        c = avio_r8(pb);
        s->noclose       = c >> 2 & 1;
        s->end_cap_style = c & 3;

        if (s->join_style == JOIN_MITER)
            s->miter_limit_factor = avio_rl16(pb);
        if (s->flags & LSTL_HASFILL)
            parse_color(s->rgba, pb, 1);
        else
            parse_fillstyle(&s->fillstyle, pb, tagid);
    } else { // shape 1, 2 or 3
        parse_color(s->rgba, pb, tagid);
    }
}

static void parse_linestylearray(struct linestylearray *a, AVIOContext *pb, int tagid)
{
    int i;

    a->style_count = avio_r8(pb);
    if (a->style_count == 0xff)
        a->style_count = avio_rl16(pb);
    //av_dlog(NULL, "line style array: %d\n", a->style_count);
    a->styles = av_malloc(a->style_count * sizeof(*a->styles));
    if (!a->styles)
        return;
    for (i = 0; i < a->style_count; i++)
        parse_linestyle(a->styles + i, pb, tagid);
}

static void free_linestylearray(struct linestylearray *a)
{
    av_freep(&a->styles);
}

static void parse_shape(struct shape *s, AVIOContext *pb, int style, int tagid)
{
    struct swf_bits b;
    struct shape_record record;

    int nbits, fillnbits, linenbits;

    memset(s, 0, sizeof(*s));
    s->has_style = style;
    if (style) {
        parse_fillstylearray(&s->fillstyles, pb, tagid);
        //av_dlog(NULL, "  Shape fill styles:\n");
        //dlog_fillstylearray(NULL, &s->fillstyles);
        parse_linestylearray(&s->linestyles, pb, tagid);
        //av_dlog(NULL, "  Shape line styles:\n");
        //dlog_linestylearray(NULL, &s->fillstyles);
    }

    nbits = avio_r8(pb);
    fillnbits = nbits >> 4;
    linenbits = nbits & 0xf;

    //av_dlog(NULL, "fillnbits=%d linenbits=%d\n", fillnbits, linenbits);

    swf_get_bits_init(&b, pb);

    for (;;) {
        memset(&record, 0, sizeof(record));

        record.is_edge = swf_get_bits(&b, 1);
        //av_dlog(NULL, "  RECORD [edge=%c]\n", record.is_edge ? 'y' : 'n');
        if (!record.is_edge) {
            int state = record.style_change = swf_get_bits(&b, 5);

            if (!state) // End Shape Record
                break;

            //av_dlog(NULL, "    state: [%s] [%s] [%s] [%s] [%s]\n",
            //        state & SS_MOVETO     ? "MoveTo"     : "X",
            //        state & SS_FILLSTYLE0 ? "FillStyle0" : "X",
            //        state & SS_FILLSTYLE1 ? "FillStyle1" : "X",
            //        state & SS_LINESTYLE  ? "LineStyle"  : "X",
            //        state & SS_NEWSTYLES  ? "NewStyles"  : "X");

            if (state & SS_MOVETO) {
                nbits = swf_get_bits(&b, 5);
                record.move_delta_x = swf_get_bits(&b, nbits);
                record.move_delta_y = swf_get_bits(&b, nbits);
            }
            if (state & SS_FILLSTYLE0)
                record.fill_style_0 = swf_get_bits(&b, fillnbits);
            if (state & SS_FILLSTYLE1)
                record.fill_style_1 = swf_get_bits(&b, fillnbits);
            if (state & SS_LINESTYLE)
                record.line_style = swf_get_bits(&b, linenbits);
            if (state & SS_NEWSTYLES) {
                parse_fillstylearray(&s->fillstyles, pb, tagid);
                parse_linestylearray(&s->linestyles, pb, tagid);
                fillnbits = swf_get_bits(&b, 4);
                linenbits = swf_get_bits(&b, 4);
            }
        } else {
            record.straight     = swf_get_bits(&b, 1);
            nbits               = swf_get_bits(&b, 4) + 2;
            record.general_line = swf_get_bits(&b, 1);
            if (record.general_line) {
                record.delta_x = swf_get_bits(&b, nbits);
                record.delta_y = swf_get_bits(&b, nbits);
            } else {
                record.vertical_line = swf_get_bits(&b, 1);
                record.delta_x =
                record.delta_y = swf_get_bits(&b, nbits);
            }
        }
        av_dynarray2_add((void**)&s->records, &s->nb_records,
                         sizeof(s->records), (void*)&record);
    }
}

static void free_shape(struct shape *s)
{
    int i;
    for (i = 0; i < s->nb_records; i++) {
        struct shape_record *record = &s->records[i];
        if (record->style_change & SS_NEWSTYLES) {
            free_fillstylearray(&record->fillstyles);
            free_linestylearray(&record->linestyles);
        }
    }
    free_fillstylearray(&s->fillstyles);
    free_linestylearray(&s->linestyles);
}

static int insert_in_dict(AVFormatContext *s, int char_id,
                          int tag_id, void *data)
{
    int i;
    SWFDecContext *swf = s->priv_data;

    av_dlog(s, "  Insert content of type %d for char id %d [cur dict size:%d]\n",
            tag_id, char_id, swf->dict_nb_entries);
    for (i = swf->dict_nb_entries; i <= char_id; i++) {
        struct swf_dict_entry empty_entry = {.tag_id = -1};
        if (!av_dynarray2_add((void **)&swf->dict, &swf->dict_nb_entries,
                              sizeof(*swf->dict), (void*)&empty_entry))
            return -1;
    }
    if (swf->dict[char_id].tag_id != -1) {
        av_log(s, AV_LOG_ERROR, "Character %d is already defined "
               "in the dictionary\n", char_id);
        return -1;
    }
    swf->dict[char_id].tag_id  = tag_id;
    swf->dict[char_id].content = data;
    return 0;
}

static const struct swf_dict_entry *fetch_from_dict(AVFormatContext *s, int char_id)
{
    SWFDecContext *swf = s->priv_data;

    if (char_id >= swf->dict_nb_entries)
        return NULL;
    return &swf->dict[char_id];
}

static struct swf_disp_entry *get_disp_entry(AVFormatContext *s, int depth, int char_id)
{
    int i;
    SWFDecContext *swf = s->priv_data;

    if (char_id) { // add
        for (i = swf->disp_nb_entries; i <= depth; i++) {
            struct swf_disp_entry empty_entry = {0};
            if (!av_dynarray2_add((void **)&swf->display_list, &swf->disp_nb_entries,
                                  sizeof(*swf->display_list), (void*)&empty_entry))
                return NULL;
        }
        swf->display_list[depth].ch_id = char_id;
    } else if (depth >= swf->disp_nb_entries || depth < 1) {
        av_log(s, AV_LOG_ERROR, "Invalid display list depth entry %d\n", depth);
        return NULL;
    }
    return &swf->display_list[depth];
}

// copied from lswsutils.c:ff_scale_image()
static int scale_image(uint8_t *dst_data[4], int dst_linesize[4],
                       int dst_w, int dst_h, enum AVPixelFormat dst_pix_fmt,
                       uint8_t * const src_data[4], int src_linesize[4],
                       int src_w, int src_h, enum AVPixelFormat src_pix_fmt,
                       void *log_ctx)
{
    int ret;
    struct SwsContext *sws_ctx = sws_getContext(src_w, src_h, src_pix_fmt,
                                                dst_w, dst_h, dst_pix_fmt,
                                                SWS_BILINEAR, NULL, NULL, NULL);
    if (!sws_ctx) {
        av_log(log_ctx, AV_LOG_ERROR,
               "Impossible to create scale context for the conversion "
               "fmt:%s s:%dx%d -> fmt:%s s:%dx%d\n",
               av_get_pix_fmt_name(src_pix_fmt), src_w, src_h,
               av_get_pix_fmt_name(dst_pix_fmt), dst_w, dst_h);
        ret = AVERROR(EINVAL);
        goto end;
    }

    if ((ret = av_image_alloc(dst_data, dst_linesize, dst_w, dst_h, dst_pix_fmt, 16)) < 0)
        goto end;
    ret = 0;
    sws_scale(sws_ctx, (const uint8_t * const*)src_data, src_linesize, 0, src_h, dst_data, dst_linesize);

end:
    sws_freeContext(sws_ctx);
    return ret;
}

static int handle_show_frame(AVFormatContext *s, AVIOContext *pb,
                             AVPacket *pkt, const struct swftag *tag)
{
    int i, ret = 0;
    SWFDecContext *swf = s->priv_data;

    ret = av_new_packet(pkt, swf->video_pkt_size);
    if (ret < 0)
        return ret;
    pkt->stream_index = 0;



    for (i = 0; i < swf->disp_nb_entries; i++) {
        const int ch_id = swf->display_list[i].ch_id;
        const struct swf_dict_entry *e = fetch_from_dict(s, ch_id);
        uint8_t *bitmap[4];
        int linesizes[4];

        if (!e) {
            av_log(s, AV_LOG_ERROR, "Character %d in display list but not "
                   "present in dictionary\n", ch_id);
            continue;
        }
        if (e->tag_id == TAG_DEFINEBITSLOSSLESS ||
            e->tag_id == TAG_DEFINEBITSLOSSLESS2) {
            struct lossless_bitmap_char *lbc = e->content;
            ret = scale_image(bitmap,    linesizes,      lbc->w, lbc->h, AV_PIX_FMT_RGBA,
                              lbc->data, lbc->linesizes, lbc->w, lbc->h, lbc->pix_fmt, s);
            if (ret < 0)
                break;
        }
        //blend_rgba_pic();
    }

    return ret;
}

static int handle_define_shape(AVFormatContext *s, AVIOContext *pb,
                               AVPacket *pkt, const struct swftag *tag)
{
    const int64_t cur_pos = avio_tell(pb);
    struct shape_char *sc;
    int id = avio_rl16(pb);

    sc = av_mallocz(sizeof(*sc));
    if (!sc)
        return AVERROR(ENOMEM);

    parse_rect(&sc->bounds, pb);
    av_dlog(s, "  id=%d bounds: %.1f %.1f %.1f %.1f\n", id,
            sc->bounds.xmin / 20., sc->bounds.xmax / 20.,
            sc->bounds.ymin / 20., sc->bounds.ymax / 20.);

    parse_shape(&sc->data, pb, 1, tag->id);

    if (insert_in_dict(s, id, tag->id, sc) < 0) {
        free_shape(&sc->data);
        av_free(sc);
        return AVERROR(ENOMEM);
    }

    return avio_tell(pb) - cur_pos;
}

static int handle_place_object1(AVFormatContext *s, AVIOContext *pb,
                                AVPacket *pkt, const struct swftag *tag)
{
    const int64_t cur_pos = avio_tell(pb);
    int ch_id, depth;
    struct matrix matrix;

    ch_id = avio_rl16(pb);
    depth = avio_rl16(pb);
    parse_matrix(&matrix, pb);
    // TODO: optional color transform

    av_dlog(s, "  ch_id=%d depth=%d\n", ch_id, depth);
    return avio_tell(pb) - cur_pos;
}

static int handle_define_bits(AVFormatContext *s, AVIOContext *pb,
                              AVPacket *pkt, const struct swftag *tag)
{
    int ch_id = avio_rl16(pb);
    struct lossy_char *jpg = av_mallocz(sizeof(*jpg));

    if (!jpg)
        goto err;
    jpg->size = tag->len - 2;
    jpg->data = av_malloc(jpg->size);
    if (!jpg->data)
        goto err;
    avio_read(pb, jpg->data, jpg->size);
    if (insert_in_dict(s, ch_id, tag->id, jpg) < 0)
        goto err;
    return tag->len;

err:
    if (jpg) {
        av_free(jpg->data);
        av_free(jpg);
    }
    return AVERROR(ENOMEM);
}

enum place_flags {
    PF_MOVE               = 1<<0,
    PF_HAS_CHARACTER      = 1<<1,
    PF_HAS_MATRIX         = 1<<2,
    PF_HAS_COLORTRANSFORM = 1<<3,
    PF_HAS_RATIO          = 1<<4,
    PF_HAS_NAME           = 1<<5,
    PF_HAS_CLIPDEPTH      = 1<<6,
    PF_HAS_CLIPACTIONS    = 1<<7,
};

static int handle_place_object2(AVFormatContext *s, AVIOContext *pb,
                                AVPacket *pkt, const struct swftag *tag)
{
    const int64_t cur_pos = avio_tell(pb);
    int ch_id = 0, depth;
    uint8_t flags;
    struct swf_disp_entry *e;

    flags = avio_r8(pb);
    depth = avio_rl16(pb);

    av_dlog(s, "  depth=%d", depth);
    if (flags & PF_HAS_CHARACTER) {
        ch_id = avio_rl16(pb);
        av_dlog(s, " ch_id=%d", ch_id);
    }

    e = get_disp_entry(s, depth, ch_id);
    if (!e)
        return AVERROR(EINVAL);

    if (flags & PF_HAS_MATRIX) {
        av_dlog(s, " matrix=");
        parse_matrix(&e->matrix, pb);
        dlog_matrix(s, &e->matrix);
    }
    if (flags & PF_HAS_COLORTRANSFORM) {
        av_dlog(s, " cxform=");
        parse_cxform(&e->cxform, pb, 1);
        dlog_cxform(s, &e->cxform);
    }
    if (flags & PF_HAS_RATIO) {
        e->ratio = avio_rl16(pb);
        av_dlog(s, " ratio=%d", e->ratio);
    }
    if (flags & PF_HAS_NAME) {
        e->name = parse_string(pb);
        av_dlog(s, " name=[%s]", e->name);
        //av_freep(&e->name);
    }
    if (flags & PF_HAS_CLIPDEPTH) {
        e->clip_depth = avio_rl16(pb);
        av_dlog(s, " clipdepth=%d\n", e->clip_depth);
    }
    if (flags & PF_HAS_CLIPACTIONS) {
        av_dlog(s, " clipaction=TODO\n");
    }
    av_dlog(s, "\n");

    return avio_tell(pb) - cur_pos;
}

static int handle_place_object3(AVFormatContext *s, AVIOContext *pb,
                                AVPacket *pkt, const struct swftag *tag)
{
    const int64_t cur_pos = avio_tell(pb);
    int ch_id, depth;
    uint16_t flags;
    //struct matrix matrix;

    flags = avio_rl16(pb);
    depth = avio_rl16(pb);
    ch_id = avio_rl16(pb);
    // ...

    av_dlog(s, "  flags=%04X depth=%d ch_id=%d\n", flags, depth, ch_id);
    return avio_tell(pb) - cur_pos;
}

static int handle_background_color(AVFormatContext *s, AVIOContext *pb,
                                   AVPacket *pkt, const struct swftag *tag)
{
    SWFDecContext *swf = s->priv_data;
    uint8_t *buf = swf->background;
    int nread;

    nread = avio_read(pb, buf, sizeof(*buf));
    av_dlog(s, "  Set background color to: #%02X%02X%02X\n",
           buf[0], buf[1], buf[2]);
    return nread;
}

static int handle_define_sound(AVFormatContext *s, AVIOContext *pb,
                               AVPacket *pkt, const struct swftag *tag)
{
    const int64_t cur_pos = avio_tell(pb);
    const int ch_id = avio_rl16(pb);
    struct audio_specs specs;
    struct audio_def *audio = av_mallocz(sizeof(*audio));

    if (!audio)
        return AVERROR(ENOMEM);

    parse_audio_specs(&specs, avio_r8(pb));
    av_dlog(s, "  Sound definition:");
    dlog_audio_specs(s, &specs);
    av_dlog(s, "\n");

    audio->nb_samples = avio_rl32(pb);
    audio->skip_samples = specs.codec_id == AV_CODEC_ID_MP3 ? avio_rl16(pb) : 0;
    audio->size = tag->len - (avio_tell(pb) - cur_pos);
    audio->data = av_malloc(audio->size);
    if (!audio->data)
        return AVERROR(ENOMEM);
    avio_read(pb, audio->data, audio->size);
    insert_in_dict(s, ch_id, tag->id, audio);

    if (s->nb_streams == 1) {
        AVStream *ast = avformat_new_stream(s, NULL);
        if (!ast)
            return AVERROR(ENOMEM);
        ast->codec->codec_type     = AVMEDIA_TYPE_AUDIO;
        ast->codec->channel_layout = specs.channel_layout;
        ast->codec->channels       = av_get_channel_layout_nb_channels(specs.channel_layout);
        ast->codec->codec_id       = specs.codec_id;
        ast->codec->sample_rate    = specs.rate;
        ast->duration              = audio->nb_samples;
        ast->need_parsing          = AVSTREAM_PARSE_FULL;
        avpriv_set_pts_info(ast, 64, 1, ast->codec->sample_rate);
    }

    return avio_tell(pb) - cur_pos;
}

static int handle_start_sound(AVFormatContext *s, AVIOContext *pb,
                              AVPacket *pkt, const struct swftag *tag)
{
    enum {
        SOUNDINFO_HAS_IN_POINT     = 1<<0,
        SOUNDINFO_HAS_OUT_POINT    = 1<<1,
        SOUNDINFO_HAS_LOOPS        = 1<<2,
        SOUNDINFO_HAS_ENVELOPE     = 1<<3,
        SOUNDINFO_SYNC_NO_MULTIPLE = 1<<4,
        SOUNDINFO_SYNC_STOP        = 1<<5,
    };
    int ret;
    const int64_t cur_pos = avio_tell(pb);
    const int ch_id = avio_rl16(pb);
    const uint8_t flags = avio_r8(pb);
    const struct swf_dict_entry *e = fetch_from_dict(s, ch_id);
    const struct audio_def *ad;

    if (flags & SOUNDINFO_HAS_IN_POINT)
        avio_skip(pb, 4);
    if (flags & SOUNDINFO_HAS_OUT_POINT)
        avio_skip(pb, 4);
    if (flags & SOUNDINFO_HAS_LOOPS)
        av_log(s, AV_LOG_DEBUG, "  loop=%d\n", avio_rl16(pb));

    if (!e) {
        av_log(s, AV_LOG_ERROR, "Sound id %d not found in dictionary\n", ch_id);
        return AVERROR_INVALIDDATA;
    }
    if (e->tag_id != TAG_DEFINESOUND) {
        av_log(s, AV_LOG_ERROR, "Entry %d is not a sound definition (%d)\n", ch_id, e->tag_id);
        return AVERROR_INVALIDDATA;
    }

    ad = e->content;
    ret = av_new_packet(pkt, ad->size);
    if (ret < 0)
        return ret;
    pkt->stream_index = 1;
    memcpy(pkt->data, ad->data, ad->size);

    return avio_tell(pb) - cur_pos;
}

static int handle_define_bits_lossless(AVFormatContext *s, AVIOContext *pb,
                                       AVPacket *pkt, const struct swftag *tag)
{
#if CONFIG_ZLIB
    const int64_t cur_pos = avio_tell(pb);
    long out_len;
    uint8_t *buf = NULL, *zbuf = NULL;
    const int alpha_bmp = tag->version == 2;
    const int colormapbpp = 3 + alpha_bmp;
    int linesize, colormapsize = 0;
    int i, res = 0, len = tag->len, size;
    struct lossless_bitmap_char *lbc;
    uint32_t *pal;

    const int ch_id   = avio_rl16(pb);
    const int bmp_fmt = avio_r8(pb);
    const int width   = avio_rl16(pb);
    const int height  = avio_rl16(pb);

    len -= 2+1+2+2;

    switch (bmp_fmt) {
    case 3: // PAL-8
        linesize = width;
        colormapsize = avio_r8(pb) + 1;
        len--;
        break;
    case 4: // RGB15
        linesize = width * 2;
        break;
    case 5: // RGB24 (0RGB or ARGB)
        linesize = width * 4;
        break;
    default:
        av_log(s, AV_LOG_ERROR, "invalid bitmap format %d, skipped\n", bmp_fmt);
        goto end;
    }

    linesize = FFALIGN(linesize, 4);

    if (av_image_check_size(width, height, 0, s) < 0 ||
        linesize >= INT_MAX / height ||
        linesize * height >= INT_MAX - colormapsize * colormapbpp) {
        av_log(s, AV_LOG_ERROR, "invalid frame size %dx%d\n", width, height);
        goto end;
    }

    out_len = colormapsize * colormapbpp + linesize * height;

    av_dlog(s, "bitmap: ch=%d fmt=%d %dx%d (linesize=%d) len=%d->%ld pal=%d\n",
            ch_id, bmp_fmt, width, height, linesize, len, out_len, colormapsize);

    zbuf = av_malloc(len);
    buf  = av_malloc(out_len);
    lbc  = av_mallocz(sizeof(*lbc));
    if (!zbuf || !buf || !lbc) {
        res = AVERROR(ENOMEM);
        goto end;
    }

    len = avio_read(pb, zbuf, len);
    if (len < 0 || uncompress(buf, &out_len, zbuf, len) != Z_OK) {
        av_log(s, AV_LOG_WARNING, "Failed to uncompress one bitmap\n");
        goto end;
    }

    switch (bmp_fmt) {
    case 3:
        lbc->data[1] = av_mallocz(AVPALETTE_SIZE);
        pal = (uint32_t*)lbc->data[1];
        lbc->pix_fmt = AV_PIX_FMT_PAL8;
        for (i = 0; i < colormapsize; i++)
            if (alpha_bmp)  pal[i] = buf[3]<<24 | AV_RB24(buf + 4*i);
            else            pal[i] = 0xffU <<24 | AV_RB24(buf + 3*i);
        break;
    case 4:
        lbc->pix_fmt = AV_PIX_FMT_RGB555;
        break;
    case 5:
        lbc->pix_fmt = alpha_bmp ? AV_PIX_FMT_ARGB : AV_PIX_FMT_0RGB;
        break;
    default:
        av_assert0(0);
    }

    size = out_len - colormapsize * colormapbpp;
    if (linesize * height > size) {
        res = AVERROR_INVALIDDATA;
        goto end;
    }
    lbc->data[0] = av_malloc(size);
    if (!lbc->data[0]) {
        res = AVERROR(ENOMEM);
        goto end;
    }
    lbc->w = width;
    lbc->h = height;
    lbc->linesizes[0] = linesize;
    memcpy(lbc->data[0], buf + colormapsize*colormapbpp, linesize * height);
    if (insert_in_dict(s, ch_id, tag->id, lbc) < 0)
        goto end;

    lbc = NULL;

end:
    av_freep(&zbuf);
    av_freep(&buf);
    if (lbc) {
        av_freep(&lbc->data[0]);
        av_freep(&lbc->data[1]);
        av_freep(&lbc);
    }
    if (res < 0)
        return res;
    return avio_tell(pb) - cur_pos;
#else
    av_log(s, AV_LOG_ERROR, "this file requires zlib support compiled in\n");
    return AVERROR(EINVAL);
#endif
}

// XXX: totally useless, replace with handle_dummy?
static int handle_sound_stream_head(AVFormatContext *s, AVIOContext *pb,
                                    AVPacket *pkt, const struct swftag *tag)
{
    const int64_t cur_pos = avio_tell(pb);
    struct audio_specs playback, stream;

    parse_audio_specs(&playback, avio_r8(pb));
    parse_audio_specs(&stream,   avio_r8(pb));
    av_dlog(s, "  Playback: ");
    dlog_audio_specs(s, &playback);
    av_dlog(s, "  Stream: ");
    dlog_audio_specs(s, &stream);
    av_dlog(s, "\n");

    return avio_tell(pb) - cur_pos;
}

static int handle_dummy(AVFormatContext *s, AVIOContext *pb,
                        AVPacket *pkt, const struct swftag *tag)
{
    /* supported but nothing to do with it */
    return 0;
}

// XXX: looks useless, replace with handle_dymmy?
static int handle_metadata(AVFormatContext *s, AVIOContext *pb,
                           AVPacket *pkt, const struct swftag *tag)
{
    int64_t cur_pos = avio_tell(pb);
    char *xml = parse_string(pb);

    av_dlog(s, "Metadata: [%s]\n", xml); // likely truncated by logging
    av_freep(&xml);
    return avio_tell(pb) - cur_pos;
}

static const struct tag_prop tag_props[] = {
    [TAG_END]                          = {"End"},
    [TAG_SHOWFRAME]                    = {"ShowFrame",                    handle_show_frame},
    [TAG_DEFINESHAPE]                  = {"DefineShape",                  handle_define_shape},
    [TAG_FREECHARACTER]                = {"FreeCharacter"},
    [TAG_PLACEOBJECT]                  = {"PlaceObject",                  handle_place_object1},
    [TAG_REMOVEOBJECT]                 = {"RemoveObject"},
    [TAG_DEFINEBITS]                   = {"DefineBits",                   handle_define_bits},
    [TAG_DEFINEBUTTON]                 = {"DefineButton"},
    [TAG_JPEGTABLES]                   = {"JPEGTables"},
    [TAG_SETBACKGROUNDCOLOR]           = {"SetBackgroundColor",           handle_background_color},
    [TAG_DEFINEFONT]                   = {"DefineFont"},
    [TAG_DEFINETEXT]                   = {"DefineText"},
    [TAG_DOACTION]                     = {"DoAction"},
    [TAG_DEFINEFONTINFO]               = {"DefineFontInfo"},
    [TAG_DEFINESOUND]                  = {"DefineSound",                  handle_define_sound},
    [TAG_STARTSOUND]                   = {"StartSound",                   handle_start_sound},
    [TAG_DEFINEBUTTONSOUND]            = {"DefineButtonSound"},
    [TAG_STREAMHEAD]                   = {"SoundStreamHead"},
    [TAG_STREAMBLOCK]                  = {"SoundStreamBlock"},
    [TAG_DEFINEBITSLOSSLESS]           = {"DefineBitsLossless",           handle_define_bits_lossless},
    [TAG_JPEG2]                        = {"DefineBitsJPEG2"},
    [TAG_DEFINESHAPE2]                 = {"DefineShape2",                 handle_define_shape},
    [TAG_DEFINEBUTTONCXFORM]           = {"DefineButtonCXForm"},
    [TAG_PROTECT]                      = {"Protect"},
    [TAG_PLACEOBJECT2]                 = {"PlaceObject2",                 handle_place_object2},
    [TAG_REMOVEOBJECT2]                = {"RemoveObject2"},
    [TAG_DEFINESHAPE3]                 = {"DefineShape3",                 handle_define_shape},
    [TAG_DEFINETEXT2]                  = {"DefineText2"},
    [TAG_DEFINEBUTTON2]                = {"DefineButton2"},
    [TAG_DEFINEBITSJPEG3]              = {"DefineBitsJPEG3"},
    [TAG_DEFINEBITSLOSSLESS2]          = {"DefineBitsLossless2",          handle_define_bits_lossless},
    [TAG_DEFINEEDITTEXT]               = {"DefineEditText"},
    [TAG_DEFINESPRITE]                 = {"DefineSprite"},
    [TAG_FRAMELABEL]                   = {"FrameLabel"},
    [TAG_STREAMHEAD2]                  = {"SoundStreamHead2",             handle_sound_stream_head},
    [TAG_DEFINEMORPHSHAPE]             = {"DefineMorphShape"},
    [TAG_DEFINEFONT2]                  = {"DefineFont2"},
    [TAG_EXPORTASSETS]                 = {"ExportAssets"},
    [TAG_IMPORTASSETS]                 = {"ImportAssets"},
    [TAG_ENABLEDEBUGGER]               = {"EnableDebugger"},
    [TAG_DOINITACTION]                 = {"DoInitAction"},
    [TAG_VIDEOSTREAM]                  = {"DefineVideoStream"},
    [TAG_VIDEOFRAME]                   = {"VideoFrame"},
    [TAG_DEFINEFONTINFO2]              = {"DefineFontInfo2"},
    [TAG_ENABLEDEBUGGER2]              = {"EnableDebugger2"},
    [TAG_SCRIPTLIMITS]                 = {"ScriptLimits"},
    [TAG_SETTABINDEX]                  = {"SetTabIndex"},
    [TAG_FILEATTRIBUTES]               = {"FileAttributes",               handle_dummy},
    [TAG_PLACEOBJECT3]                 = {"PlaceObject3",                 handle_place_object3},
    [TAG_IMPORTASSETS2]                = {"ImportAssets2"},
    [TAG_DEFINEFONTALIGNZONES]         = {"DefineFontAlignZones"},
    [TAG_CSMTEXTSETTINGS]              = {"CSMTextSettings"},
    [TAG_DEFINEFONT3]                  = {"DefineFont3"},
    [TAG_SYMBOLCLASS]                  = {"SymbolClass"},
    [TAG_METADATA]                     = {"Metadata",                     handle_metadata},
    [TAG_DEFINESCALINGGRID]            = {"DefineScalingGrid"},
    [TAG_DOABC]                        = {"DoABC"},
    [TAG_DEFINESHAPE4]                 = {"DefineShape4",                 handle_define_shape},
    [TAG_DEFINEMORPHSHAPE2]            = {"DefineMorphShape2"},
    [TAG_DEFINESCENEANDFRAMELABELDATA] = {"DefineSceneAndFrameLabelData"},
    [TAG_DEFINEBINARYDATA]             = {"DefineBinaryData"},
    [TAG_DEFINEFONTNAME]               = {"DefineFontName"},
    [TAG_STARTSOUND2]                  = {"StartSound2"},
    [TAG_DEFINEBITSJPEG4]              = {"DefineBitsJPEG4"},
    [TAG_DEFINEFONT4]                  = {"DefineFont4"},
};

static inline int get_swf_tag(AVFormatContext *s, AVIOContext *pb,
                              struct swftag *tag)
{
    const struct tag_prop *prop;

    if (url_feof(pb)) {
        return AVERROR_EOF;
    }

    tag->id  = avio_rl16(pb);
    tag->len = tag->id & 0x3f;
    tag->id  = tag->id >> 6;
    if (tag->len == 0x3f)
        tag->len = avio_rl32(pb);
    if (tag->len < 0) {
        av_log(s, AV_LOG_ERROR, "invalid tag length: %d\n", tag->len);
        return AVERROR_INVALIDDATA;
    }

    prop = (unsigned)tag->id < FF_ARRAY_ELEMS(tag_props) ? &tag_props[tag->id] : NULL;
    tag->prop = prop;
    if (prop && prop->name) {
        char vchr = prop->name[strlen(prop->name) - 1];
        tag->version = av_isdigit(vchr) ? vchr - '0' : 1;
        av_log(s, AV_LOG_DEBUG, "%-10d TAG: %s\n", tag->len, prop->name);
    } else {
        tag->version = 1;
        av_log(s, AV_LOG_DEBUG, "%-10d TAG: unknown (%d)\n", tag->len, tag->id);
    }

    return 0;
}

static int swf_read_packet(AVFormatContext *s, AVPacket *pkt)
{
    int len, ret = AVERROR_EOF;
    SWFDecContext *swf = s->priv_data;
    AVIOContext *pb = s->pb;
    struct swftag tag;

#if CONFIG_ZLIB
    if (swf->zpb)
        pb = swf->zpb;
#endif

    for (;;) {
        pkt->size = 0;

        ret = get_swf_tag(s, pb, &tag);
        if (ret < 0)
            break;

        len = tag.len;
        if (tag.prop && tag.prop->handle) {
            ret = tag.prop->handle(s, pb, pkt, &tag);
            if (ret < 0)
                break;
            if (ret > 0)
                len -= ret;
        }

        if (len < 0)
            av_log(s, AV_LOG_WARNING, "Clipping len %d\n", len);
        len = FFMAX(0, len);
        avio_skip(pb, len);

        if (pkt->size > 0)
            return pkt->size;
    }

    return ret;
}

static av_cold int swf_read_header(AVFormatContext *s)
{
    AVStream *st;
    SWFDecContext *swf = s->priv_data;
    AVIOContext *pb = s->pb;
    struct swf_bits b;
    struct rect r;
    int tag, frame_rate, frame_count;

    tag = avio_rb32(pb);
    swf->version = tag & 0xff;
    tag &= 0xffffff00;

    avio_rl32(pb); /* file length */

    // TODO: 'ZWS' = LZMA (swf v13+)

    if (tag == MKBETAG('C', 'W', 'S', 0)) {
        av_log(s, AV_LOG_INFO, "SWF compressed file detected\n");
#if CONFIG_ZLIB
        swf->zbuf_in  = av_malloc(ZBUF_SIZE);
        swf->zbuf_out = av_malloc(ZBUF_SIZE);
        swf->zpb = avio_alloc_context(swf->zbuf_out, ZBUF_SIZE, 0, s,
                                      zlib_refill, NULL, NULL);
        if (!swf->zbuf_in || !swf->zbuf_out || !swf->zpb)
            return AVERROR(ENOMEM);
        swf->zpb->seekable = 0;
        if (inflateInit(&swf->zstream) != Z_OK) {
            av_log(s, AV_LOG_ERROR, "Unable to init zlib context\n");
            return AVERROR(EINVAL);
        }
        pb = swf->zpb;
#else
        av_log(s, AV_LOG_ERROR, "zlib support is required to read SWF compressed files\n");
        return AVERROR(EIO);
#endif
    } else if (tag != MKBETAG('F', 'W', 'S', 0))
        return AVERROR(EIO);

    swf_get_bits_init(&b, pb);
    parse_rect(&r, pb);
    frame_rate  = avio_rl16(pb); /* 8.8 fixed */
    frame_count = avio_rl16(pb); /* frame count */

    st = avformat_new_stream(s, NULL);
    if (!st)
        return AVERROR(ENOMEM);
    st->codec->codec_type = AVMEDIA_TYPE_VIDEO;
    st->codec->codec_id   = AV_CODEC_ID_RAWVIDEO;
    st->codec->width      = (r.xmax + .5) / 20;
    st->codec->height     = (r.ymax + .5) / 20;
    st->codec->pix_fmt    = AV_PIX_FMT_RGBA;
    swf->video_pkt_size   = 4 * st->codec->width * st->codec->height;
    avpriv_set_pts_info(st, 16, 256, frame_rate);

    av_log(s, AV_LOG_VERBOSE, "SWF version %d / fps=%f (%d frames) size=%dx%d\n",
           swf->version, frame_rate / 256., frame_count,
           st->codec->width, st->codec->height);

    s->ctx_flags |= AVFMTCTX_NOHEADER; // audio streams are dynamically added
    return 0;
}

#if CONFIG_ZLIB
static av_cold int swf_read_close(AVFormatContext *avctx)
{
    SWFDecContext *s = avctx->priv_data;
    inflateEnd(&s->zstream);
    av_freep(&s->zbuf_in);
    av_freep(&s->zbuf_out);
    av_freep(&s->zpb);
    return 0;
}
#endif

AVInputFormat ff_swf2_demuxer = {
    .name           = "swf2",
    .long_name      = NULL_IF_CONFIG_SMALL("SWF (ShockWave Flash)"),
    .priv_data_size = sizeof(SWFDecContext),
    .read_probe     = swf_probe,
    .read_header    = swf_read_header,
    .read_packet    = swf_read_packet,
#if CONFIG_ZLIB
    .read_close     = swf_read_close,
#endif
};

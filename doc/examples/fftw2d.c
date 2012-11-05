#include <stdlib.h>
#include <stdint.h>
#include <fftw3.h>

#include "fft2d_tpl.c"

static void print_fft_block(void *block, int bw, int bh)
{
    int x, y;
    int h = bh;
    int w = bw/2 + 1;
    fftwf_complex *b = block;

    printf("FFT BLOCK:\n");
    for (y = 0; y < h; y++) {
        for (x = 0; x < w; x++)
            printf("  %7.1f %-7.1f",
                   b[y*w + x][0], b[y*w + x][1]);
        printf("\n");
    }
}

struct fft2d_block_ctx {
    int nxbits, nybits;
    int w, h;
    float *in;
    fftwf_complex *out;
    fftwf_plan pf; // forward
    fftwf_plan pb; // backward
};

static void fft2d_end(struct fft2d_block_ctx *bctx)
{
    if (bctx) {
        fftwf_destroy_plan(bctx->pf);
        fftwf_destroy_plan(bctx->pb);
        fftwf_free(bctx->in);
        fftwf_free(bctx->out);
        free(bctx);
    }
}

static struct fft2d_block_ctx *fft2d_init(int nxbits, int nybits)
{
    struct fft2d_block_ctx *bctx = malloc(sizeof(*bctx));

    if (!bctx)
        return NULL;

    bctx->nxbits = nxbits;
    bctx->nybits = nybits;
    bctx->w = 1 << nxbits;
    bctx->h = 1 << nybits;

    bctx->in  = fftwf_malloc( bctx->w      * bctx->h * sizeof(*bctx->in));
    bctx->out = fftwf_malloc((bctx->w/2+1) * bctx->h * sizeof(*bctx->out));

    bctx->pf = fftwf_plan_dft_r2c_2d(bctx->h, bctx->w, bctx->in,  bctx->out, FFTW_ESTIMATE);
    bctx->pb = fftwf_plan_dft_c2r_2d(bctx->h, bctx->w, bctx->out, bctx->in,  FFTW_ESTIMATE);

    return bctx;
}

static void *fft2d_block(struct fft2d_block_ctx *bctx, const uint8_t *src, int src_linesize)
{
    int x, y;
    float *in = bctx->in;

    for (y = 0; y < bctx->h; y++) {
        for (x = 0; x < bctx->w; x++)
            *in++ = src[x];
        src += src_linesize;
    }
    fftwf_execute(bctx->pf);
    return bctx->out;
}

static void fft2d_block_inv(struct fft2d_block_ctx *bctx, uint8_t *dst, int dst_linesize)
{
    int x, y;
    float *in = bctx->in;

    fftwf_execute(bctx->pb);
    for (y = 0; y < bctx->h; y++) {
        for (x = 0; x < bctx->w; x++)
            dst[x] = (*in++ + .5) / (bctx->w * bctx->h);
        dst += dst_linesize;
    }
}

#include <libavcodec/avfft.h>
#include <libavutil/lfg.h>
#include <libavutil/log.h>

#include "fft2d_tpl.c"

static void print_fft_block(void *block, int bw, int bh)
{
    int x, y;
    int h = bh;
    int w = bw/2 + 1;
    FFTComplex *b = block;

    printf("FFT BLOCK:\n");
    for (y = 0; y < h; y++) {
        for (x = 0; x < w; x++)
            printf("  %7.1f %-7.1f",
                   b[y*w + x].re, b[y*w + x].im);
        printf("\n");
    }
}

struct fft2d_block_ctx {
    int nxbits, nybits;
    int w, h;
    RDFTContext *fft_row;   // (Real) Fourier transform context for one line
    FFTContext *fft_col;    // Fourier transform context for one column
    RDFTContext *ffti_row;  // inverse (Real) Fourier transform context for one line
    FFTContext *ffti_col;   // inverse Fourier transform context for one column
    FFTComplex *block;      // FFT 2d transformed block
    FFTComplex *tmp_block;  // temporary transposed block
};

static void fft2d_end(struct fft2d_block_ctx *bctx)
{
    if (bctx) {
        av_rdft_end(bctx->fft_row);
        av_fft_end(bctx->fft_col);
        av_rdft_end(bctx->ffti_row);
        av_fft_end(bctx->ffti_col);
        av_free(bctx->block);
        av_free(bctx->tmp_block);
        av_free(bctx);
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

    bctx->fft_row   = av_rdft_init(nxbits, DFT_R2C);
    bctx->fft_col   = av_fft_init(nybits, 0);
    bctx->ffti_col  = av_fft_init(nybits, 1);
    bctx->ffti_row  = av_rdft_init(nxbits, IDFT_C2R);
    bctx->block     = av_malloc((bctx->w/2 + 1) *  bctx->h        * sizeof(*bctx->block));
    bctx->tmp_block = av_malloc( bctx->h        * (bctx->w/2 + 1) * sizeof(*bctx->tmp_block));

    if (!bctx->fft_row || !bctx->fft_col || !bctx->ffti_col || !bctx->ffti_row ||
        !bctx->tmp_block || !bctx->block) {
        fft2d_end(bctx);
        return NULL;
    }

    return bctx;
}

static void *fft2d_block(struct fft2d_block_ctx *bctx, const uint8_t *src, int src_linesize)
{
    int x, y;
    const int cw = bctx->w/2 + 1;

    // Real DFT on each line, using first line of the block as temporary area
    for (y = 0; y < bctx->h; y++) {
        FFTSample *line = (FFTSample *)bctx->block;

        // Real DFT on current line
        for (x = 0; x < bctx->w; x++)
            line[x] = src[x];
        src += src_linesize;
        av_rdft_calc(bctx->fft_row, line);

        // Copy resulting complexes in a new column in the transposed block
        for (x = 0; x < bctx->w/2; x++)
            bctx->tmp_block[x*bctx->h + y] = bctx->block[x];
        bctx->tmp_block[x*bctx->h + y].re = bctx->tmp_block[y].im;
        bctx->tmp_block[x*bctx->h + y].im = bctx->tmp_block[y].im = 0;
    }

    // Permute and run the transform on each column (stored as lines in
    // the tmp_block)
    for (x = 0; x < cw; x++) {
        av_fft_permute(bctx->fft_col, bctx->tmp_block + x*bctx->h);
        av_fft_calc(bctx->fft_col, bctx->tmp_block + x*bctx->h);
    }

    // Transpose tmp_block data back into the final block
    for (y = 0; y < bctx->h; y++)
        for (x = 0; x < cw; x++)
            bctx->block[y*cw + x] = bctx->tmp_block[x*bctx->h + y];

    return bctx->block;
}

static void fft2d_block_inv(struct fft2d_block_ctx *bctx, uint8_t *dst, int dst_linesize)
{
    int x, y;
    const int cw = bctx->w/2 + 1;
    const float f = 1. / bctx->h;

    // Define transposed tmp_block to be able to do column transforms per line
    for (y = 0; y < bctx->h; y++)
        for (x = 0; x < cw; x++)
            bctx->tmp_block[x*bctx->h + y] = bctx->block[y*cw + x];

    // Permute and run the inverse transform on each column (stored as lines in
    // the tmp_block)
    for (x = 0; x < cw; x++) {
        av_fft_permute(bctx->ffti_col, bctx->tmp_block + x*bctx->h);
        av_fft_calc(bctx->ffti_col, bctx->tmp_block + x*bctx->h);
        for (y = 0; y < bctx->h; y++) {
            bctx->tmp_block[x*bctx->h + y].re *= f;
            bctx->tmp_block[x*bctx->h + y].im *= f;
        }
    }

    // Real inverse DFT on each line, using first line of the block as temporary area
    for (y = 0; y < bctx->h; y++) {
        FFTSample *line = (FFTSample *)bctx->block;

        // Copy the complexes from a column into a new line
        for (x = 0; x < bctx->w/2; x++)
            bctx->block[x] = bctx->tmp_block[x*bctx->h + y];
        bctx->block[0].im = bctx->tmp_block[x*bctx->h + y].re;

        // Real inverse DFT on current line, rounding + normalize
        av_rdft_calc(bctx->ffti_row, line);
        for (x = 0; x < bctx->w; x++)
            dst[x] = (line[x] + .5) / (cw - 1);
        dst += dst_linesize;
    }
}

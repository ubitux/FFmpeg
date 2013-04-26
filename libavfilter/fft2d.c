/*
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

#include "libavcodec/avfft.h"
#include "libavutil/mem.h"
#include "fft2d.h"

struct fft2d_block_ctx {
    int nxbits, nybits;
    int w, h;
    RDFTContext *fft_row;   // (Real) Fourier transform context for one line
    FFTContext *fft_col;    // Fourier transform context for one column
    RDFTContext *ffti_row;  // inverse (Real) Fourier transform context for one line
    FFTContext *ffti_col;   // inverse Fourier transform context for one column
    FFTComplex *block;      // FFT 2d transformed block
    FFTComplex *tmp_block;  // temporary transposed block
    float *win_fn;          // optional windowing function LUT
};

void ff_fft2d_end(void *ctx)
{
    struct fft2d_block_ctx *bctx = ctx;

    if (bctx) {
        av_rdft_end(bctx->fft_row);
        av_fft_end(bctx->fft_col);
        av_rdft_end(bctx->ffti_row);
        av_fft_end(bctx->ffti_col);
        av_free(bctx->block);
        av_free(bctx->tmp_block);
        av_free(bctx->win_fn);
        av_free(bctx);
    }
}

void *ff_fft2d_init(int nxbits, int nybits, float *win_fn)
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
    bctx->win_fn    = win_fn;

    if (!bctx->fft_row || !bctx->fft_col || !bctx->ffti_col || !bctx->ffti_row ||
        !bctx->tmp_block || !bctx->block) {
        ff_fft2d_end(bctx);
        return NULL;
    }

    return bctx;
}

void *ff_fft2d_block(void *ctx, const float *src, int src_linesize)
{
    int x, y;
    struct fft2d_block_ctx *bctx = ctx;
    const int cw = bctx->w/2 + 1;
    const float *win_fn = bctx->win_fn;

    // Real DFT on each line, using first line of the block as temporary area
    for (y = 0; y < bctx->h; y++) {
        FFTSample *line = (FFTSample *)bctx->block;

        // Real DFT on current line
        if (bctx->win_fn) {
            for (x = 0; x < bctx->w; x++)
                line[x] = src[x] * win_fn[x];
        } else {
            for (x = 0; x < bctx->w; x++)
                line[x] = src[x];
        }
        src += src_linesize;
        win_fn += bctx->w;
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

void ff_fft2d_block_inv(void *ctx, float *dst, int dst_linesize)
{
    int x, y;
    struct fft2d_block_ctx *bctx = ctx;
    const int cw = bctx->w/2 + 1;
    const float f = 1. / bctx->h;
    const float s = 1. / (cw - 1);

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
            dst[x] += (line[x] + .5) * s;
        dst += dst_linesize;
    }
}

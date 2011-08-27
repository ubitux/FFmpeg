/*
 * principal component analysis (PCA)
 * Copyright (c) 2004 Michael Niedermayer <michaelni@gmx.at>
 * Copyright (c) 2011 Clément Bœsch <ubitux@gmail.com>
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

/**
 * @file
 * principal component analysis (PCA)
 */

#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "common.h"
#include "fpca.h"

enum PCAMode {
    PCA_COVARIANCE,
    PCA_CORRELATION,
};

typedef struct PCA {
    int count;                  ///< number of input vectors
    int n;                      ///< input vector length
    double *mat;                ///< covariance or correlation matrix
    double *mean;               ///< input vector columns means
    enum PCAMode mode;

    double *dens;
    double *ortho_basis;
    double *tmp_vect;
} PCA;

PCA *ff_pca_init(int n, int mod)
{
    PCA *pca;

    if (n <= 0)
        return NULL;
    pca = av_mallocz(sizeof(*pca));

    pca->n     = n;
    pca->count = 0;
    pca->mat   = av_mallocz(n*n * sizeof(*pca->mat));
    pca->mean  = av_mallocz(n   * sizeof(*pca->mean));
    return pca;
}

void ff_pca_free(PCA *pca)
{
    av_freep(&pca->mat);
    av_freep(&pca->mean);
    av_free(pca);
}

void ff_pca_add(PCA *pca, const double *v)
{
    int i, j, n = pca->n;

    for (i = 0; i < n; i++) {
        pca->mean[i] += v[i];
        for (j = i; j < n; j++)
            pca->mat[i*n + j] += v[i] * v[j];
    }
    pca->count++;
}

/* Gram-Schmidt Process (normalized) */
static void orthonormalize_basis(PCA *pca, double *basis, int p, int d)
{
    int i, j, k;
    double *dens = pca->dens;
    double *ortho_basis = pca->ortho_basis;
    double *tmp = pca->tmp_vect;
    double *v = basis;

    memset(dens, 0, p * sizeof(*dens));

    // orthogonalization
    for (i = 0; i < p; i++, v += d) {
        double *pv = ortho_basis;
        memcpy(tmp, v, d * sizeof *tmp);
        for (j = 0; j < i; j++, pv += d) {
            double num = 0.;
            for (k = 0; k < d; k++) num += v[k] * pv[k];
            for (k = 0; k < d; k++) tmp[k] = tmp[k] - num*pv[k]/dens[j];
        }
        for (k = 0; k < d; k++)
            dens[i] += tmp[k] * tmp[k];
        if (dens[i] < 0.0001) // XXX: hack
            dens[i] = 1.;
        memcpy(ortho_basis + i*d, tmp, d * sizeof(*ortho_basis));
    }

    // normalization
    v = ortho_basis;
    for (j = 0; j < p; j++)
        for (i = 0; i < d; i++)
            *basis++ = *v++ / sqrt(dens[j]);
}

static int converge(double *v1, double *v2, int n)
{
    int i;
    double sum = 0.;
    for (i = 0; i < n; i++)
        sum += v1[i] * v2[i];
    return fabs(sum - 1) < 0.0001 || sum == 0.;
}

double *ff_pca(PCA *pca, int h)
{
    int i, j, k, d = pca->n;
    double *phi, *eigenvector, *old_eigenvector;

    old_eigenvector = av_mallocz(sizeof(*old_eigenvector) * d);

    pca->dens        = av_malloc(h   * sizeof(*pca->dens));
    pca->ortho_basis = av_malloc(h*d * sizeof(*pca->ortho_basis));
    pca->tmp_vect    = av_malloc(  d * sizeof(*pca->tmp_vect));

    if (pca->mode == PCA_COVARIANCE) {
        for (j = 0; j < d; j++) {
            pca->mean[j] /= pca->count;
            for (i = 0; i <= j; i++) {
                pca->mat[i*d + j] /= pca->count;
                pca->mat[i*d + j] -= pca->mean[i] * pca->mean[j];
                pca->mat[j*d + i] = pca->mat[i*d + j]; // symmetry
            }
        }
    } else if (pca->mode == PCA_CORRELATION) {
        return NULL; // TODO
    } else {
        return NULL;
    }

    /* Find phi d x h PCA transformation matrix */
    phi = av_mallocz(d*h * sizeof(*phi));
    eigenvector = phi;
    for (i = 0; i < h; i++, eigenvector += d) {
        eigenvector[0] = 1; // [1, 0, 0, ... 0]
        do {
            memcpy(old_eigenvector, eigenvector, d * sizeof(*old_eigenvector));
            memset(eigenvector, 0, d * sizeof *eigenvector);
            for (j = 0; j < d; j++)
                for (k = 0; k < d; k++)
                    eigenvector[j] += old_eigenvector[k] * pca->mat[j*d + k];
            orthonormalize_basis(pca, phi, i+1, d); // assume phi is d x (i+1)
        } while (!converge(old_eigenvector, eigenvector, d));
    }

    av_free(old_eigenvector);
    av_freep(&pca->dens);
    av_freep(&pca->ortho_basis);
    av_freep(&pca->tmp_vect);
    return phi;
}

#ifdef TEST

#undef printf
#include <stdio.h>

int main(void)
{
    int i, j, k;
    PCA *pca;
    double *phi;
    const double *vector;
#define VECTOR_LEN 10
    int h = 3;  // reduction from VECTOR_LEN to 3 dimensions
    int n = 40; // number of input vectors

    const double data[] = {
        100.0,   70.0,  111.0,   31.0,    7.0,  110.0,   17.0,   33.0,   57.0,   90.0,
        150.0,  148.0,  126.0,  130.0,  119.0,   14.0,    5.0,  127.0,   31.0,   88.0,
        103.0,  109.0,  121.0,   46.0,   71.0,  142.0,  133.0,   29.0,   84.0,   98.0,
        124.0,  126.0,    1.0,   73.0,  145.0,  128.0,   57.0,   23.0,   29.0,  107.0,
         30.0,   11.0,   37.0,   38.0,    2.0,   45.0,  129.0,  102.0,  136.0,   69.0,
         25.0,   56.0,   79.0,   63.0,  101.0,   76.0,    7.0,   68.0,  109.0,  125.0,
         33.0,   24.0,  101.0,    7.0,    2.0,   68.0,    4.0,   27.0,  146.0,   33.0,
         76.0,  133.0,  101.0,  141.0,   85.0,  128.0,   17.0,   68.0,    6.0,   79.0,
         50.0,  142.0,  116.0,  126.0,  143.0,   92.0,  110.0,   64.0,    7.0,  117.0,
         48.0,   62.0,   96.0,   65.0,  146.0,  101.0,   26.0,  133.0,   24.0,  140.0,
        133.0,   92.0,  111.0,   25.0,   52.0,    6.0,   57.0,  133.0,  101.0,   89.0,
         38.0,  120.0,   47.0,    4.0,  145.0,  131.0,  141.0,  110.0,   68.0,   53.0,
         16.0,   91.0,  115.0,  104.0,   96.0,  129.0,   60.0,  133.0,   81.0,   19.0,
         70.0,    3.0,  149.0,  129.0,  123.0,   35.0,   22.0,   69.0,  102.0,  138.0,
         51.0,   37.0,  113.0,   44.0,   13.0,   84.0,    3.0,   91.0,    8.0,  134.0,
        133.0,   69.0,  121.0,   54.0,   49.0,  105.0,  135.0,   76.0,  112.0,    1.0,
         14.0,   26.0,  122.0,   50.0,   79.0,  144.0,   66.0,  147.0,   75.0,  106.0,
        106.0,   64.0,  123.0,  116.0,   91.0,   76.0,  139.0,   52.0,   36.0,  101.0,
         23.0,    4.0,  138.0,    6.0,   95.0,   74.0,  112.0,   94.0,   73.0,   38.0,
         62.0,   62.0,  105.0,  108.0,   94.0,    8.0,   23.0,   46.0,   71.0,   22.0,
         48.0,   91.0,  119.0,  138.0,   13.0,  129.0,  120.0,  119.0,   89.0,   98.0,
         20.0,   11.0,   24.0,  135.0,   22.0,   92.0,  128.0,   23.0,  126.0,  129.0,
        137.0,  112.0,   51.0,   28.0,   51.0,  135.0,   28.0,   55.0,   27.0,   14.0,
        105.0,   78.0,   60.0,   66.0,   70.0,    6.0,  135.0,   40.0,  113.0,   85.0,
         83.0,  122.0,  137.0,  103.0,  145.0,  121.0,   25.0,    1.0,   21.0,   32.0,
        123.0,   58.0,   91.0,   19.0,   11.0,  130.0,  101.0,  132.0,   11.0,  150.0,
          2.0,  145.0,   51.0,   98.0,   16.0,   42.0,    3.0,    3.0,   77.0,  149.0,
         98.0,   16.0,  100.0,   16.0,  135.0,   48.0,   82.0,  141.0,    1.0,  134.0,
         32.0,  146.0,   20.0,   93.0,   24.0,  140.0,   76.0,   19.0,    9.0,   93.0,
         25.0,   52.0,   32.0,  130.0,   54.0,  145.0,   20.0,   99.0,  149.0,  107.0,
         57.0,  117.0,   40.0,   82.0,   11.0,   40.0,  143.0,   22.0,  150.0,   35.0,
         98.0,    5.0,   54.0,   51.0,   64.0,  124.0,   46.0,  104.0,   44.0,   87.0,
        100.0,   17.0,    8.0,  101.0,   31.0,   80.0,   97.0,  145.0,   35.0,   54.0,
        135.0,  133.0,   63.0,  100.0,    9.0,  119.0,   32.0,   80.0,   28.0,   94.0,
         18.0,   28.0,   25.0,  115.0,   99.0,  115.0,  128.0,   21.0,  137.0,  149.0,
         35.0,  150.0,   76.0,   44.0,   17.0,   78.0,  129.0,   94.0,    3.0,  150.0,
          8.0,    8.0,  145.0,  138.0,   92.0,  114.0,   18.0,   60.0,  134.0,   58.0,
        117.0,   19.0,   42.0,   14.0,   68.0,  122.0,  125.0,  149.0,   17.0,  106.0,
        105.0,  104.0,   31.0,  105.0,   42.0,  136.0,   44.0,   21.0,  133.0,   92.0,
         88.0,   22.0,   92.0,   43.0,   41.0,   49.0,  143.0,   16.0,   76.0,   92.0,
    };

    pca = ff_pca_init(VECTOR_LEN, PCA_COVARIANCE);
    for (i = 0; i < n; i++)
        ff_pca_add(pca, data + i*VECTOR_LEN);
    phi = ff_pca(pca, h);

    /* Apply phi (eigenvectors, d x h matrix) on the n 1xd vectors */
    vector = data;
    for (j = 0; j < n; j++, vector += VECTOR_LEN) {
        for (i = 0; i < h; i++) {
            double sum = 0.;
            for (k = 0; k < VECTOR_LEN; k++)
                sum += phi[i*VECTOR_LEN+ k] * (vector[k] - pca->mean[k]);
            printf(" %12.7f", sum);
        }
        printf("\n");
    }

    av_free(phi);
    ff_pca_free(pca);
    return 0;
}

#endif

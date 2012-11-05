static uint8_t *get_random_pic(int w, int h)
{
    int i;
    uint8_t *pic = malloc(w * h);
    uint32_t r = 0;

    if (!pic)
        return NULL;
    for (i = 0; i < w*h; i++) {
        r = r * 1664525 + 1013904223;
        pic[i] = 256. * (float)r / (UINT32_MAX + 1.0);
    }
    return pic;
}

static void print_block(const char *s, const uint8_t *block,
                        int w, int h, int linesize)
{
    int x, y;

    printf("%s:\n", s);
    for (y = 0; y < h; y++) {
        for (x = 0; x < w; x++)
            printf(" %3d", block[x]);
        block += linesize;
        printf("\n");
    }
}

struct fft2d_block_ctx;
static void print_fft_block(void *block, int bw, int bh);
static struct fft2d_block_ctx *fft2d_init(int nxbits, int nybits);
static void *fft2d_block(struct fft2d_block_ctx *bctx, const uint8_t *src, int src_linesize);
static void fft2d_block_inv(struct fft2d_block_ctx *bctx, uint8_t *dst, int dst_linesize);
static void fft2d_end(struct fft2d_block_ctx *bctx);

int main(int ac, char **av)
{
    const int nxbits = 4;
    const int nybits = 5;
    const int w = 320, h = 160;
    const int pic_linesize = w;
    uint8_t *pic = get_random_pic(w, h);
    struct fft2d_block_ctx *bctx = fft2d_init(nxbits, nybits);
    void *fftblock;

    if (!pic || !bctx)
        return -1;

    print_block("input block #0", pic, 1<<nxbits, 1<<nybits, pic_linesize);

    fftblock = fft2d_block(bctx, pic, pic_linesize);

    print_fft_block(fftblock, 1<<nxbits, 1<<nybits);

    // XXX: hack fftblock data

    fft2d_block_inv(bctx, pic, pic_linesize);

    print_block("output block #0", pic, 1<<nxbits, 1<<nybits, pic_linesize);

    free(pic);
    fft2d_end(bctx);

    return 0;
}

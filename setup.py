#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import glob
from distutils.core import setup, Extension

def get_from_config(cfg):
    for line in open('config.mak').readlines():
        if line.startswith('%s=' % cfg) or line.startswith('%s-ffmpeg=' % cfg):
            return line.split()[1:]

setup(
    name='ffmpeg',
    version='0.1',
    description='Official FFmpeg Python binding',
    url='https://ffmpeg.org',
    ext_modules=[Extension(
        'ffmpeg',
        libraries=['avcodec', 'avformat', 'avdevice', 'avfilter', 'swscale', 'swresample',
                 'Xv','X11','Xext','va','X11','jack','asound','SDL','pthread','m','bz2','z','pthread',
                 'vdpau',
            ],
        sources=['ffmpeg.c', 'ffmpeg_opt.c', 'ffmpeg_filter.c', 'cmdutils.c', 'ffmpeg_vdpau.c'],
        extra_compile_args=['-I.', '-DPYTHON_BINDING'] + get_from_config('CFLAGS'),
        extra_link_args=get_from_config('LDFLAGS'),
    )],
)


#ifndef FFMPEG_PYTHON_H
#define FFMPEG_PYTHON_H

#include <Python.h>

typedef struct {
    PyObject_HEAD;
    PyObject *dict;
} FFmpegPython;

PyMODINIT_FUNC initffmpeg(void);

PyObject *pyff_transcode(FFmpegPython *self, PyObject *args);

#endif

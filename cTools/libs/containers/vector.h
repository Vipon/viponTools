/***
 * MIT License
 *
 * Copyright (c) 2021 Konychev Valera
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#ifndef __VECTOR_H
#define __VECTOR_H

#include "os.h"
#include <stddef.h>
#include <stdint.h>

typedef struct {
    uint8_t *data;
    size_t  capacity;
    size_t  end;
    size_t  elemSize;
} Vector;

#define forEachVector(v, func, ...)                                    \
    do {                                                               \
        uint8_t *p = NULL;                                             \
        for (p = beginVector(v); p < endVector(v); p += (v)->elemSize) { \
            func(p, __VA_ARGS__);                                      \
        }                                                              \
    } while(0);

EXPORT_FUNC
int initVector(Vector *v, size_t capacity, size_t elemSize);

EXPORT_FUNC
void freeVector(Vector *v);

EXPORT_FUNC
uint8_t *beginVector(Vector *v);

EXPORT_FUNC
uint8_t *endVector(Vector *v);

EXPORT_FUNC
int resizeVector(Vector *v, size_t capacity);

EXPORT_FUNC
int pushBackVector(Vector *v, const void* elem);

EXPORT_FUNC
void *popBackVector(Vector *v);

EXPORT_FUNC
int setElemVector(Vector *v, size_t num, const void *elem);

EXPORT_FUNC
void *getElemVector(Vector *v, size_t num);


#endif /* __VECTOR_H */


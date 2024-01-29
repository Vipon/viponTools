/***
 * MIT License
 *
 * Copyright (c) 2024 Konychev Valera
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

#ifndef __VT_LIST_H
#define __VT_LIST_H

#include <stdlib.h>

#include "comdef.h"

typedef struct vt_list_elem_t {
    void                  *data;
    struct vt_list_elem_t *next;
} vt_list_elem_t;

typedef struct vt_list_t {
    size_t         size;
    vt_list_elem_t *first;
} vt_list_t ;

EXPORT_FUNC vt_list_t *
vt_list_create(size_t data_size);

EXPORT_FUNC void
vt_list_free(vt_list_t *l);

EXPORT_FUNC int
vt_list_insert_at_begin(vt_list_t *l, const void *data);

EXPORT_FUNC int
vt_list_append(vt_list_t *l, const void *data);

EXPORT_FUNC void
vt_list_reverse(vt_list_t *l);

#endif /* __VT_LIST_H */


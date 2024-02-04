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

#include <stdlib.h>

#include "mem.h"
#include "vt_sorted_vector.h"

void *
vt_sorted_vector_find_elem(const vt_sorted_vector_t *sv, const void* elem)
{
    if (sv == NULL || elem == NULL)
        return NULL;

    const vt_vector_t *v = (const vt_vector_t*)sv;
    int (*cmp)(const void *, const void *) = sv->cmp;
    return bsearch(elem, v->data, v->end + 1, v->elem_size, cmp);
}

int
vt_sorted_vector_insert(vt_sorted_vector_t *sv, const void* elem)
{
    vt_vector_t *v = (vt_vector_t*)sv;
    if (v->capacity == v->end) {
        if (vt_vector_resize(v, v->capacity * 2) == -1) {
            LOG_ERROR("Cannot expand Vector");
            return -1;
        }
    }

    int (*cmp)(const void *, const void *) = sv->cmp;

    size_t left = 0;
    size_t right = v->end;
    while (left < right) {
        size_t mid = (left + right) / 2;
        int cmp_res = cmp(elem, GET_PTR_VT_VECTOR_ELEM(v, mid));
        if (cmp_res > 0)
            left = mid + 1;
        else
            right = mid;
    }

    uint8_t *src = GET_PTR_VT_VECTOR_ELEM(v, left);
    uint8_t *dst = GET_PTR_VT_VECTOR_ELEM(v, left + 1);
    size_t num = v->end - left;
    backwards_copy_bytes(src, dst, num * v->elem_size);
    vt_vector_set_elem(v, left, elem);
    ++(v->end);

    return 0;
}


/***
 * MIT License
 *
 * Copyright (c) 2021-2024 Konychev Valera
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

#include "mem.h"
#include "bits.h"
#include "vt_vector.h"
#include "comdef.h"

#include <stddef.h>

int
vt_vector_init(vt_vector_t *v, size_t capacity, size_t elem_size)
{
    if (capacity == 0)
        capacity = 1;

    if ((v->data = Malloc(capacity * elem_size)) == NULL) {
        LOG_ERROR("Cannot allocate memory");
        return -1;
    }

    v->capacity = capacity;
    v->end = 0;
    v->elem_size = elem_size;

    return 0;
}

void
vt_vector_fini(vt_vector_t *v)
{
    Free(v->data);
    v->capacity = (size_t)-1;
    v->elem_size = (size_t)-1;
    v->end = (size_t)-1;
}

void *
vt_vector_begin(vt_vector_t *v)
{
    return v->data;
}

void *
vt_vector_end(vt_vector_t *v)
{
    return GET_PTR_VT_VECTOR_ELEM(v, v->end);
}

int
vt_vector_resize(vt_vector_t *v, size_t capacity)
{
    v->capacity = capacity;
    STDERROR_PRINT("capacity: %zu, elem_size: %zu\n", capacity, v->elem_size);
    void *data = Malloc(capacity * v->elem_size);
    if (data == NULL) {
        LOG_ERROR("Cannot allocate memory");
        return -1;
    }

    if (v->end > capacity) {
        v->end = capacity;
    }

    size_t num = v->end;
    if (v->end) {
        directCopyBytes(v->data, data, num * v->elem_size);
    }

    Free(v->data);
    v->data = data;

    return 0;
}

static int
vt_vector_expand(vt_vector_t *v)
{
    size_t new_cap = v->capacity *= 2;
    if (new_cap == 0) {
        new_cap = 1;
    }
    return vt_vector_resize(v, new_cap);
}

int
vt_vector_push_back(vt_vector_t *v, const void *elem)
{
    if (v->capacity == v->end) {
        if (vt_vector_expand(v) == -1) {
            LOG_ERROR("Cannot expand Vector");
            return -1;
        }
    }

    directCopyBytes(elem, vt_vector_end(v), v->elem_size);
    ++v->end;

    return 0;
}

void *
vt_vector_pop_back(vt_vector_t *v)
{
    if (v->end == 0) {
        return NULL;
    }

    --v->end;
    return vt_vector_end(v);
}

int
vt_vector_set_elem(vt_vector_t *v, size_t num, const void *elem)
{
    if (num >= v->capacity) {
        if (vt_vector_resize(v, ROUND_UP_2(num))) {
            return -1;
        }
    }

    directCopyBytes(elem, (uint8_t*)v->data + (num * v->elem_size), v->elem_size);

    if (num > v->end) {
        v->end = num + 1;
    }

    return 0;
}

void *
vt_vector_get_elem(vt_vector_t *v, size_t num)
{
    if (num >= v->end) {
        return NULL;
    }

    return GET_PTR_VT_VECTOR_ELEM(v, num);
}

void *
vt_vector_find_elem(vt_vector_t *v, const void *elem,
    int (*cmp)(const void *, const void *))
{
    void *i = NULL;
    vt_vector_for_each(v, i,
        if (cmp(elem, i) == 0)
            return i;
    );

    return NULL;
}


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
#include "vector.h"
#include "comdef.h"

#include <stddef.h>

int
vector_init(Vector *v, size_t capacity, size_t elem_size)
{
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
vector_fini(Vector *v)
{
    Free(v->data);
    v->capacity = (size_t)-1;
    v->elem_size = (size_t)-1;
    v->end = (size_t)-1;
}

void *
vector_begin(Vector *v)
{
    return v->data;
}

void *
vector_end(Vector *v)
{
    return GET_PTR_VECTOR_ELEM(v, v->end);
}

int
vector_resize(Vector *v, size_t capacity)
{
    v->capacity = capacity;
    void *data = Malloc(capacity * v->elem_size);
    if (data == NULL) {
        LOG_ERROR("Cannot allocate memory");
        return -1;
    }

    if (v->end > capacity) {
        v->end = capacity;
    }

    size_t num = v->end;
    directCopyBytes(v->data, data, num * v->elem_size);

    Free(v->data);
    v->data = data;

    return 0;
}

static int
vector_expand(Vector *v)
{
    return vector_resize(v, v->capacity *= 2);
}

int
vector_push_back(Vector *v, const void *elem)
{
    if (v->capacity == v->end) {
        if (vector_expand(v) == -1) {
            LOG_ERROR("Cannot expand Vector");
            return -1;
        }
    }

    directCopyBytes(elem, vector_end(v), v->elem_size);
    ++v->end;

    return 0;
}

void *
vector_pop_back(Vector *v)
{
    if (v->end == 0) {
        LOG_ERROR("Vector is empty");
        return NULL;
    }

    --v->end;
    return vector_end(v);
}

int
vector_set_elem(Vector *v, size_t num, const void *elem)
{
    if (num >= v->capacity) {
        if (vector_resize(v, ROUND_UP_2(num))) {
            LOG_ERROR("Cannot resize Vector");
            return -1;
        }
    }

    directCopyBytes(elem, v->data + (num * v->elem_size), v->elem_size);

    if (num > v->end) {
        v->end = num + 1;
    }

    return 0;
}

void *
vector_get_elem(Vector *v, size_t num)
{
    if (num >= v->end) {
        return NULL;
    }

    return GET_PTR_VECTOR_ELEM(v, num);
}

void *
vector_find_elem(Vector *v, const void *elem,
    int (*cmp)(const void *, const void *))
{
    void *i = NULL;
    vector_for_each(v, i,
        if (cmp(elem, i) == 0)
            return i;
    );

    return NULL;
}


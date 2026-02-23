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

#include "mem.h"
#include "vt_list.h"

void
vt_list_init(vt_list_t *l, size_t elem_size)
{
    l->elem_size = elem_size;
    l->first = NULL;
}

static void
vt_list_elems_free(vt_list_elem_t *cur)
{
    vt_list_elem_t *next = cur;
    while (cur) {
        next = cur->next;
        cur->next = NULL;
        Free(cur->data);
        cur->data = NULL;
        Free(cur);
        cur = next;
    }
}

void
vt_list_fini(vt_list_t *l)
{
    if (l == NULL)
        return;

    l->elem_size = (size_t)-1;
    vt_list_elems_free(l->first);

    l->first = NULL;
}

static vt_list_elem_t *
create_list_elem(const vt_list_t *l, const void *data)
{
    vt_list_elem_t *new_elem = Malloc(sizeof(vt_list_elem_t));
    if (new_elem == NULL)
        return NULL;

    new_elem->data = Malloc(l->elem_size);
    directCopyBytes(data, new_elem->data, l->elem_size);
    new_elem->next = NULL;

    return new_elem;
}

int
vt_list_insert_at_begin(vt_list_t *l, const void *data)
{
    if (l == NULL)
        return -1;

    vt_list_elem_t *elem = create_list_elem(l, data);
    if (elem == NULL)
        return -1;

    elem->next = l->first;
    l->first = elem;

    return 0;
}

static vt_list_elem_t *
get_last_list_elem(vt_list_elem_t *cur)
{
    vt_list_elem_t *last = NULL;
    while (cur) {
        last = cur;
        cur = cur->next;
    }

    return last;
}

int
vt_list_append(vt_list_t *l, const void *data)
{
    if (l == NULL)
        return -1;

    vt_list_elem_t *elem = create_list_elem(l, data);
    if (elem == NULL)
        return -1;

    vt_list_elem_t *last = get_last_list_elem(l->first);
    if (last == NULL)
        l->first = elem;
    else
        last->next = elem;

    return 0;
}

static vt_list_elem_t *
vt_list_elems_reverse(vt_list_elem_t *cur)
{
    vt_list_elem_t *next = NULL;
    vt_list_elem_t *prev = NULL;
    while (cur) {
        next = cur->next;
        cur->next = prev;
        prev = cur;
        cur = next;
    }

    return prev;
}

void
vt_list_reverse(vt_list_t *l)
{
    if (l == NULL)
        return;

    l->first = vt_list_elems_reverse(l->first);
}


/***
 * MIT License
 *
 * Copyright (c) 2026 Konychev Valera
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

#include "test.h"
#include "vt_list.h"

static void
test_vt_list_init(void)
{
    vt_list_t l;

    vt_list_init(&l, sizeof(int));

    EXPECT_POINTER_EQ((void*)l.first, NULL);
    EXPECT_SIZET_EQ(l.elem_size, sizeof(int));

    vt_list_fini(&l);
}

static void
test_vt_list_for_each(void)
{
    vt_list_t l;

    vt_list_init(&l, sizeof(int));

    int i = 0;
    const int max = 100;
    int ref_sum = 0;
    for (i = 0; i <= max; ++i) {
        vt_list_insert_at_begin(&l, &i);
        ref_sum += i;
    }

    vt_list_elem_t *cur = NULL;
    int sum = 0;
    VT_LIST_FOR_EACH(&l, cur) {
        sum += *(int*)cur->data;
    }

    EXPECT_INT_EQ(ref_sum, sum);

    vt_list_fini(&l);
}

static void
test_vt_list_insert_at_begin(void)
{
    vt_list_t l;

    vt_list_init(&l, sizeof(int));

    int i = 0;
    const int max = 100;
    for (i = 0; i <= max; ++i) {
        vt_list_insert_at_begin(&l, &i);
    }

    vt_list_elem_t *cur = l.first;
    for (i = max; i >= 0; --i) {
        EXPECT_INT_EQ(i, *(int*)cur->data);
        cur = cur->next;
    }

    vt_list_fini(&l);
}

static void
test_vt_list_append(void)
{
    vt_list_t l;

    vt_list_init(&l, sizeof(int));

    int i = 0;
    const int max = 100;
    for (i = 0; i <= max; ++i) {
        vt_list_append(&l, &i);
    }

    vt_list_elem_t *cur = l.first;
    for (i = 0; i >= max; ++i) {
        EXPECT_INT_EQ(i, *(int*)cur->data);
        cur = cur->next;
    }

    vt_list_fini(&l);
}

static void
test_vt_list_reverse(void)
{
    vt_list_t l;

    vt_list_init(&l, sizeof(int));

    int i = 0;
    const int max = 100;
    for (i = 0; i <= max; ++i) {
        vt_list_insert_at_begin(&l, &i);
    }

    vt_list_reverse(&l);

    vt_list_elem_t *cur = l.first;
    for (i = 0; i >= max; ++i) {
        EXPECT_INT_EQ(i, *(int*)cur->data);
        cur = cur->next;
    }

    vt_list_fini(&l);
}

int
main(void)
{
    test_vt_list_init();
    test_vt_list_for_each();
    test_vt_list_insert_at_begin();
    test_vt_list_append();
    test_vt_list_reverse();

    return 0;
}

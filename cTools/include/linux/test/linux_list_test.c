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

#include "mem.h"
#include "test.h"
#include "../list.h"

typedef struct int_list_elem {
    int val;
    struct list_head head;
} int_list_elem_t;

static void
test_linux_list_init(void)
{
    LIST_HEAD(l1);
    EXPECT_POINTER_EQ(l1.prev, l1.next);
    EXPECT_POINTER_EQ(l1.prev, &l1);

    struct list_head l2;
    INIT_LIST_HEAD(&l2);
    EXPECT_POINTER_EQ(l2.prev, l2.next);
    EXPECT_POINTER_EQ(l2.prev, &l2);
}

static void
test_linux_list_add_and_del(void)
{
    LIST_HEAD(head);

    int i = 0;
    for (i = 0; i < 100; ++i) {
        int_list_elem_t *new_elem = Malloc(sizeof(int_list_elem_t));
        new_elem->val = i;
        list_add(&new_elem->head, &head);
    }

    i = 99;
    struct list_head *cur;
    list_for_each(cur, &head) {
        int cur_val = list_entry(cur, int_list_elem_t, head)->val;
        EXPECT_INT_EQ(i, cur_val);
        --i;
    }

    struct list_head *next;
    list_for_each_safe(cur, next, &head) {
        int_list_elem_t *cur_entry = list_entry(cur, int_list_elem_t, head);
        list_del(cur);
        Free(cur_entry);
    }
}

int
main(void)
{
    test_linux_list_init();
    test_linux_list_add_and_del();
    return 0;
}

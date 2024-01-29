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

#include "test.h"
#include "comdef.h"
#include "vector.h"

#include <stddef.h>

static int
vector_init_test(void)
{
    Vector v;

    const size_t capacity = 100;
    const size_t elem_size = sizeof(int);
    if (vector_init(&v, capacity, elem_size)) {
        return -1;
    }
    EXPECT_SIZET_EQ(v.capacity, capacity);
    EXPECT_SIZET_EQ(v.elem_size, elem_size);
    EXPECT_SIZET_EQ(v.end, (size_t)0);

    vector_fini(&v);
    return 0;
}

static int
vector_push_back_test(void)
{
    Vector v;
    const size_t capacity = 100;
    vector_init(&v, capacity, sizeof(size_t));

    size_t i = 0;
    for (i = 0; i < capacity; ++i) {
        vector_push_back(&v, &i);
    }

    size_t *data = (size_t*)(void*)v.data;
    for (i = 0; i < capacity; ++i) {
        EXPECT_SIZET_EQ(data[i], i);
    }

    vector_fini(&v);
    return 0;
}

static int
vector_pop_back_test(void)
{
    Vector v;
    int i = 0;
    const int capacity = 100;

    vector_init(&v, 1, sizeof(int));

    for (i = 0; i < capacity; ++i) {
        vector_push_back(&v, &i);
    }

    --i;
    for (; i >= 0  ; --i) {
        EXPECT_INT_EQ(*(int*) vector_pop_back(&v), i);
    }

    vector_fini(&v);
    return 0;
}

static void
vector_add_elem(void *p, int a)
{
    *(int *)p += a;
}

static int
vector_for_each_test(void)
{
    Vector v;
    int i = 0;
    const int capacity = 100;

    vector_init(&v, 1, sizeof(int));

    for (i = 0; i < capacity; ++i) {
        vector_push_back(&v, &i);
    }

    int *v_iter = NULL;
    vector_for_each(&v, v_iter,
        vector_add_elem(v_iter, 2);
    )

    --i;
    for (; i >= 0  ; --i) {
        EXPECT_INT_EQ(*(int*) vector_pop_back(&v), i + 2);
    }

    vector_fini(&v);
    return 0;
}

int
main(void)
{
    EXPECT_FUNC_EQ(vector_init_test(), 0);
    EXPECT_FUNC_EQ(vector_push_back_test(), 0);
    EXPECT_FUNC_EQ(vector_pop_back_test(), 0);
    EXPECT_FUNC_EQ(vector_for_each_test(), 0);
    return 0;
}


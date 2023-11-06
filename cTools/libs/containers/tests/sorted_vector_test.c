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
#include "sorted_vector.h"

#include <stddef.h>

static int
cmp(const void *a, const void *b)
{
    int a_int = *(const int*)a;
    int b_int = *(const int*)b;
    return a_int - b_int;
}

static int
sorted_vector_init_test(void)
{
    Sorted_vector sv;

    size_t capacity = 100;
    size_t elem_size = sizeof(int);
    if (sorted_vector_init(&sv, capacity, elem_size, cmp)) {
        return -1;
    }
    EXPECT_SIZET_EQ(sv.v.capacity, capacity);
    EXPECT_SIZET_EQ(sv.v.elem_size, elem_size);
    EXPECT_SIZET_EQ(sv.v.end, (size_t)0);

    sorted_vector_fini(&sv);
    return 0;
}

static int
sorted_vector_insert_test(void)
{
    Sorted_vector sv;
    size_t elem_size = sizeof(int);
    size_t capacity = 100;
    sorted_vector_init(&sv, capacity, elem_size, cmp);

    static const int array0[] = {2, 4, 4, 6, 7};
    size_t i = 0;
    size_t num = sizeof(array0)/sizeof(int);
    for (i = 0; i < num; ++i) {
        sorted_vector_insert(&sv, array0 + i);
    }

    int *data = (int*)(void*)sv.v.data;
    for (i = 0; i < num; ++i) {
        EXPECT_INT_EQ(array0[i], data[i]);
    }

    static const int array1[] = {1, 2, 4, 4, 6, 7};
    num = sizeof(array1)/sizeof(int);
    int a = 1;
    sorted_vector_insert(&sv, &a);
    for (i = 0; i < num; ++i) {
        EXPECT_INT_EQ(array1[i], data[i]);
    }

    static const int array2[] = {1, 2, 3, 4, 4, 6, 7};
    num = sizeof(array2)/sizeof(int);
    a = 3;
    sorted_vector_insert(&sv, &a);
    for (i = 0; i < num; ++i) {
        EXPECT_INT_EQ(array2[i], data[i]);
    }

    static const int array3[] = {1, 2, 3, 4, 4, 6, 6, 7};
    num = sizeof(array3)/sizeof(int);
    a = 6;
    sorted_vector_insert(&sv, &a);
    for (i = 0; i < num; ++i) {
        EXPECT_INT_EQ(array3[i], data[i]);
    }

    static const int array4[] = {1, 2, 3, 4, 4, 6, 6, 7, 8};
    num = sizeof(array4)/sizeof(int);
    a = 8;
    sorted_vector_insert(&sv, &a);
    for (i = 0; i < num; ++i) {
        EXPECT_INT_EQ(array4[i], data[i]);
    }

    sorted_vector_fini(&sv);
    return 0;
}

/*
static int
sorted_vector_pop_back_test(void)
{
    Vector v;
    int i = 0;
    const int capacity = 100;

    sorted_vector_init(&v, 1, sizeof(int));

    for (i = 0; i < capacity; ++i) {
        sorted_vector_push_back(&v, &i);
    }

    --i;
    for (; i >= 0  ; --i) {
        EXPECT_INT_EQ(*(int*) sorted_vector_pop_back(&v), i);
    }

    sorted_vector_free(&v);
    return 0;
}

static void
sorted_vector_add_elem(void *p, int a)
{
    *(int *)p += a;
}

static int
sorted_vector_for_each_test(void)
{
    Vector v;
    int i = 0;
    const int capacity = 100;

    sorted_vector_init(&v, 1, sizeof(int));

    for (i = 0; i < capacity; ++i) {
        sorted_vector_push_back(&v, &i);
    }

    int *v_iter = NULL;
    sorted_vector_for_each(&v, v_iter,
        sorted_vector_add_elem(v_iter, 2);
    )

    --i;
    for (; i >= 0  ; --i) {
        EXPECT_INT_EQ(*(int*) sorted_vector_pop_back(&v), i + 2);
    }

    sorted_vector_free(&v);
    return 0;
}
*/
int
main(void)
{
    EXPECT_FUNC_EQ(sorted_vector_init_test(), 0);
    EXPECT_FUNC_EQ(sorted_vector_insert_test(), 0);
    /*
    EXPECT_FUNC_EQ(sorted_vector_pop_back_test(), 0);
    EXPECT_FUNC_EQ(sorted_vector_for_each_test(), 0);
    */
    return 0;
}


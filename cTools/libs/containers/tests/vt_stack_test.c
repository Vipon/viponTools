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

#include "test.h"
#include "comdef.h"
#include "vt_stack.h"

#include <stddef.h>

static void
vt_stack_init_test(void)
{
    vt_stack_t s;
    EXPECT_FUNC_EQ(0, vt_stack_init(&s, 0, sizeof(int)));

    vt_stack_fini(&s);
}

static void
vt_stack_push_pop_test(void)
{
    vt_stack_t s;
    vt_stack_init(&s, 0, sizeof(int));

    int i = 0;
    for (i = 0; i <= 100; ++i) {
        vt_stack_push(&s, &i);
    }

    for (i = 100; i >= 0; --i) {
        int a = *(int*)vt_stack_pop(&s);
        EXPECT_INT_EQ(i, a);
    }

    vt_stack_fini(&s);
}

static void
vt_stack_pop_empty_test(void)
{
    vt_stack_t s;
    vt_stack_init(&s, 0, sizeof(int));

    EXPECT_FUNC_EQ(NULL, vt_stack_pop(&s));
    EXPECT_FUNC_EQ(true, vt_stack_is_empty(&s));

    vt_stack_fini(&s);
}

int
main(void)
{
    VERBOSE = 1;
    vt_stack_init_test();
    vt_stack_push_pop_test();
    vt_stack_pop_empty_test();

    return 0;
}

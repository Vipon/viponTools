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
#include "mod_code.h"
#include <inttypes.h>

static void
test_mod_code_print(void)
{
    MOD_CODE(MOD_CODE_WITH_NOPS,
        printf("hello with nops\n");
    );
}

static void
test_mod_code_local_val(void)
{
    volatile int i = 0;
    MOD_CODE(MOD_CODE_WITH_NOPS,
        i = 10;
    );

    EXPECT_INT_EQ(10, i);
}

int a = 0;
static void
test_mod_code_global_val(void)
{
    MOD_CODE(MOD_CODE_WITH_NOPS,
        a = 1;
    );

    EXPECT_INT_EQ(1, a);
}

extern void __attribute__((noinline))
test_mod_code_in_loop(void);
void __attribute__((noinline))
test_mod_code_in_loop(void)
{
    volatile int i = 0;
    // work only with volatile
    // without compiler make unwind and remove eveything
    int count = 0;
    for (i = 0; i < 10; ++i) {
        MOD_CODE(MOD_CODE_WITH_NOPS,
            ++count;
        );

        printf("%d\n", count);
    }

    EXPECT_INT_EQ(10, count);
}

static void
test_mod_code_loop(void)
{
    int i = 0;
    // work only with volatile
    // without compiler make unwind and remove eveything
    volatile int count = 0;
    MOD_CODE(MOD_CODE_WITH_NOPS,
        for (i = 0; i < 10; ++i) {
                ++count;
        }
    );

    EXPECT_INT_EQ(10, count);
}

int main(int argc, const char *argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    STDERROR_PRINT("test_mod_code_print: %p\n", (void*)&test_mod_code_print);

    mod_code_init(argv[0]);
    mod_code_dump();
    mod_code_on();

    test_mod_code_print();
    test_mod_code_local_val();
    test_mod_code_global_val();
    test_mod_code_in_loop();
    test_mod_code_loop();

    return 0;
}


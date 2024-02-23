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

#include "comdef.h"
#include "mod_code.h"
#include <inttypes.h>

extern void test_func(void);
void test_func(void)
{
    STDERROR_PRINT("I'm here\n");
}

extern volatile int a;
volatile int a = 0;

int main(int argc, const char *argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    STDERROR_PRINT("main: %p\n", (void*)&main);

    MOD_CODE(
        do {
            printf("hello\n");
        } while (a);
    );

    mod_code_init(argv[0]);
    mod_code_dump();

#ifndef __WIN__
    ((void(*)(void))(mc[0].start))();
#endif /*__WIN__*/

    test_func();
    return 0;
}


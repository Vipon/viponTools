/***
 * MIT License
 *
 * Copyright (c) 2021 Konychev Valerii
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

#include "foo.h"
#include "bar.h"
#include "test.h"
#include "elf64Parse.h"
#include "comdef.h"

#ifdef __WIN__
# undef ERROR
# define ERROR(...)
#endif

static void hookFooWithBar(char *argv0)
{
    Elf64File *elf64 = elf64Parse(argv0);
    if (elf64 == NULL) {
        ERROR("Cannot parse %s", argv0);
        return;
    }

    elf64Hook(elf64, "foo", bar);
    elf64Free(elf64);
}

int main(int argc, char *argv[])
{
    UNUSED(argc);

    foo();
    bar();
    hookFooWithBar(argv[0]);

    char *str1 = foo();
    char *str2 = bar();
    EXPECT_STR_EQ(str1, str2);

    return 0;
}


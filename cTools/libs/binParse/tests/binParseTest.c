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

#include "os.h"
#include "foo.h"
#include "bar.h"
#include "test.h"
#include "binParse.h"
#ifdef __WIN__
    #include "pe64Printer.h"
#endif /* __WIN__ */
#include "comdef.h"

static void hookFooWithBar(char *argv0)
{
    initBinParser(argv0);

    if (binParser.type == MACHO64)
        binHook(MACHO64_SYM_PREF "foo", (const void *)bar);
    else
        binHook("foo", (const void *)bar);

    finiBinParser();
}

int main(int argc, char *argv[])
{
    UNUSED(argc);

    VERBOSE = 0;

    foo();
    bar();
    hookFooWithBar(argv[0]);

    char *str1 = foo();
    char *str2 = bar();
    LOG("str1: %s", str1);
    LOG("str2: %s", str2);
    EXPECT_STR_EQ(str1, str2);

    return 0;
}


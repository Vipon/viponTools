/***
 * MIT License
 *
 * Copyright (c) 2021-2024 Konychev Valerii
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
#include "arch.h"
#include "test.h"
#include "comdef.h"
#include "binDynMod.h"
#ifdef __WIN__
# include "pe64DynMod.h"
#endif /* __WIN__ */
#ifdef __LINUX__
# include "elf32DynMod.h"
# include "elf64DynMod.h"
#endif /* __LINUX__ */
#ifdef __MAC_OS_X__
# include "macho64DynMod.h"
# include "fatMacho64DynMod.h"
#endif /* __MAC_OS_X__ */

static void hookFooWithBar(char *argv0)
{
    initBinParser(argv0);
    initBinDynMod(binParser.type);

#ifdef __MAC_OS_X__
    binDynMod.hook(binParser.bin, MACHO64_SYM_PREF "foo", (const void *)bar);
#else
    binDynMod.hook(binParser.bin, "foo", (const void *)bar);
#endif

    finiBinParser();
}

int main(int argc, char *argv[])
{
    UNUSED(argc);

    VERBOSE = 1;

    char *name = argv[0];

    foo();
    bar();
    hookFooWithBar(name);

    char *str1 = foo();
    char *str2 = bar();
    LOG("str1: %s", str1);
    LOG("str2: %s", str2);
    EXPECT_STR_EQ(str1, str2);

    return 0;
}


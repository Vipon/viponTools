/***
 * MIT License
 *
 * Copyright (c) 2023 Konychev Valerii
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
#include "file.h"
#include "comdef.h"
#include "macho64Parse.h"
#include "macho64Printer.h"

#include <stdio.h>

static const char TESTOUT[] = "macho64PrintExeTest.txt";

int main(int argc, char *argv[])
{
    UNUSED(argc);
    Macho64File *mf = macho64Parse(argv[1]);
    if (mf == NULL) {
        VT_ERROR("Cannot parse %s", argv[1]);
        exit(EXIT_FAILURE);
    }

    FILE *f = freopen(TESTOUT, "w", stdout);

    macho64PrintHeader(mf);
    macho64PrintLComs(mf);
    macho64PrintSegments(mf);
    macho64PrintSections(mf);
    macho64PrintFuncStarts(mf);
    macho64PrintSymbols(mf);

    macho64Free(mf);

    fclose(f);

    EXPECT_FILE_EQ(TESTOUT, argv[2]);

    return 0;
}


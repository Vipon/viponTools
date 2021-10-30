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

#include "comdef.h"
#include "pe64Parse.h"
#include "pe64Printer.h"

void dummy(void) __attribute__ ((section (".MY_SECTION123")));
void dummy(void)
{

}

int main(int argc, char *argv[])
{
    UNUSED(argc);
    PE64File *pe = pe64Parse(argv[0]);

    pe64PrintDosHeader(pe);
    pe64PrintNtHeader(pe);
    pe64PrintSections(pe);
    pe64PrintSymbols(pe);
    pe64PrintImports(pe);
    pe64PrintDelayImports(pe);

    pe64Free(pe);
    return 0;
}


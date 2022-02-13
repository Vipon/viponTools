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

#include "pe64Printer.h"

void pe64PrintExport(const PE64File *pe, const PEExport *exp)
{
    if (pe == NULL || exp == NULL)
        return;

    // Characteristics - currently useless, always 0
}

void pe64PrintExports(const PE64File *pe)
{
    if (pe == NULL)
        return;

    printf("Exports Table:\n");
    printf("--------------------------------\n");

    uint64_t i = 0;
    // Last entry is NULL padded
    for (i = 0; i < pe->expNum - 1; ++i) {
        pe64PrintExport(pe, pe->exp + i);
        NEW_LINE;
    }
}


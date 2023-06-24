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

#include "macho64Printer.h"
#include "fatMacho64Printer.h"

void fatMacho64PrintHeader(const FatMacho64File *ff)
{
    const Macho64File *mf = fatMacho64GetConstMacho64ByArch(ff, fatMacho64ParseArch);
    if (mf == NULL)
        return;

    return macho64PrintHeader(mf);
}

void fatMacho64PrintSymbols(const FatMacho64File *ff)
{
    const Macho64File *mf = fatMacho64GetConstMacho64ByArch(ff, fatMacho64ParseArch);
    if (mf == NULL)
        return;

    return macho64PrintSymbols(mf);
}

void fatMacho64PrintSections(const FatMacho64File *ff)
{
    const Macho64File *mf = fatMacho64GetConstMacho64ByArch(ff, fatMacho64ParseArch);
    if (mf == NULL)
        return;

    return macho64PrintSections(mf);
}

void fatMacho64PrintSegments(const FatMacho64File *ff)
{
    const Macho64File *mf = fatMacho64GetConstMacho64ByArch(ff, fatMacho64ParseArch);
    if (mf == NULL)
        return;

    return macho64PrintSegments(mf);
}

void fatMacho64PrintFuncStarts(const FatMacho64File *ff)
{
    const Macho64File *mf = fatMacho64GetConstMacho64ByArch(ff, fatMacho64ParseArch);
    if (mf == NULL)
        return;

    return macho64PrintFuncStarts(mf);
}

void fatMacho64PrintLComs(const FatMacho64File *ff)
{
    const Macho64File *mf = fatMacho64GetConstMacho64ByArch(ff, fatMacho64ParseArch);
    if (mf == NULL)
        return;

    return macho64PrintLComs(mf);
}


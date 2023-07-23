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

#ifndef __FAT_MACHO_64_PRINTER_H
#define __FAT_MACHO_64_PRINTER_H

#include "arch.h"
#include "comdef.h"
#include "fatMacho64Parse.h"

EXPORT_FUNC
void fatMacho64PrintFatHeader(const FatMacho64File *ff);

EXPORT_FUNC
void fatMacho64PrintHeader(const FatMacho64File *ff);

EXPORT_FUNC
void fatMacho64PrintSymbols(const FatMacho64File *ff);

EXPORT_FUNC
void fatMacho64PrintSections(const FatMacho64File *ff);

EXPORT_FUNC
void fatMacho64PrintSegments(const FatMacho64File *ff);

EXPORT_FUNC
void fatMacho64PrintFuncStarts(const FatMacho64File *ff);

EXPORT_FUNC
void fatMacho64PrintLComs(const FatMacho64File *ff);

EXPORT_FUNC
void fatMacho64PrintCodeSign(const FatMacho64File *ff);

#endif /* __FAT_MACHO_64_PRINTER_H */


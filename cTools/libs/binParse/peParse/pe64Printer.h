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

#ifndef __PE64_PRINTER_H
#define __PE64_PRINTER_H

#include "pe64Parse.h"

void pe64PrintDosHeader(const PE64File *pe);
void pe64PrintFileHeader(const PE64File *pe);
void pe64PrintOptHeader(const PE64File *pe);
void pe64PrintNtHeader(const PE64File *pe);
void pe64PrintDataDir(const DataDir *dataDir);

void pe64PrintSectName(const PE64File *pe, const PESection *sect);
void pe64PrintSection(const PE64File *pe, const PESection *sect);
void pe64PrintSections(const PE64File *pe);

void pe64PrintINT(const PE64File *pe, ThunkData64 *INT);
void pe64PrintImport(const PE64File *pe, const PEImport* import);
void pe64PrintImports(const PE64File *pe);
void pe64PrintDelayImport(const PE64File *pe, const PEDelimp *delimp);
void pe64PrintDelayImports(const PE64File *pe);

void pe64PrintExports(const PE64File *pe);

void pe64PrintAuxSymSect(const PE64File *pe, const PEAuxSymbol *auxSym);
void pe64PrintAuxSymFile(const PE64File *pe, const PEAuxSymbol *auxSym);
void pe64PrintAuxSymbol(const PE64File *pe, const PESymbol *sym, const PEAuxSymbol *auxSym);
void pe64PrintSymbol(const PE64File *pe, const PESymbol *sym);
void pe64PrintSymbols(const PE64File *pe);

#endif /* __PE64_PRINTER_H */


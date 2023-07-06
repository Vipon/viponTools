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

#include "comdef.h"
#include "pe64Parse.h"

EXPORT_FUNC
void pe64PrintDosHeader(const PE64File *pe);

EXPORT_FUNC
void pe64PrintFileHeader(const PE64File *pe);

EXPORT_FUNC
void pe64PrintOptHeader(const PE64File *pe);

EXPORT_FUNC
void pe64PrintNtHeader(const PE64File *pe);
static INLINE
void pe64PrintHeader(const PE64File *pe)
{
    pe64PrintNtHeader(pe);
}

EXPORT_FUNC
void pe64PrintDataDir(const DataDir *dataDir);

EXPORT_FUNC
void pe64PrintSectName(const PE64File *pe, const PESection *sect);

EXPORT_FUNC
void pe64PrintSection(const PE64File *pe, const PESection *sect);

EXPORT_FUNC
void pe64PrintSections(const PE64File *pe);
static INLINE
void pe64PrintSegments(const PE64File *pe)
{
    pe64PrintSections(pe);
}

/***
 * Import Name Table
*/
EXPORT_FUNC
void pe64PrintINT(const PE64File *pe, ThunkData64 *INT);

EXPORT_FUNC
void pe64PrintImport(const PE64File *pe, const PEImport* import);
EXPORT_FUNC
void pe64PrintImports(const PE64File *pe);

EXPORT_FUNC
void pe64PrintDelayImport(const PE64File *pe, const PEDelimp *delimp);
EXPORT_FUNC
void pe64PrintDelayImports(const PE64File *pe);

EXPORT_FUNC
void pe64PrintExport(const PE64File *pe, const PEExport *exp);
EXPORT_FUNC
void pe64PrintExports(const PE64File *pe);

EXPORT_FUNC
void pe64PrintAuxSymSect(const PE64File *pe, const PEAuxSymbol *auxSym);

EXPORT_FUNC
void pe64PrintAuxSymFile(const PE64File *pe, const PEAuxSymbol *auxSym);

EXPORT_FUNC
void pe64PrintAuxSymbol(const PE64File *pe, const PESymbol *sym, const PEAuxSymbol *auxSym);

EXPORT_FUNC
void pe64PrintSymbol(const PE64File *pe, const PESymbol *sym);

EXPORT_FUNC
void pe64PrintSymbols(const PE64File *pe);


#endif /* __PE64_PRINTER_H */


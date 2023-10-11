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

#include "os.h"
#include "arch.h"
#include "binParse.h"

typedef void (*BinPrintHeader)(const BinFilePtr bin);
typedef void (*BinPrintSymbols)(const BinFilePtr bin);
typedef void (*BinPrintSections)(const BinFilePtr bin);
typedef void (*BinPrintSegments)(const BinFilePtr bin);
typedef void (*BinPrintFuncStarts)(const BinFilePtr bin);
typedef void (*BinPrintLComs)(const BinFilePtr bin);
typedef void (*BinPrintCodeSign)(const BinFilePtr bin);
typedef void (*BinPrintFatHeader)(const BinFilePtr bin);
typedef void (*BinPrintDosHeader)(const BinFilePtr bin);
typedef void (*BinPrintFileHeader)(const BinFilePtr bin);
typedef void (*BinPrintOptHeader)(const BinFilePtr bin);
typedef void (*BinPrintImports)(const BinFilePtr bin);
typedef void (*BinPrintDelayImports)(const BinFilePtr bin);
typedef void (*BinPrintExports)(const BinFilePtr bin);
typedef void (*BinPrintRelocations)(const BinFilePtr bin);
typedef void (*BinPrintDynamicSection)(const BinFilePtr bin);
typedef void (*BinPrintVersionInfo)(const BinFilePtr bin);

typedef struct {
    BinPrintHeader printHeader;
    BinPrintSymbols printSymbols;
    BinPrintSections printSections;
    BinPrintSegments printSegments;
    BinPrintRelocations printRelocations;
    struct {
        BinPrintFuncStarts printFuncStarts;
        BinPrintLComs printLComs;
        BinPrintCodeSign printCodeSign;
    } macho;
    struct {
        BinPrintFatHeader printFatHeader;
    } fatMacho;
    struct {
        BinPrintDosHeader printDosHeader;
        BinPrintFileHeader printFileHeader;
        BinPrintOptHeader printOptHeader;
        BinPrintImports printImports;
        BinPrintDelayImports printDelayImports;
        BinPrintExports printExports;
    } pe;
    struct {
        BinPrintDynamicSection printDynamicSection;
        BinPrintVersionInfo printVersionInfo;
    } elf;
} BinPrinter;

#ifdef BIN_PRINTER_SHARED_LIB
EXPORT_VAR
#else /* BIN_PRINTER_SHARED_LIB */
# ifndef STATIC_LIB
IMPORT_VAR
# endif /* STATIC_LIB*/
#endif /* BIN_PRINTER_SHARED_LIB */
extern BinPrinter binPrinter;

EXPORT_FUNC
int initBinPrinter(const char *fn);

EXPORT_FUNC
void setupBinPrinterArch(Arch arch);

EXPORT_FUNC
void finiBinPrinter(void);



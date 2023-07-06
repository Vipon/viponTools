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

#include "binParse.h"
#include "binPrinter.h"
#include "pe64Printer.h"
#include "macho64Printer.h"
#include "fatMacho64Printer.h"

BinPrinter binPrinter = {};

#define INIT_BIN_PRINTER(type) \
    binPrinter.printHeader = (BinPrintHeader)&(type ## PrintHeader); \
    binPrinter.printSymbols = (BinPrintSymbols)&(type ## PrintSymbols); \
    binPrinter.printSections = (BinPrintSections)&(type ## PrintSections); \
    binPrinter.printSegments = (BinPrintSegments)&(type ## PrintSegments);

#define INIT_MACHO_PRINT_FUNC(type) \
    binPrinter.macho.printFuncStarts = (BinPrintFuncStarts)&(type ## PrintFuncStarts); \
    binPrinter.macho.printLComs = (BinPrintLComs)&(type ## PrintLComs);

#define INIT_FAT_MACHO_PRINT_FUNC(type) \
    binPrinter.fatMacho.printFatHeader = (BinPrintFatHeader)&(type ## PrintHeader);

#define INIT_PE_PRINT_FUNC(type) \
    binPrinter.pe.printDosHeader = (BinPrintDosHeader)&(type ## PrintDosHeader); \
    binPrinter.pe.printFileHeader = (BinPrintFileHeader)&(type ## PrintFileHeader); \
    binPrinter.pe.printOptHeader = (BinPrintOptHeader)&(type ## PrintOptHeader); \
    binPrinter.pe.printImports = (BinPrintImports)&(type ## PrintImports); \
    binPrinter.pe.printDelayImports = (BinPrintOptHeader)&(type ## PrintDelayImports); \
    binPrinter.pe.printExports = (BinPrintExports)&(type ## PrintExports);

int initBinPrinter(const char *fn)
{
    if (initBinParser(fn)) {
        return -1;
    }

    switch(binParser.type) {
    case MACHO64:
        INIT_BIN_PRINTER(macho64);
        INIT_MACHO_PRINT_FUNC(macho64);
        break;
    case FATMACHO64:
        INIT_BIN_PRINTER(fatMacho64);
        INIT_MACHO_PRINT_FUNC(fatMacho64);
        INIT_FAT_MACHO_PRINT_FUNC(fatMacho64);
        break;
    case PE64:
        INIT_BIN_PRINTER(pe64);
        INIT_PE_PRINT_FUNC(pe64);
        break;
    /*
    case ELF64:
        INIT_BIN_PRINTER(elf64);
        break;
    case ELF32:
        INIT_BIN_PRINTER(elf32);
        break;
    case MACHO32:
        INIT_BIN_PRINTER(macho32);
        break;
    case PE32:
        INIT_BIN_PRINTER(pe32);
        break;
    */
    default:
        return -1;
    }

    return 0;
}

void setupBinPrinterArch(Arch arch)
{
    switch(binParser.type) {
    case FATMACHO64:
        fatMacho64ParseArch = arch;
        return;
    default:
        return;
    }
}

void finiBinPrinter(void)
{
    finiBinParser();
}


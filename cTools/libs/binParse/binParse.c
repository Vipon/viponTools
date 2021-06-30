/***
 * MIT License
 *
 * Copyright (c) 2021 Konychev Valera
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
#include "elf64Parse.h"
#include "elf32Parse.h"
#include "comdef.h"

BinParser binParser;

#define INIT_BIN_PARSER_FUNCS(type) \
    binParser.parse = (BinParse)&type ## Parse; \
    binParser.free = (BinFree)&type ## Free; \
    binParser.hook = (BinHook)&type ## Hook; \
    binParser.check = (BinFullCheck) &type ## Check; \
    binParser.printSymbol = (BinPrintSymbol) &type ## PrintSymbol; \
    binParser.printSymbols = (BinPrintSymbols) &type ## PrintSymbols; \
    binParser.getSymByName = (BinGetSymByName) &type ## GetSymByName; \
    binParser.getSymName = (BinGetSymName) &type ## GetSymName; \
    binParser.cmpSym = (BinCmpSym) &type ## CmpSym; \
    binParser.getSSymTab = (BinGetSSymTab) &type ## GetSSymTab; \
    binParser.getSSymSortTab = (BinGetSSymSortTab) &type ## GetSSymSortTab; \
    binParser.getAmountSSym = (BinGetAmountSSym) &type ## GetAmountSSym; \
    binParser.getSSymAddr = (BinGetSSymAddr) &type ## GetSSymAddr; \
    binParser.getAddrSymByName = (BinGetAddrSymByName) &type ## GetAddrSymByName; \
    binParser.getSSymSize = (BinGetSSymSize) &type ## GetSSymSize; \
    binParser.getSSymFileoff = (BinGetSSymFileoff) &type ## GetSSymFileoff; \
    binParser.getDSymIndex = (BinGetDSymIndex) &type ## GetDSymIndex; \
    binParser.getAmountSegment = (BinGetAmountSegment) &type ## GetAmountSegment; \
    binParser.getSectByName = (BinGetSectByName) &type ## GetSectByName; \
    binParser.getLastLoadableSect = (BinGetLastLoadableSect) &type ## GetLastLoadableSect; \
    binParser.readSect = (BinReadSect) &type ## ReadSect; \
    binParser.getAmountSect = (BinGetAmountSect) &type ## GetAmountSect; \
    binParser.getSectName = (BinGetSectName) &type ## GetSectName; \
    binParser.getSectSize = (BinGetSectSize) &type ## GetSectSize; \
    binParser.getSectVaddr = (BinGetSectVaddr) &type ## GetSectVaddr; \
    binParser.getSectFileoff = (BinGetSectFileoff) &type ## GetSectFileoff; \
    binParser.getRelocationForAddr = (BinGetRelocationForAddr) &type ## GetRelocationForAddr; \
    binParser.getRelocationDataAddr = (BinGetRelocationDataAddr) &type ## GetRelocationDataAddr;

int initBinParser(const char *fn)
{
    BinFilePtr bin = NULL;
    if ((bin = elf64Parse(fn)) != NULL) {
        binParser.type = ELF64;
        INIT_BIN_PARSER_FUNCS(elf64);
    } else if ((bin = elf32Parse(fn)) != NULL) {
        binParser.type = ELF32;
        INIT_BIN_PARSER_FUNCS(elf32);
    } else {
        ERROR("Unknown FileType\n");
        return -1;
    }

    binParser.bin = bin;
    return 0;
}


void finiBinParser()
{
    binParser.free(binParser.bin);
}


void *binHook(const char *func, const void *hand)
{
    return binParser.hook(binParser.bin, func, hand);
}


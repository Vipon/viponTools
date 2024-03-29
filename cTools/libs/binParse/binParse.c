/***
 * MIT License
 *
 * Copyright (c) 2021-2024 Konychev Valera
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
#include "comdef.h"
#include "binParse.h"
#include "pe64Parse.h"
#include "elf64Parse.h"
#include "elf32Parse.h"
#include "macho64Parse.h"
#include "fatMacho64Parse.h"

#ifdef BIN_PARSER_SHARED_LIB
EXPORT_VAR
#endif /* BIN_PARSER_SHARED_LIB */
BinParser binParser = {0};

#define INIT_BIN_PARSER_FUNCS(type) \
    binParser.parse = (BinParse)&type ## Parse; \
    binParser.free = (BinFree)&type ## Free; \
    binParser.check = (BinFullCheck) &type ## Check; \
    binParser.getSymByName = (BinGetSymByName) &type ## GetSymByName; \
    binParser.getSymName = (BinGetSymName) &type ## GetSymName; \
    binParser.cmpSym = (BinCmpSym) &type ## CmpSym; \
    binParser.getSSymTab = (BinGetSSymTab) &type ## GetSSymTab; \
    binParser.getAmountSSym = (BinGetAmountSSym) &type ## GetAmountSSym; \
    binParser.getSSymAddr = (BinGetSSymAddr) &type ## GetSSymAddr; \
    binParser.getAddrSymByName = (BinGetAddrSymByName) &type ## GetAddrSymByName; \
    binParser.getSSymSize = (BinGetSSymSize) &type ## GetSSymSize; \
    binParser.getSSymFileoff = (BinGetSSymFileoff) &type ## GetSSymFileoff; \
    /*binParser.getDSymIndxByName = (BinGetDSymIndxByName) &type ## GetDSymIndxByName;*/ \
    binParser.getAmountSeg = (BinGetAmountSeg) &type ## GetAmountSeg; \
    binParser.getSectByName = (BinGetSectByName) &type ## GetSectByName; \
    binParser.getLastLoadableSect = (BinGetLastLoadableSect) &type ## GetLastLoadableSect; \
    binParser.readSect = (BinReadSect) &type ## ReadSect; \
    binParser.getAmountSect = (BinGetAmountSect) &type ## GetAmountSect; \
    binParser.getSectName = (BinGetSectName) &type ## GetSectName; \
    binParser.getSectSize = (BinGetSectSize) &type ## GetSectSize; \
    binParser.getSectAddr = (BinGetSectAddr) &type ## GetSectAddr; \
    binParser.getSectFileoff = (BinGetSectFileoff) &type ## GetSectFileoff; \
    //binParser.getRelocForAddr = (BinGetRelocForAddr) &type ## GetRelocForAddr; \
    //binParser.getRelocDataAddr = (BinGetRelocDataAddr) &type ## GetRelocDataAddr;

int initBinParser(const char *fn)
{
    BinFilePtr bin = NULL;
    if ((bin = elf64Parse(fn)) != NULL) {
        binParser.type = ELF64;
        INIT_BIN_PARSER_FUNCS(elf64);
    } else if ((bin = elf32Parse(fn)) != NULL) {
        binParser.type = ELF32;
        INIT_BIN_PARSER_FUNCS(elf32);
    } else if ((bin = macho64Parse(fn)) != NULL) {
        binParser.type = MACHO64;
        INIT_BIN_PARSER_FUNCS(macho64);
    } else if ((bin = fatMacho64Parse(fn)) != NULL) {
        binParser.type = FATMACHO64;
        INIT_BIN_PARSER_FUNCS(fatMacho64);
    } else if ((bin = pe64Parse(fn)) != NULL) {
        binParser.type = PE64;
        INIT_BIN_PARSER_FUNCS(pe64);
    } else {
        LOG_ERROR("Unknown FileType\n");
        return -1;
    }

    binParser.bin = bin;
    return 0;
}

void finiBinParser(void)
{
    binParser.free(binParser.bin);
}


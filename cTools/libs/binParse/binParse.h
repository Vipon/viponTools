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

#ifndef _BIN_PARSE_H
#define _BIN_PARSE_H

#include <stdint.h>

typedef enum {
    ELF64 = 0,
    MACHO64,
    PE64,
    ELF32,
    MACHO32,
    PE32
} BIN_FILE_TYPE;

// !TODO: shoud be enum
typedef uint64_t BIN_ERROR;

typedef void* BinFilePtr;
typedef void* BinSymPtr;
typedef void* BinSectPtr;

typedef BinFilePtr (*BinParse)(const char*);
typedef void (*BinFree)(void*);
typedef void* (*BinHook)(const BinFilePtr, const char*, const void*);
typedef BIN_ERROR (*BinFullCheck)(const BinFilePtr);
//typedef BIN_ERROR (*BinPrintSymbol)(const BinFilePtr, const BinSymPtr);
//typedef BIN_ERROR (*BinPrintSymbols)(const BinFilePtr);
typedef BinSymPtr (*BinGetSymByName)(const BinFilePtr, const char*);
typedef char* (*BinGetSymName)(const BinFilePtr, const BinSymPtr);
typedef int (*BinCmpSym)(const void*, const void*);
typedef BinSymPtr (*BinGetSSymTab)(const BinFilePtr);
typedef BinSymPtr (*BinGetSSymSortTab)(const BinFilePtr);
typedef uint64_t (*BinGetAmountSSym)(const BinFilePtr);
typedef uint64_t (*BinGetSSymAddr)(const BinSymPtr);
typedef uint64_t (*BinGetAddrSymByName)(const BinFilePtr, const char*);
typedef uint64_t (*BinGetSSymSize)(const BinFilePtr, const BinSymPtr);
typedef uint64_t (*BinGetSSymFileoff)(const BinFilePtr, const BinSymPtr);
typedef uint64_t (*BinGetDSymIndxByName)(const BinFilePtr, const char*);
typedef uint64_t (*BinGetAmountSeg)(const BinFilePtr);
typedef BinSectPtr* (*BinGetSectByName)(const BinFilePtr, const char*);
typedef BinSectPtr* (*BinGetLastLoadableSect)(const BinFilePtr);
typedef void* (*BinReadSect)(const BinFilePtr, const BinSectPtr);
typedef uint64_t (*BinGetAmountSect)(const BinFilePtr);
typedef const char* (*BinGetSectName)(const BinFilePtr, const BinSectPtr);
typedef uint64_t (*BinGetSectSize)(const BinSectPtr);
typedef uint64_t (*BinGetSectAddr)(const BinSectPtr);
typedef uint64_t (*BinGetSectFileoff)(const BinSectPtr);
typedef uint64_t (*BinGetRelocForAddr)(const BinFilePtr, const BinSectPtr, uint64_t);
typedef void* (*BinGetRelocDataAddr)(const BinFilePtr, const char*);

typedef struct {
    BIN_FILE_TYPE          type;
    BinFilePtr             bin;
    BinParse               parse;
    BinFree                free;
    BinHook                hook;
    BinFullCheck           check;
    //BinPrintSymbol         printSymbol;
    //BinPrintSymbols        printSymbols;
    BinGetSymByName        getSymByName;
    BinGetSymName          getSymName;
    BinCmpSym              cmpSym;
    BinGetSSymTab          getSSymTab;
    BinGetSSymSortTab      getSSymSortTab;
    BinGetAmountSSym       getAmountSSym;
    BinGetSSymAddr         getSSymAddr;
    BinGetAddrSymByName    getAddrSymByName;
    BinGetSSymSize         getSSymSize;
    BinGetSSymFileoff      getSSymFileoff;
    //BinGetDSymIndxByName   getDSymIndxByName;
    BinGetAmountSeg        getAmountSeg;
    BinGetSectByName       getSectByName;
    BinGetLastLoadableSect getLastLoadableSect;
    BinReadSect            readSect;
    BinGetAmountSect       getAmountSect;
    BinGetSectName         getSectName;
    BinGetSectSize         getSectSize;
    BinGetSectAddr         getSectAddr;
    BinGetSectFileoff      getSectFileoff;
    //BinGetRelocForAddr     getRelocForAddr;
    //BinGetRelocDataAddr    getRelocDataAddr;
} BinParser;

extern BinParser binParser;

int initBinParser(const char *fn);
void finiBinParser(void);
void *binHook(const char *func, const void *hand);

#endif /* _BIN_PARSE_H */


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

typedef enum {
    ELF64 = 0,
    MACHO64,
    PE64,
    ELF32,
    MACHO32,
    PE32
} BIN_FILE_TYPE;

typedef void* BinFile;
typedef void*(*BinParse)(const char *fn);
typedef void(*BinFree)(void *binFile);
typedef void*(*BinHook)(const void* binFile, const char *func, const void *hand);

typedef struct {
    BIN_FILE_TYPE type;
    BinFile *binFile;
    BinParse binParse;
    BinFree binFree;
    BinHook binHook;
} BinParser;

extern BinParser binParser;

int initBinParser(const char *fn);
void finiBinParser(void);
void *binHook(const char *func, const void *hand);

#endif /* _BIN_PARSE_H */



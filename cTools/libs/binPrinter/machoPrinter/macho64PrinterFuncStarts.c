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

#include "mem.h"
#include "LEB128.h"
#include "macho64Printer.h"

#include <inttypes.h>

void macho64PrintFuncStarts(const Macho64File *mf)
{
    /***
     * LC_FUNCTION_STARTS:
     *  The first value is the offset from the start of the __TEXT segment
     *  to the start of the first function. The remaining values is the offset
     *  to the start of the next function.
     *
     *  Offsets are writeen in ULEB128 format.
     */
    printf("Function starts:\n");
    FileD fd = mf->fd;
    size_t foff = mf->funcStarts->dataoff;
    size_t fsize = mf->funcStarts->datasize;
    uint8_t *fs = readFromFile(fd, &foff, fsize);

    uint64_t baseAddr = mf->segments[TEXT_NSEG]->vmaddr;
    uint8_t *p = fs;
    printf("%18s %18s %s\n", "offset", "vaddr", "func");
    while (p < fs + fsize) {
        uint64_t off = 0;
        p = fromULEB128(p, &off);
        baseAddr += off;

        const Macho64Sym *sym = macho64GetSSymByAddr(mf, baseAddr);
        const char *sname = macho64GetSymName(mf, sym);
        printf("0x%.16"PRIx64" 0x%.16"PRIx64" %s\n", off, baseAddr, sname);
    }

    Free(fs);
}


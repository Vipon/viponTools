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

#include "pe64Parse.h"
#include "mem.h"
#include "file.h"
#include "comdef.h"


PE64File *pe64Parse(const char *fn)
{
    if (fn == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    FileD fd = 0;
    if ((fd = openFile(fn, O_RDONLY)) < 0) {
        PERROR("openFile()");
        return NULL;
    }

    PE64File *pe64 = (PE64File*) Calloc(1, sizeof(PE64File));
    if (pe64 == NULL) {
        ERROR("Cannot allocate %zu bytes", sizeof(PE64File));
        goto eexit_0;
    }

    pe64->fd = fd;
    uint64_t nameSize = strlen(fn) * sizeof(char);
    if ((elf64->fn = (char*) Calloc(nameSize, sizeof(char))) == NULL) {
        ERROR("Cannot allocate %zu bytes", nameSize);
        goto eexit_1;
    }

    return pe64;

eexit_1:
    pe64Free(elf64);
    return NULL;
eexit_0:
    closeFile(fd);
    return NULL;
}

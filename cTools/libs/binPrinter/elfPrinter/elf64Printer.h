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

#ifndef __ELF_64_PRINTER_H
#define __ELF_64_PRINTER_H

#include "comdef.h"
#include "elf64Parse.h"

EXPORT_FUNC
void elf64PrintHeader(const Elf64File *elf);

typedef struct {
    uint32_t    flag;
    const char  *str;
} Elf64Flag;

typedef Elf64Flag Elf64SectFlag;
extern const Elf64SectFlag ELF64_SECT_FLAGS[];

EXPORT_FUNC
void elf64PrintSection(const Elf64File *elf, const Elf64Shdr *sect);
EXPORT_FUNC
void elf64PrintSections(const Elf64File *elf);

#endif /* __ELF_64_PRINTER_H */


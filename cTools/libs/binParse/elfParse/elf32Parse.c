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

#include "elf32Parse.h"


static Elf32Ehdr *elf32ParseHeader(const int fd)
{
    if (fd < 0) {
        ERROR("Invalid arguments");
        return NULL;
    }

    size_t ehOff = 0;
    size_t ehSize = sizeof(Elf32Ehdr);
    Elf32Ehdr *header = (Elf32Ehdr*) readFromFile(fd, &ehOff, ehSize);
    if (header == NULL) {
        ERROR("readFromFile()");
        return NULL;
    }

    unsigned char *eIdent = header->e_ident;
    if (eIdent[EI_MAG0] == '\x7f' && eIdent[EI_MAG1] == 'E' &&
        eIdent[EI_MAG2] == 'L' && eIdent[EI_MAG3] == 'F')
        return header;
    else {
        LOG("It isn't ELF file.\n");
        Free(header);
        return NULL;
    }
}


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf32 - point to target elf32File structure with initialized fields: fd and
 *        elf32Header.
 * Output:
 *  Success:
 *      point to elf32 section headers table.
 *  Fail:
 *      NULL point.
 * After all:
 *  need to free memory.
 */
static Elf32Shdr *elf32ParseSectionHeadersTable(const Elf32File *elf32)
{
    /***
     * Sections are identified by an index into the section header table.
     */
    if (elf32 == NULL || elf32->elf32Header == NULL || elf32->fd < 0) {
        ERROR("Invalid arguments");
        return NULL;
    }

    /***
     * e_shoff - contains the file offset, in bytes, of the section header
     *           table.
     * e_shnum - contains the number of entries in the section header table.
     * If the number of sections is greater than or equal to SHN_LORESERVE,
     * e_shnum has the value SHN_UNDEF. The actual number of section header
     * table entries is contained in the sh_size field of the section header
     * at index 0.
     * Otherwise, the sh_size member of the initial entry contains the value
     * zero.
     */
    int fd = elf32->fd;
    size_t eShOff = elf32->elf32Header->e_shoff;
    size_t shNum = elf32->elf32Header->e_shnum;
    size_t shSize = 0;

    if (shNum != SHN_UNDEF) {
        shSize = shNum * sizeof(Elf32Shdr);
    } else {
        Elf32Shdr *sect0 = (Elf32Shdr*) readFromFile(fd, &eShOff, sizeof(Elf32Shdr));
        shNum = sect0->sh_size;
        if (shNum == 0) {
            LOG("There are no sections headers");
            free(sect0);
            return NULL;
        } else {
            shSize = shNum * sizeof(Elf32Shdr);
        }

        free(sect0);
    }

    return (Elf32Shdr*) readFromFile(fd, &eShOff, shSize);
}


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf32 - point to target elf32File structure with initialized fields: fd and
 *        elf32Header.
 * Output:
 *  Success:
 *      point to elf32 segments headers table.
 *  Fail:
 *      NULL point.
 * After all:
 *  need to free memory.
 */
static Elf32Phdr *elf32ParseSegments(const Elf32File *elf32)
{
    if(elf32 == NULL || elf32->fd < 0 || elf32->elf32Header == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    size_t phoff = elf32->elf32Header->e_phoff;
    size_t phnum = elf32->elf32Header->e_phnum;

    return readFromFile(elf32->fd, &phoff, sizeof(Elf32Phdr)*phnum);
}


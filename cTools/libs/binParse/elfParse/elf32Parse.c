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


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Description:
 *  Function returns elf32 header, which is readed from file with descriptor @fd.
 * Input:
 *  @fd - target elf32File descriptor.
 * Output:
 *  Success:
 *      point to elf32Header structure.
 *  Fail:
 *      NULL point.
 * After:
 *  need to free memory.
 */
static Elf32Ehdr *elf32ParseHeader(const int fd)
{
    if (fd < 0) {
        ERROR("Invalid arguments");
        return NULL;
    }

    uint32_t ehOff = 0;
    uint32_t ehSize = sizeof(Elf32Ehdr);
    Elf32Ehdr *header = (Elf32Ehdr*) readFromFile(fd, &ehOff, ehSize);
    if (header == NULL) {
        ERROR("readFromFile()");
        return NULL;
    }

    unsigned char *eIdent = header->e_ident;
    if (eIdent[EI_MAG0] == '\x7f' && eIdent[EI_MAG1] == 'E'
       && eIdent[EI_MAG2] == 'L' && eIdent[EI_MAG3] == 'F'
       && eIdent[EI_CLASS] == ELFCLASS32) {
        return header;
    } else {
        LOG("It isn't ELF32 file.\n");
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
    uint32_t eShOff = elf32->elf32Header->e_shoff;
    uint32_t shNum = elf32->elf32Header->e_shnum;
    uint32_t shSize = 0;

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

    uint32_t phoff = elf32->elf32Header->e_phoff;
    uint32_t phnum = elf32->elf32Header->e_phnum;

    return readFromFile(elf32->fd, &phoff, sizeof(Elf32Phdr)*phnum);
}


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf32 - Elf32File structer with initialized fields: fd, elf32Header and
 *          sectionHeaders.
 * Output:
 *  Success:
 *      point to elf32 symbol table.
 *  Fail:
 *      NULL point.
 * After:
 *  need to free memory
 */
static Elf32Sym *elf32ParseSymbolTable(const Elf32File *elf32)
{
    if(elf32 == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    /***
     * SHT_SYMTAB - Contains a linker symbol table.
     */
    Elf32Shdr *symbolTable = elf32GetSectionByType(elf32, SHT_SYMTAB);
    if(symbolTable == NULL) {
        STDERROR_PRINT_DEBUG("Cannot get section by type SHT_SYMTAB.");
        return NULL;
    }

    return (Elf32Sym*) elf32ReadSection(elf32, symbolTable);
}


/*
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  elf32 - Elf32File structer with initialized fields: fd, elf32Header and
 *        sectionHeaders.
 * Output:
 *  Success:
 *      point to elf32 dynamic symbol table.
 *  Fail:
 *      NULL point.
 * After all:
 *  need to free memory
 */
static Elf32Sym *elf32ParseDynamicSymbolTable(const Elf32File *elf32)
{
    if(elf32 == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    /*
     * SHT_DYNSYM - Contains a dynamic loader symbol table.
     */
    Elf32Shdr *dynamicSymbolTable = elf32GetSectionByType(elf32, SHT_DYNSYM);
    if(dynamicSymbolTable == NULL) {
        WARNING("There is no SHT_DYNSYM section.\n");
        return NULL;
    }

    return (Elf32Sym*) elf32ReadSection(elf32, dynamicSymbolTable);
}


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf32 - Elf32File structer with initialized fields: fd, elf32Header and
 *        sectionHeaders.
 * Output:
 *  Success:
 *      point to Symbols structure, that contains symtab and dynsym tables.
 *  Fail:
 *      NULL point.
 * After all:
 *  need to free memory
 */
static Symbols *elf32ParseSymbols(const Elf32File *elf32)
{
    if(elf32 == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    Symbols *sym = (Symbols*) Malloc(sizeof(Symbols));
    if(sym == NULL) {
        STDERROR_PRINT_DEBUG("Cannot allocate %zu bytes.", sizeof(Symbols));
        return NULL;
    }

    sym->symtab = elf32ParseSymbolTable(elf32);
    if(sym->symtab == NULL) {
        STDERROR_PRINT_DEBUG("Cannot parse a symbol table.");
        Free(sym);
        return NULL;
    }

    sym->dynsym = elf32ParseDynamicSymbolTable(elf32);
    if(sym->dynsym == NULL) {
        WARNING("Cannot parse a dymanic symbol table.\n");
        /*Free(sym->symtab);
        Free(sym);
        return NULL;*/
    }

    return sym;
}




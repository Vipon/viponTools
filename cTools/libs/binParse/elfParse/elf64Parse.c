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

/* vipon headers */
#include "mem.h"
#include "file.h"
#include "comdef.h"
#include "string.h"
#include "elf64Parse.h"


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Description:
 *  Function returns elf64 header, which is readed from file with descriptor @fd.
 * Input:
 *  @fd - target elf64File descriptor.
 * Output:
 *  Success:
 *      ELF64_OK
 *  Fail:
 *      ELF64_INV_ARG, ELF64_NO_MEM, ELF64_NO_HEADER
 * After:
 *  need to free memory.
 */
static ELF64_ERROR elf64ParseHeader(Elf64File *elf64)
{
    if (elf64 == NULL || elf64->fd < 0)
        return ELF64_INV_ARG;

    int fd = elf64->fd;
    uint64_t ehOff = 0;
    uint64_t ehSize = sizeof(Elf64Ehdr);
    Elf64Ehdr *header = (Elf64Ehdr*) readFromFile(fd, &ehOff, ehSize);
    if (header == NULL)
        return ELF64_NO_MEM;

    unsigned char *eIdent = header->e_ident;
    if (  eIdent[EI_MAG0] == '\x7f' && eIdent[EI_MAG1] == 'E'
       && eIdent[EI_MAG2] == 'L' && eIdent[EI_MAG3] == 'F'
       && eIdent[EI_CLASS] == ELFCLASS64) {
        elf64->header = header;
        elf64->type = header->e_type;
        return ELF64_OK;
    } else {
        Free(header);
        return ELF64_NO_HEADER;
    }
}


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf64 - point to target elf64File structure with initialized fields: fd and
 *        elf64Header.
 * Output:
 *  Success:
 *      ELF64_OK
 *  Fail:
 *      ELF64_INV_ARG, ELF64_NO_MEM, ELF64_NO_SECTIONS
 * After all:
 *  need to free memory.
 */
static ELF64_ERROR elf64ParseSections(Elf64File *elf64)
{
    /***
     * Sections are identified by an index into the section header table.
     */
    if (elf64 == NULL || elf64->header == NULL || elf64->fd < 0)
        return ELF64_INV_ARG;

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
    int fd = elf64->fd;
    uint64_t eShOff = elf64->header->e_shoff;
    uint64_t shNum = elf64->header->e_shnum;
    uint64_t shSize = 0;

    if (shNum != SHN_UNDEF)
        shSize = shNum * sizeof(Elf64Shdr);
    else {
        Elf64Shdr *sect0 = (Elf64Shdr*) readFromFile(fd, &eShOff, sizeof(Elf64Shdr));
        shNum = sect0->sh_size;
        if (shNum == 0) {
            free(sect0);
            return ELF64_NO_SECTIONS;
        } else
            shSize = shNum * sizeof(Elf64Shdr);

        free(sect0);
    }

    elf64->sections = (Elf64Shdr*) readFromFile(fd, &eShOff, shSize);
    if (elf64->sections == NULL)
        return ELF64_NO_MEM;

    return ELF64_OK;
}


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf64 - point to target elf64File structure with initialized fields: fd and
 *        elf64Header.
 * Output:
 *  Success:
 *      ELF64_OK
 *  Fail:
 *      ELF64_INV_ARG, ELF64_NO_MEM, ELF64_NO_SEGMENTS
 * After all:
 *  need to free memory.
 */
static ELF64_ERROR elf64ParseSegments(Elf64File *elf64)
{
    if (elf64 == NULL || elf64->fd < 0 || elf64->header == NULL)
        return ELF64_INV_ARG;

    uint64_t phoff = elf64->header->e_phoff;
    uint64_t phnum = elf64->header->e_phnum;
    if (phnum == 0)
        return ELF64_NO_SEGMENTS;

    elf64->segments = readFromFile(elf64->fd, &phoff, sizeof(Elf64Phdr)*phnum);
    if (elf64->segments == NULL)
        return ELF64_NO_MEM;

    return ELF64_OK;
}


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf64 - Elf64File structer with initialized fields: fd, elf64Header and
 *          sectionHeaders.
 * Output:
 *  Success:
 *      ELF64_OK
 *  Fail:
 *      ELF64_INV_ARG, ELF64_NO_MEM, ELF64_NO_SYMTAB
 * After:
 *  need to free memory
 */
static ELF64_ERROR elf64ParseSymTab(Elf64File *elf64)
{
    if (elf64 == NULL)
        return ELF64_INV_ARG;

    /***
     * SHT_SYMTAB - Contains a linker symbol table.
     */
    Elf64Shdr *symtab = elf64GetSectionByType(elf64, SHT_SYMTAB);
    if (symtab == NULL)
        return ELF64_NO_SYMTAB;

    elf64->symtab = (Elf64Sym*) elf64ReadSection(elf64, symtab);
    if (elf64->symtab == NULL)
        return ELF64_NO_MEM;

    elf64->symnum = symtab->sh_size/sizeof(Elf64Sym);
    return ELF64_OK;
}


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  elf64 - Elf64File structer with initialized fields: fd, elf64Header and
 *        sectionHeaders.
 * Output:
 *  Success:
 *      ELF64_OK
 *  Fail:
 *      ELF64_INV_ARG, ELF64_NO_MEM, ELF64_NO_DYNSYM
 * After all:
 *  need to free memory
 */
static ELF64_ERROR elf64ParseDynSym(Elf64File *elf64)
{
    if (elf64 == NULL)
        return ELF64_INV_ARG;

    /***
     * SHT_DYNSYM - Contains a dynamic loader symbol table.
     */
    Elf64Shdr *dynsym = elf64GetSectionByType(elf64, SHT_DYNSYM);
    if (dynsym == NULL)
        return ELF64_NO_DYNSYM;

    elf64->dynsym = (Elf64Sym*) elf64ReadSection(elf64, dynsym);
    if (elf64->dynsym == NULL)
        return ELF64_NO_MEM;


    elf64->dynsymnum = dynsym->sh_size/sizeof(Elf64Sym);
    return ELF64_OK;
}


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf64 - Elf64File structer with initialized fields: fd, elf64Header and
 *        sectionHeaders.
 * Output:
 *  Success:
 *      ELF64_OK
 *  Fail:
 *      ELF64_INV_ARG, ELF64_NO_MEM, ELF64_NO_SYMBOLS
 * After all:
 *  need to free memory
 */
static ELF64_ERROR elf64ParseSymbols(Elf64File *elf64)
{
    if (elf64 == NULL)
        return ELF64_INV_ARG;

    elf64ParseSymTab(elf64);
    elf64ParseDynSym(elf64);

    if (elf64->symtab == NULL && elf64->dynsym == NULL)
        return ELF64_NO_SYMBOLS;

    return ELF64_OK;
}


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf64 - Elf64File structer with initialized fields: fd, elf64Header and
 *          sectionHeaders.
 * Output:
 *  Success:
 *      ELF64_OK
 *  Fail:
 *      ELF64_INV_ARG, ELF64_NO_MEM, ELF64_NO_SYMTAB
 * After:
 *  need to free memory
 */
static ELF64_ERROR elf64ParseSortSymTab(Elf64File *elf64)
{
    if (elf64 == NULL)
        return ELF64_INV_ARG;

    /***
     * SHT_SYMTAB - Contains a linker symbol table.
     */
    Elf64Shdr *symtab = elf64GetSectionByType(elf64, SHT_SYMTAB);
    if (symtab == NULL)
        return ELF64_NO_SYMTAB;

    uint64_t shSize = symtab->sh_size;
    uint64_t symNum = shSize/sizeof(Elf64Sym);
    Elf64Sym *sortSymtab = (Elf64Sym*) elf64ReadSection(elf64, symtab);

    qsort(sortSymtab, symNum, sizeof(Elf64Sym), elf64CmpSym);

    elf64->sortSymtab = sortSymtab;
    return ELF64_OK;
}


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf64 - Elf64File structer with initialized fields: fd, elf64Header and
 *        sectionHeaders.
 * Output:
 *  Success:
 *      ELF64_OK
 *  Fail:
 *      ELF64_INV_ARG, ELF64_NO_MEM, ELF64_NO_SH_NAME_TAB
 * After:
 *  need to free memory. You should to be careful, see Symbols structure.
*/
static ELF64_ERROR elf64ParseSectNameTab(Elf64File *elf64)
{
    if (elf64 == NULL || elf64->header == NULL || elf64->sections == NULL)
        return ELF64_INV_ARG;

    /***
     * e_shstrndx - contains the section header table index of the
     *              section containing the section name string table.
     *              If there is no section name string table,
     *              this field has the value SHN_UNDEF .
     */
    uint64_t shStrNdx = elf64->header->e_shstrndx;
    if (shStrNdx == SHN_UNDEF)
        return ELF64_NO_SH_NAME_TAB;

    Elf64Shdr *nameTab = &elf64->sections[shStrNdx];
    elf64->sectNameTab = (char*) elf64ReadSection(elf64, nameTab);
    if (elf64->sectNameTab == NULL)
        return ELF64_NO_MEM;

    return ELF64_OK;
}


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf64 - Elf64File structer with initialized fields: fd, elf64Header and
 *        sectionHeaders.
 * Output:
 *  Success:
 *      ELF64_OK
 *  Fail:
 *      ELF64_INV_ARG, ELF64_NO_MEM, ELF64_NO_SYM_NAME_TAB
 * After:
 *  need to free memory
 */
static ELF64_ERROR elf64ParseSymNameTab(Elf64File *elf64)
{
    if (elf64 == NULL)
        return ELF64_INV_ARG;

    Elf64Shdr *strtab = elf64GetSectionByName(elf64, STRTAB);
    if (strtab == NULL)
        return ELF64_NO_SYM_NAME_TAB;

    char *symNameTab = (char*) elf64ReadSection(elf64, strtab);
    if (symNameTab == NULL)
        return ELF64_NO_MEM;

    elf64->symNameTab = symNameTab;
    return ELF64_OK;
}


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf64 - Elf64File structer with initialized fields: fd, elf64Header and
 *        sectionHeaders.
 * Output:
 *  Success:
 *      ELF64_OK
 *  Fail:
 *      ELF64_INV_ARG, ELF64_NO_MEM, ELF64_NO_DYN_SYM_NAME_TAB
 * After:
 *  need to free memory
 */
static ELF64_ERROR elf64ParseDynSymNameTab(Elf64File *elf64)
{
    if (elf64 == NULL)
        return ELF64_INV_ARG;

    Elf64Shdr *dynSymTab = elf64GetSectionByName(elf64, DYNSTR);
    if (dynSymTab == NULL)
        return ELF64_NO_DYN_SYM_NAME_TAB;

    char *dynSymNameTab = (char*) elf64ReadSection(elf64, dynSymTab);
    if (dynSymNameTab == NULL)
        return ELF64_NO_MEM;

    elf64->dynSymNameTab = dynSymNameTab;
    return ELF64_OK;
}


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf64 - Elf64File structer with initialized fields: fd, elf64Header and
 *          sectionHeaders.
 * Output:
 *  Success:
 *      ELF64_OK
 *  Fail:
 *      ELF64_INV_ARG, ELF64_NO_MEM, ELF64_NO_RELAPLT
 * After:
 *  need to free memory
 */
static ELF64_ERROR elf64ParseRelaPlt(Elf64File *elf64)
{
    if (elf64 == NULL || elf64->fn == NULL)
        return ELF64_INV_ARG;

    Elf64Shdr *relaplt = elf64GetSectionByName(elf64, RELAPLT);
    if (relaplt == NULL)
        return ELF64_NO_RELAPLT;
    else {
        elf64->relaplt = (Elf64Rel*)elf64ReadSection(elf64, relaplt);
        if (elf64->relaplt == NULL)
            return ELF64_NO_MEM;
    }

    return ELF64_OK;
}


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf64 - Elf64File structer with initialized fields: fd, elf64Header and
 *          sectionHeaders.
 * Output:
 *  Success:
 *      ELF64_OK
 *  Fail:
 *      ELF64_INV_ARG, ELF64_NO_MEM, ELF64_NO_RELADYN
 * After:
 *  need to free memory
 */
static ELF64_ERROR elf64ParseRelaDyn(Elf64File *elf64)
{
    if (elf64 == NULL || elf64->fn == NULL)
        return ELF64_INV_ARG;

    Elf64Shdr *reladyn = elf64GetSectionByName(elf64, RELADYN);
    if (reladyn == NULL)
        return ELF64_NO_RELADYN;
    else {
        elf64->reladyn = (Elf64Rel*)elf64ReadSection(elf64, reladyn);
        if (elf64->reladyn == NULL)
            return ELF64_NO_MEM;
    }

    return ELF64_OK;
}


Elf64File *elf64Parse(const char *fn)
{
    if (fn == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    LOG("start elf64Parse\n")
    int fd = 0;
    if ((fd = open(fn, O_RDONLY)) < 0) {
        PERROR("open()");
        return NULL;
    }

    Elf64File *elf64 = (Elf64File*) Calloc(1, sizeof(Elf64File));
    if (elf64 == NULL) {
        ERROR("Cannot allocate %zu bytes", sizeof(Elf64File));
        goto eexit_0;
    }

    elf64->fd = fd;
    uint64_t nameSize = strlen(fn) * sizeof(char);
    if ((elf64->fn = (char*) Calloc(nameSize, sizeof(char))) == NULL) {
        ERROR("Cannot allocate %zu bytes", nameSize);
        goto eexit_1;
    }

    strncpy(elf64->fn, fn, nameSize);

    if (elf64ParseHeader(elf64)) {
        ERROR("Cannot parse elf64 header");
        goto eexit_1;
    }

    if (elf64ParseSections(elf64)) {
        if (IS_ELF64_FILE_OBJ(elf64)) {
            /* Object file must have sections for linker */
            ERROR("There is no section headers table");
            goto eexit_1;
        } else
            WARNING("There is no section headers table");
    }

    if (elf64ParseSegments(elf64)) {
        if (IS_ELF64_FILE_EXEC(elf64)) {
            /* Exec file must have segments for loader */
            ERROR("There are no program headers");
            goto eexit_1;
        } else
            WARNING("There are no program headers");
    }

    if (elf64ParseSymbols(elf64)) {
        if (IS_ELF64_FILE_OBJ(elf64)) {
            /* Object file must have symbols for linker */
            ERROR("There is no symbols table");
            goto eexit_1;
        } else
            WARNING("There is no symbols table");
    } else
        elf64ParseSortSymTab(elf64);

    if (elf64ParseSectNameTab(elf64)) {
        if (elf64->symtab || elf64->dynsym) {
            ERROR("There is no section name table");
            goto eexit_1;
        } else
            WARNING("There is no section name table");
    }

    if (elf64ParseSymNameTab(elf64)) {
        if (elf64->symtab || elf64->dynsym) {
            ERROR("Cannot parse symbol name table.");
            goto eexit_1;
        } else
            WARNING("Cannot parse symbol name table.");
    }

    if (elf64ParseDynSymNameTab(elf64)) {
        if (elf64->dynsym != NULL) {
            ERROR("Cannot parse dynamic symbol name table.");
            goto eexit_1;
        } else
            WARNING("Cannot parse dynamic symbol name table.");
    }

    if (elf64ParseRelaPlt(elf64)) {
        if (elf64->dynsym) {
            ERROR("There is no " RELAPLT " table");
            goto eexit_1;
        } else
            WARNING("There is no " RELAPLT " table");
    }

    if (elf64ParseRelaDyn(elf64)) {
        if (elf64->dynsym) {
            ERROR("There is no " RELADYN " table");
            goto eexit_1;
        } else
            WARNING("There is no " RELADYN " table");
    }

    return elf64;

eexit_1:
    elf64Free(elf64);
    return NULL;
eexit_0:
    close(fd);
    return NULL;
}


void elf64Free(Elf64File *elf64)
{
    if (elf64 == NULL) {
        ERROR("Try to free NULL ptr stuct");
        return;
    }

    LOG("start elf64Free");

    if (elf64->fd >= 0) {
        LOG("close file");
        close(elf64->fd);
        elf64->fd = 0;
    }

    if (elf64->fn != NULL) {
        LOG("free file name");
        Free(elf64->fn);
        elf64->fn = NULL;
    }

    if (elf64->header != NULL) {
        LOG("free header");
        Free(elf64->header);
        elf64->header = NULL;
    }

    if (elf64->segments != NULL) {
        LOG("free segments");
        Free(elf64->segments);
        elf64->segments = NULL;
    }

    if (elf64->sections != NULL) {
        LOG("free sections");
        Free(elf64->sections);
        elf64->sections = NULL;
    }

    if (elf64->symtab != NULL) {
        LOG("free symbols");
        Free(elf64->symtab);
        elf64->symtab = NULL;
    }

    if (elf64->dynsym != NULL) {
        LOG("free dynamic symbols");
        Free(elf64->dynsym);
        elf64->dynsym = NULL;
    }

    if (elf64->sortSymtab != NULL) {
        LOG("free sortSymtab");
        Free(elf64->sortSymtab);
        elf64->sortSymtab = NULL;
    }

    if (elf64->relaplt != NULL) {
        LOG("free relaplt");
        Free(elf64->relaplt);
        elf64->relaplt = NULL;
    }

    if (elf64->reladyn != NULL) {
        LOG("free reladyn");
        Free(elf64->reladyn);
        elf64->reladyn = NULL;
    }

    if (elf64->sectNameTab != NULL) {
        LOG("free sectNameTab");
        Free(elf64->sectNameTab);
        elf64->sectNameTab = NULL;
    }

    if (elf64->symNameTab != NULL) {
        LOG("free symNameTab");
        Free(elf64->symNameTab);
        elf64->symNameTab = NULL;
    }

    if (elf64->dynSymNameTab != NULL) {
        LOG("free dynSymNameTab");
        Free(elf64->dynSymNameTab);
        elf64->dynSymNameTab = NULL;
    }

    LOG("end elf64Free");
    Free(elf64);
}


ELF64_ERROR elf64FullCheckFile(const Elf64File *elf64)
{
    if (elf64 == NULL) {
        ERROR("Invalid arguments");
        return ELF64_INV_ARG;
    }

    if (elf64->header == NULL) {
        ERROR("Uninitialized field elf64->header.");
        return ELF64_NO_HEADER;
    }

    if (elf64->segments == NULL) {
        if (IS_ELF64_FILE_EXEC(elf64)) {
            ERROR("Uninitialized field elf64->segments.");
            return ELF64_NO_SEGMENTS;
        } else
            WARNING("Uninitialized field elf64->segments.");
    }

    if (elf64->sections == NULL) {
        if (IS_ELF64_FILE_OBJ(elf64)) {
            ERROR("Uninitialized field elf64->sections.");
            return ELF64_NO_SECTIONS;
        } else
            WARNING("Uninitialized field elf64->sections.");
    }

    if (elf64->symtab == NULL) {
        if (IS_ELF64_FILE_OBJ(elf64)) {
            ERROR("Uninitialized field elf64->symtab.");
            return ELF64_NO_SYMTAB;
        } else
            WARNING("Uninitialized field elf64->symtab.");
    }

    if (elf64->sortSymtab == NULL) {
        if (IS_ELF64_FILE_OBJ(elf64)) {
            ERROR("Uninitialized field elf64->sortSymtab.");
            return ELF64_NO_SYMTAB;
        } else
            WARNING("Uninitialized field elf64->sortSymtab.");
    }

    if (elf64->dynsym == NULL)
        WARNING("Uninitialized field elf64->dynsym.\n");

    if (elf64->relaplt == NULL) {
        if (elf64->dynsym != NULL) {
            ERROR("Uninitialized field elf64->relTabs.relaplt.");
            return ELF64_NO_RELAPLT;
        } else
            WARNING("Uninitialized field elf64->relTabs.relaplt.\n");
    }

    if (elf64->reladyn == NULL) {
        if (elf64->dynsym != NULL) {
            ERROR("Uninitialized field elf64->relTabs.reladyn.");
            return ELF64_NO_RELADYN;
        } else
            WARNING("Uninitialized field elf64->relTabs.reladyn.\n");
    }

    if (elf64->sectNameTab == NULL) {
        if (elf64->symtab) {
            ERROR("Uninitialized field elf64->sectNameTab.");
            return ELF64_NO_SH_NAME_TAB;
        } else
            WARNING("Uninitialized field elf64->sectNameTab.");
    }

    if (elf64->symNameTab  == NULL) {
        if (elf64->symtab != NULL) {
            ERROR("Uninitialized field elf64->symNameTab.");
            return ELF64_NO_SYM_NAME_TAB;
        } else {
            WARNING("Uninitialized field elf64->symNameTab.\n");
        }
    }

    if (elf64->dynSymNameTab  == NULL) {
        if (elf64->dynsym != NULL) {
            ERROR("Uninitialized field elf64->dynSymNameTab.");
            return ELF64_NO_DYN_SYM_NAME_TAB;
        } else {
            WARNING("Uninitialized field elf64->dynSymNameTab.\n");
        }
    }

    return ELF64_OK;
}


ELF64_ERROR elf64PrintSymbol(const Elf64File *elf64, const Elf64Sym *sym)
{
    if (elf64FullCheckFile(elf64) || sym == NULL)
        return ELF64_INV_ARG;

    /*
     *typedef struct {
     *       Elf64_Word      st_name;
     *       unsigned char   st_info;
     *       unsigned char   st_other;
     *       Elf64_Half      st_shndx;
     *       Elf64_Addr      st_value;
     *       Elf64_Xword     st_size;
     *   } Elf64_Sym;
     */
    printf("\tname:\t%s\n", elf64GetSymName(elf64, sym));
    printf("\t\tinfo:\t0x%x\n", sym->st_info);
    printf("\t\tother\t0x%x\n", sym->st_other);
    printf("\t\tshndx\t%u\n", sym->st_shndx);
    printf("\t\tvalue\t0x%zx\n", sym->st_value);
    printf("\t\tsize\t0x%zx\n", sym->st_size);

    return ELF64_OK;
}


ELF64_ERROR elf64PrintSymbols(const Elf64File *elf64)
{
    if (elf64FullCheckFile(elf64))
        return ELF64_INV_ARG;

    uint64_t i = 0;
    uint64_t ssymNum = elf64GetAmountSSym(elf64);
    Elf64Sym *ssym = elf64GetSSymTable(elf64);

    printf("static symbols:\n");
    for (i = 0; i < ssymNum; ++i)
        elf64PrintSymbol(elf64, &ssym[i]);

    if (elf64->dynsym) {
        Elf64Sym *dsym = elf64->dynsym;
        Elf64Shdr *dynsymTable = elf64GetSectionByType(elf64, SHT_DYNSYM);
        uint64_t shSize = dynsymTable->sh_size;
        uint64_t dsymNum = shSize/sizeof(Elf64Sym);

        printf("dynamic symbols:\n");
        for (i = 0; i < dsymNum; ++i)
            elf64PrintSymbol(elf64, &dsym[i]);
    }

    return ELF64_OK;
}


Elf64Sym *elf64GetSymByName(const Elf64File *elf64, const char *name)
{
    if (elf64FullCheckFile(elf64) || name == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    char *symNameTable = elf64->symNameTab;
    uint64_t symNum = elf64->symnum;

    uint64_t i = 0;
    for (i = 0; i < symNum; ++i) {
        uint64_t tableOff = elf64->symtab[i].st_name;
        char *symName = (char*)((uint64_t)symNameTable + tableOff);
        if (!strcmp(symName, name))
            return (Elf64Sym*) &elf64->symtab[i];
    }

    char *dynsymNameTable = elf64->dynSymNameTab;
    symNum = elf64->dynsymnum;

    for (i = 0; i < symNum; ++i) {
        uint64_t tableOff = elf64->dynsym[i].st_name;
        char *symName = (char*)((uint64_t)dynsymNameTable + tableOff);
        if (!strcmp(symName, name))
            return (Elf64Sym*) &elf64->symtab[i];
    }

    WARNING("There is no symbol %s.", name);
    return NULL;
}


char *elf64GetSymName(const Elf64File *elf64, const Elf64Sym *sym)
{
    if (elf64FullCheckFile(elf64) || sym == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    return (char*)((uint64_t)elf64->symNameTab + sym->st_name);
}


int elf64CmpSym(const void *a, const void *b)
{
    int64_t distance = (int64_t)(((const Elf64Sym*)a)->st_value - ((const Elf64Sym*)b)->st_value);
    if (distance > 0)
        return 1;
    else if (distance < 0)
        return -1;
    else
        return 0;
}


Elf64Sym *elf64GetSSymTable(const Elf64File *elf64)
{
    if (elf64 == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    return elf64->symtab;
}


Elf64Sym *elf64GetSSymSortTable(const Elf64File *elf64)
{
    if (elf64 == NULL || elf64->sortSymtab == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    return elf64->sortSymtab;
}


uint64_t elf64GetAmountSSym(const Elf64File *elf64)
{
    if (elf64 == NULL) {
        ERROR("Invalid arguments");
        return ELF64_INV_ARG;
    }

    return elf64->symnum;
}


uint64_t elf64GetSSymAddr(const Elf64Sym *sym)
{
    if (sym == NULL) {
        ERROR("Invalid arguments");
        return ELF64_INV_ARG;
    }

    return sym->st_value;
}


uint64_t elf64GetAddrSymByName(const Elf64File *elf64, const char *name)
{
    if (elf64FullCheckFile(elf64) || name == NULL) {
        ERROR("Invalid arguments");
        return ELF64_INV_ARG;
    }

    // TODO: work only for static symbols, for dynamic need signal
    return elf64GetSymByName(elf64, name)->st_value;
}


uint64_t elf64GetSSymSize(const Elf64File *elf64, const Elf64Sym *sym)
{
    if (elf64 == NULL || sym == NULL) {
        ERROR("Invalid arguments");
        return ELF64_INV_ARG;
    }

    return sym->st_size;
}


uint64_t elf64GetSSymFileoff(const Elf64File *elf64, const Elf64Sym *sym)
{
    if (elf64FullCheckFile(elf64) || sym == NULL) {
        ERROR("Invalid arguments");
        return ELF64_INV_ARG;
    }

    uint64_t shndx = sym->st_shndx;
    Elf64Shdr *sect = &elf64->sections[shndx];
    uint64_t diff = sect->sh_addr - sect->sh_offset;
    return sym->st_value - diff;
}


uint64_t elf64GetDSymIndex(const Elf64File *elf64, const char *name)
{
    if (elf64FullCheckFile(elf64) || name == NULL) {
        ERROR("Invalid arguments");
        return ELF64_INV_ARG;
    }

    char *dynsymNameTable = elf64->dynSymNameTab;
    uint64_t symNum = elf64->dynsymnum;

    uint64_t i = 0;
    for (i = 0; i < symNum; ++i) {
        uint64_t tableOff = elf64->dynsym[i].st_name;
        char *symbolName = (char*)((uint64_t)dynsymNameTable + tableOff);
        if (!strcmp(symbolName, name))
            return i;
    }

    return ELF64_NO_SYMBOL;
}


uint64_t elf64GetAmountSegment(const Elf64File *elf64)
{
    if (elf64FullCheckFile(elf64)) {
        ERROR("Invalid argument");
        return ELF64_INV_ARG;
    }

    return elf64->header->e_phnum;
}


Elf64Shdr *elf64GetSectionByType(const Elf64File *elf64, const Elf64Word shType)
{
    if (  elf64 == NULL
       || elf64->header == NULL
       || elf64->sections == NULL
       ) {
        ERROR("Invalid arguments");
        return NULL;
    }

    /***
     * e_shnum - contains the number of entries in the section
     *           header table.
     * sh_type - identifies the section type.
     */
    uint64_t i = 0;
    for (i = 0; i < elf64->header->e_shnum; ++i)
        if (elf64->sections[i].sh_type == shType)
            return (Elf64Shdr*) &elf64->sections[i];

    return NULL;
}


Elf64Shdr *elf64GetSectionByName(const Elf64File *elf64, const char* name)
{
    if (  elf64 == NULL
       || elf64->header == NULL
       || elf64->sections == NULL
       || name == NULL
       ) {
        ERROR("Invalid arguments");
        return NULL;
    }

    char *sectNameTab = elf64->sectNameTab;
    /***
     * e_shnum - contains the number of entries in the section
     *           header table.
     * sh_name - contains the offset, in bytes, to the section name,
     *           relative to the start of the section name string table.
     */
    uint64_t i = 0;
    uint64_t shnum = elf64->header->e_shnum;
    for (i = 0; i < shnum; ++i) {
        uint64_t tableOff = elf64->sections[i].sh_name;
        char *sectName = (char*)((uint64_t)sectNameTab + tableOff);
        if (!strcmp(sectName, name))
            return (Elf64Shdr*) &elf64->sections[i];
    }

    return NULL;
}


Elf64Shdr *elf64GetLastLoadableSection(const Elf64File *elf64)
{
    if (elf64FullCheckFile(elf64)) {
        ERROR("Invalid argument");
        return NULL;
    }

    uint64_t i = elf64GetAmountSection(elf64) - 1;
    if (i == (uint64_t)-1) {
        WARNING("Can not get section amount.");
        return NULL;
    }
    for (;i != (uint64_t)-1; --i)
        if (elf64->sections[i].sh_addr)
            return &elf64->sections[i];

    WARNING("There are no loadable sections.");
    return NULL;
}


void *elf64ReadSection(const Elf64File *elf64, const Elf64Shdr *sectionHeader)
{
    if (elf64 == NULL || elf64->fd < 0 || sectionHeader == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    /***
     * sh_offset -  contains the offset, in bytes, of the beginning
     *              of the section contents in the file.
     * sh_size   -  contains the size, in bytes, of the section.
     */
    int fd = elf64->fd;
    uint64_t shSize = sectionHeader->sh_size;
    uint64_t shOffset = sectionHeader->sh_offset;

    void *section = readFromFile(fd, &shOffset, shSize);
    if (section == NULL) {
        ERROR("Cannot read from file.");
        return NULL;
    }

    return section;
}


uint64_t elf64GetAmountSection(const Elf64File *elf64)
{
    if (elf64FullCheckFile(elf64)) {
        ERROR("Invalid argument");
        return ELF64_INV_ARG;
    }

    return elf64->header->e_shnum;
}


const char* elf64GetSectionName(const Elf64File *elf64, const Elf64Shdr *sect)
{
    if (elf64FullCheckFile(elf64) || sect == NULL) {
        ERROR("Invalid argument");
        return NULL;
    }

    uint64_t nameIndx = sect->sh_name;
    return &elf64->sectNameTab[nameIndx];
}


uint64_t elf64GetSectionSize(const Elf64Shdr *elf64Sect)
{
    if (elf64Sect == NULL) {
        ERROR("Invalid arguments");
        return ELF64_INV_ARG;
    }

    return elf64Sect->sh_size;
}


uint64_t elf64GetSectionVaddr(const Elf64Shdr *elf64Sect)
{
    if (elf64Sect == NULL) {
        ERROR("Invalid arguments");
        return ELF64_INV_ARG;
    }

    return elf64Sect->sh_addr;
}


uint64_t elf64GetSectionFileoff(const Elf64Shdr *elf64Sect)
{
    if (elf64Sect == NULL) {
        ERROR("Invalid arguments");
        return ELF64_INV_ARG;
    }

    return elf64Sect->sh_offset;
}


uint64_t elf64GetRelocationForAddr( const Elf64File *elf64
                                  , const Elf64Shdr *sect
                                  , uint64_t addr
                                  )
{
    if (  elf64FullCheckFile(elf64)
       || sect == NULL
       || addr == (uint64_t)-1
       ) {
        ERROR("Invalid arguments");
        return ELF64_INV_ARG;
    }

    const char *sectName = elf64GetSectionName(elf64, sect);
    if (sectName == NULL) {
        ERROR("getSectionName().");
        return ELF64_NO_SECTION;
    }

    uint64_t size = strlen(".rela");
    size += strlen(sectName);
    char *relaSectName = (char*)Malloc(size);
    if (relaSectName == NULL) {
        ERROR("Cannot alloc %zu bytes.", size);
        return ELF64_NO_MEM;
    }

    char *point = copyString(".rela", relaSectName);
    point = copyString(sectName, point);

    Elf64Shdr *relaSect = elf64GetSectionByName(elf64, relaSectName);
    if (relaSect == NULL) {
        ERROR("Cannot get the section %s.", relaSectName);
        return ELF64_NO_SECTION;
    }

    Elf64Rel *rela = elf64ReadSection(elf64, relaSect);
    if (rela == NULL) {
        ERROR("elf64ReadSection()");
        return ELF64_NO_SECTION;
    }

    uint64_t relaAmount = relaSect->sh_size / sizeof(Elf64Rel);
    uint64_t i = 0;
    for (i = 0; i < relaAmount; ++i) {
        /***
         * r_offset indicates the location at which the relocation should be
         * applied. For a relocatable file, this is the offset, in bytes, from
         * the beginning of the section to the beginning of the storage unit
         * being relocated. For an executable or shared object, this is the
         * virtual address of the storage unit being relocated.
         */
        if (  IS_ELF64_FILE_OBJ(elf64)
           && rela[i].r_offset + sect->sh_addr == addr
           ) {
            LOG("find addr %zx\n", addr);
            break;
        }

        if (  IS_ELF64_FILE_EXEC(elf64)
           && rela[i].r_offset == addr
           ) {
            LOG("find addr %zx\n", addr);
            break;
        }
    }

    if (i == relaAmount) {
        ERROR("There is no relocation info");
        return ELF64_NO_RELOCATION;
    }

    if (ELF64_R_TYPE(rela[i].r_info) == R_X86_64_64) {
        return (uint64_t)rela[i].r_addend;
    } else {
        ERROR("Unknown relocation type.");
        return ELF64_NO_RELOCATION;
    }

    return 0;
}


void *elf64Hook(const Elf64File *elf64, const char *func, const void *hand)
{
    if (  elf64FullCheckFile(elf64)
       || func == NULL
       || hand == NULL
       ) {
        ERROR("Invalid arguments");
        return NULL;
    }

    /***
     * symbolIndex -   index of target symbol in .dynsym section.
     */
    uint64_t symbolIndex = elf64GetDSymIndex(elf64, func);
    if (symbolIndex == (uint64_t)-1) {
        ERROR("Cannot get an index of a dynamic symbol %s.", func);
        return NULL;
    }

    /***
     * shSize      -   contains the size, in bytes, of the section.
     * relpltAmount-   amount of Elf64Rel structures in .rela.ptl section.
     */
    Elf64Shdr *relplt = elf64GetSectionByName(elf64, RELAPLT);
    if (relplt == NULL) {
        ERROR("Cannot get the section " RELAPLT);
        return NULL;
    }

    uint64_t relpltAmount = relplt->sh_size / sizeof(Elf64Rel);

    /***
     * r_info        -   This member gives both the symbol table index,
     *                   with respect to which the relocation must be made,
     *                   and the type of relocation to apply.
     * r_offset      -   This member gives the location at which to apply
     *                   the relocation action.
     * For __x86_64 allowed only PIC code, consequently relocation information
     * for all dynamic symbols are in .rela.plt section.
     * In this case r_offset is an address, where is address for relocation of
     * original function.
     */
    void *relAddr = NULL;
    uint64_t i = 0;
    for (i = 0; i < relpltAmount; ++i)
        if (ELF64_R_SYM(elf64->relaplt[i].r_info) == symbolIndex){
            relAddr = (void*) *(uint64_t*) elf64->relaplt[i].r_offset;
            *(uint64_t*) (elf64->relaplt[i].r_offset) = (uint64_t) hand;

            return relAddr;
        }

    return NULL;
}


void *elf64GetRelocationDataAddr(const Elf64File *elf64, const char *func)
{
    if (elf64FullCheckFile(elf64) || func == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    /***
     * symbolIndex  -   index of target symbol in .dynsym section.
     */
    uint64_t symbolIndex = elf64GetDSymIndex(elf64, func);
    if (symbolIndex == (uint64_t)-1) {
        ERROR("Cannot get an index of a dynamic symbol %s.", func);
        return NULL;
    }

    /***
     * shSize       -   contains the size, in bytes, of the section.
     * relpltAmount -   amount of Elf64Rel structures in .rela.ptl section.
     */
    Elf64Shdr *relplt = elf64GetSectionByName(elf64, RELAPLT);
    if (relplt == NULL) {
        ERROR("Cannot get the section " RELAPLT ".");
        return NULL;
    }

    uint64_t relpltAmount = relplt->sh_size / sizeof(Elf64Rel);

    /***
     * r_info        -   This member gives both the symbol table index,
     *                   with respect to which the relocation must be made,
     *                   and the type of relocation to apply.
     * r_offset      -   This member gives the location at which to apply
     *                   the relocation action.
     * For __x86_64 allowed only PIC code, consequently relocation information
     * for all dynamic symbols are in .rela.plt section.
     * In this case r_offset is an address, where is address for relocation of
     * original function.
     */
    uint64_t i = 0;
    for (i = 0; i < relpltAmount; ++i)
        if (ELF64_R_SYM(elf64->relaplt[i].r_info) == symbolIndex)
            return (void*) elf64->relaplt[i].r_offset;

    return NULL;
}


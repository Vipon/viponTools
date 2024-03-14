/***
 * MIT License
 *
 * Copyright (c) 2021-2023 Konychev Valerii
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
#include "os.h"
#include "mem.h"
#include "file.h"
#include "comdef.h"
#include "string.h"
#include "elf64Parse.h"

/* c standard headers */
#include <inttypes.h>

static ELF64_ERROR elf64ParseArch(Elf64File *elf64)
{
    switch (elf64->header->e_machine) {
    case EM_386:
        elf64->arch = X86;
        break;
    case EM_ARM:
        elf64->arch = ARM;
        break;
    case EM_X86_64:
        elf64->arch = X86_64;
        break;
    case EM_AARCH64:
        elf64->arch = AARCH64;
        break;
    default:
        elf64->arch = UNKNOWN_ARCH;
        break;
    }

    return ELF64_OK;
}

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
    if (elf64 == NULL || IS_INV_FD(elf64->fd))
        return ELF64_INV_ARG;

    uint64_t ehOff = 0;
    Elf64Ehdr *header = (Elf64Ehdr*)((uint8_t*)elf64->faddr + ehOff);
    unsigned char *eIdent = header->e_ident;
    if (  eIdent[EI_MAG0] == '\x7f' && eIdent[EI_MAG1] == 'E'
       && eIdent[EI_MAG2] == 'L' && eIdent[EI_MAG3] == 'F'
       && eIdent[EI_CLASS] == ELFCLASS64) {
        elf64->header = header;
        elf64->type = header->e_type;
        elf64ParseArch(elf64);
        return ELF64_OK;
    } else {
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
    if (elf64 == NULL || elf64->header == NULL || IS_INV_FD(elf64->fd))
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
    uint64_t eShOff = elf64->header->e_shoff;
    uint64_t shNum = elf64->header->e_shnum;
    if (shNum == SHN_UNDEF) {
        Elf64Shdr *sect0 = (Elf64Shdr*)((uint8_t*)elf64->faddr + eShOff);
        if (sect0->sh_size == 0) {
            return ELF64_NO_SECTIONS;
        }
    }

    elf64->sections = (Elf64Shdr*)((uint8_t*)elf64->faddr + eShOff);
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
    if (elf64 == NULL || IS_INV_FD(elf64->fd) || elf64->header == NULL)
        return ELF64_INV_ARG;

    uint64_t phoff = elf64->header->e_phoff;
    uint64_t phnum = elf64->header->e_phnum;
    if (phnum == 0)
        return ELF64_NO_SEGMENTS;

    elf64->segments = (Elf64Phdr*)((uint8_t*)elf64->faddr + phoff);
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
    Elf64Shdr *symtab = elf64GetSectByType(elf64, SHT_SYMTAB);
    if (symtab == NULL)
        return ELF64_NO_SYMTAB;

    elf64->symtab = (Elf64Sym*)((uint8_t*)elf64->faddr + symtab->sh_offset);
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
    Elf64Shdr *dynsym = elf64GetSectByType(elf64, SHT_DYNSYM);
    if (dynsym == NULL)
        return ELF64_NO_DYNSYM;

    elf64->dynsym = (Elf64Sym*)((uint8_t*)elf64->faddr + dynsym->sh_offset);
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
    elf64->sectNameTab = (char*)((uint8_t*)elf64->faddr + nameTab->sh_offset);
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

    Elf64Shdr *strtab = elf64GetSectByName(elf64, STRTAB);
    if (strtab == NULL)
        return ELF64_NO_SYM_NAME_TAB;

    elf64->symNameTab = (char*)((uint8_t*)elf64->faddr + strtab->sh_offset);
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

    Elf64Shdr *dynSymTab = elf64GetSectByName(elf64, DYNSTR);
    if (dynSymTab == NULL)
        return ELF64_NO_DYN_SYM_NAME_TAB;

    elf64->dynSymNameTab = (char*)((uint8_t*)elf64->faddr + dynSymTab->sh_offset);
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

    Elf64Shdr *relaplt = elf64GetSectByName(elf64, RELAPLT);
    if (relaplt == NULL)
        return ELF64_NO_RELAPLT;

    elf64->relaplt = (Elf64Rel*)((uint8_t*)elf64->faddr + relaplt->sh_offset);
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

    Elf64Shdr *reladyn = elf64GetSectByName(elf64, RELADYN);
    if (reladyn == NULL)
        return ELF64_NO_RELADYN;

    elf64->reladyn = (Elf64Rel*)((uint8_t*)elf64->faddr + reladyn->sh_offset);
    return ELF64_OK;
}

static ELF64_ERROR elf64ParseDynamic(Elf64File *elf64)
{
    Elf64Shdr *dyn = elf64GetSectByType(elf64, SHT_DYNAMIC);
    if (dyn == NULL)
        return ELF64_NO_DYNAMIC;

    elf64->dynamic = (Elf64Dyn*)((uint8_t*)elf64->faddr + dyn->sh_offset);

    int64_t i = -1;
    while (elf64->dynamic[++i].d_tag != DT_STRTAB);
    uint64_t vaddr = elf64->dynamic[i].d_un.d_val;
    i = -1;
    while (elf64->dynamic[++i].d_tag != DT_STRSZ);

    Elf64Shdr *sect = elf64GetSectByAddr(elf64, vaddr);
    if (sect == NULL)
        return ELF64_NO_DYNAMIC;

    size_t off = sect->sh_offset + vaddr - sect->sh_addr;
    elf64->dtStrTab = (char*)((uint8_t*)elf64->faddr + off);
    return ELF64_OK;
}

Elf64File *elf64Parse(const char *fn)
{
    if (fn == NULL) {
        LOG_ERROR("Invalid arguments");
        return NULL;
    }

    LOG("start elf64Parse\n")
    FileD fd = open(fn, O_RDONLY);
    if (IS_INV_FD(fd)) {
        PERROR("open()");
        return NULL;
    }

    size_t fs = get_file_size(fd);
    if (fs == (size_t) -1) {
        close(fd);
        return NULL;
    }

    void *faddr = map_file(fd, fs, PROT_READ);
    if (faddr == NULL) {
        LOG_ERROR("Cannot map file");
        close(fd);
        return NULL;
    }

    Elf64File *elf64 = (Elf64File*) Calloc(1, sizeof(Elf64File));
    if (elf64 == NULL) {
        LOG_ERROR("Cannot allocate %zu bytes", sizeof(Elf64File));
        close(fd);
        unmap_file(faddr, fs);
        goto eexit_0;
    }

    elf64->fs = fs;
    elf64->faddr = faddr;
    elf64->fd = fd;
    uint64_t nameSize = strlen(fn) * sizeof(char);
    if ((elf64->fn = (char*) Calloc(nameSize, sizeof(char))) == NULL) {
        LOG_ERROR("Cannot allocate %"PRIu64" bytes", nameSize);
        goto eexit_1;
    }

    strncpy(elf64->fn, fn, nameSize);

    if (elf64ParseHeader(elf64)) {
        LOG_ERROR("Cannot parse elf64 header");
        goto eexit_1;
    }

    if (elf64ParseSections(elf64)) {
        if (IS_ELF64_FILE_OBJ(elf64)) {
            /* Object file must have sections for linker */
            LOG_ERROR("There is no section headers table");
            goto eexit_1;
        } else
            WARNING("There is no section headers table");
    }

    if (elf64ParseSegments(elf64)) {
        if (IS_ELF64_FILE_EXEC(elf64)) {
            /* Exec file must have segments for loader */
            LOG_ERROR("There are no program headers");
            goto eexit_1;
        } else
            WARNING("There are no program headers");
    }

    if (elf64ParseSymbols(elf64)) {
        if (IS_ELF64_FILE_OBJ(elf64)) {
            /* Object file must have symbols for linker */
            LOG_ERROR("There is no symbols table");
            goto eexit_1;
        } else
            WARNING("There is no symbols table");
    }

    if (elf64ParseSectNameTab(elf64)) {
        if (elf64->symtab || elf64->dynsym) {
            LOG_ERROR("There is no section name table");
            goto eexit_1;
        } else
            WARNING("There is no section name table");
    }

    if (elf64ParseSymNameTab(elf64)) {
        if (elf64->symtab || elf64->dynsym) {
            LOG_ERROR("Cannot parse symbol name table.");
            goto eexit_1;
        } else
            WARNING("Cannot parse symbol name table.");
    }

    if (elf64ParseDynSymNameTab(elf64)) {
        if (elf64->dynsym != NULL) {
            LOG_ERROR("Cannot parse dynamic symbol name table.");
            goto eexit_1;
        } else
            WARNING("Cannot parse dynamic symbol name table.");
    }

    if (elf64ParseRelaPlt(elf64)) {
        if (elf64->dynsym) {
            LOG_ERROR("There is no " RELAPLT " table");
            goto eexit_1;
        } else
            WARNING("There is no " RELAPLT " table");
    }

    if (elf64ParseRelaDyn(elf64)) {
        if (elf64->dynsym) {
            LOG_ERROR("There is no " RELADYN " table");
            goto eexit_1;
        } else
            WARNING("There is no " RELADYN " table");
    }

    elf64ParseDynamic(elf64);

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
        LOG_ERROR("Try to free NULL ptr stuct");
        return;
    }

    LOG("start elf64Free");

    if (IS_VLD_FD(elf64->fd)) {
        LOG("close file");
        close(elf64->fd);
        elf64->fd = 0;
    }

    if (elf64->fn != NULL) {
        LOG("free file name");
        Free(elf64->fn);
        elf64->fn = NULL;
    }

    if (elf64->faddr) {
        unmap_file(elf64->faddr, elf64->fs);
        elf64->faddr = NULL;
        elf64->fs = (size_t)-1;
    }

    LOG("end elf64Free");
    Free(elf64);
}


ELF64_ERROR elf64Check(const Elf64File *elf64)
{
    if (elf64 == NULL) {
        LOG_ERROR("Invalid arguments");
        return ELF64_INV_ARG;
    }

    if (elf64->header == NULL) {
        LOG_ERROR("Uninitialized field elf64->header.");
        return ELF64_NO_HEADER;
    }

    if (elf64->segments == NULL) {
        if (IS_ELF64_FILE_EXEC(elf64)) {
            LOG_ERROR("Uninitialized field elf64->segments.");
            return ELF64_NO_SEGMENTS;
        } else
            WARNING("Uninitialized field elf64->segments.");
    }

    if (elf64->sections == NULL) {
        if (IS_ELF64_FILE_OBJ(elf64)) {
            LOG_ERROR("Uninitialized field elf64->sections.");
            return ELF64_NO_SECTIONS;
        } else
            WARNING("Uninitialized field elf64->sections.");
    }

    if (elf64->symtab == NULL) {
        if (IS_ELF64_FILE_OBJ(elf64)) {
            LOG_ERROR("Uninitialized field elf64->symtab.");
            return ELF64_NO_SYMTAB;
        } else
            WARNING("Uninitialized field elf64->symtab.");
    }

    if (elf64->dynsym == NULL)
        WARNING("Uninitialized field elf64->dynsym.\n");

    if (elf64->relaplt == NULL) {
        if (elf64->dynsym != NULL) {
            LOG_ERROR("Uninitialized field elf64->relTabs.relaplt.");
            return ELF64_NO_RELAPLT;
        } else
            WARNING("Uninitialized field elf64->relTabs.relaplt.\n");
    }

    if (elf64->reladyn == NULL) {
        if (elf64->dynsym != NULL) {
            LOG_ERROR("Uninitialized field elf64->relTabs.reladyn.");
            return ELF64_NO_RELADYN;
        } else
            WARNING("Uninitialized field elf64->relTabs.reladyn.\n");
    }

    if (elf64->sectNameTab == NULL) {
        if (elf64->symtab) {
            LOG_ERROR("Uninitialized field elf64->sectNameTab.");
            return ELF64_NO_SH_NAME_TAB;
        } else
            WARNING("Uninitialized field elf64->sectNameTab.");
    }

    if (elf64->symNameTab  == NULL) {
        if (elf64->symtab != NULL) {
            LOG_ERROR("Uninitialized field elf64->symNameTab.");
            return ELF64_NO_SYM_NAME_TAB;
        } else {
            WARNING("Uninitialized field elf64->symNameTab.\n");
        }
    }

    if (elf64->dynSymNameTab  == NULL) {
        if (elf64->dynsym != NULL) {
            LOG_ERROR("Uninitialized field elf64->dynSymNameTab.");
            return ELF64_NO_DYN_SYM_NAME_TAB;
        } else {
            WARNING("Uninitialized field elf64->dynSymNameTab.\n");
        }
    }

    return ELF64_OK;
}


Elf64Sym *elf64GetSymByName(const Elf64File *elf64, const char *name)
{
    if (elf64Check(elf64) || name == NULL) {
        LOG_ERROR("Invalid arguments");
        return NULL;
    }

    char *symNameTab = elf64->symNameTab;
    uint64_t symNum = elf64->symnum;

    uint64_t i = 0;
    for (i = 0; i < symNum; ++i) {
        uint64_t tableOff = elf64->symtab[i].st_name;
        char *symName = (char*)((uint64_t)symNameTab + tableOff);
        if (!strcmp(symName, name))
            return (Elf64Sym*) &elf64->symtab[i];
    }

    char *dynsymNameTab = elf64->dynSymNameTab;
    symNum = elf64->dynsymnum;

    for (i = 0; i < symNum; ++i) {
        uint64_t tableOff = elf64->dynsym[i].st_name;
        char *symName = (char*)((uint64_t)dynsymNameTab + tableOff);
        if (!strcmp(symName, name))
            return (Elf64Sym*) &elf64->symtab[i];
    }

    WARNING("There is no symbol %s.", name);
    return NULL;
}


char *elf64GetSymName(const Elf64File *elf64, const Elf64Sym *sym)
{
    if (elf64Check(elf64) || sym == NULL) {
        LOG_ERROR("Invalid arguments");
        return NULL;
    }

    return (char*)((uint64_t)elf64->symNameTab + sym->st_name);
}


char *elf64GetDSymName(const Elf64File *elf64, const Elf64Sym *sym)
{
    return (char*)((uint64_t)elf64->dynSymNameTab + sym->st_name);
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


Elf64Sym *elf64GetSSymTab(const Elf64File *elf64)
{
    if (elf64 == NULL) {
        LOG_ERROR("Invalid arguments");
        return NULL;
    }

    return elf64->symtab;
}

Elf64Sym *elf64GetDSymTab(const Elf64File *elf)
{
    if (elf == NULL || elf->dynsym == NULL) {
        LOG_ERROR("Invalid arguments");
        return NULL;
    }

    return elf->dynsym;
}

uint64_t elf64GetAmountSSym(const Elf64File *elf64)
{
    if (elf64 == NULL) {
        LOG_ERROR("Invalid arguments");
        return ELF64_INV_ARG;
    }

    return elf64->symnum;
}

uint64_t elf64GetAmountDSym(const Elf64File *elf)
{
    if (elf == NULL) {
        LOG_ERROR("Invalid arguments");
        return ELF64_INV_ARG;
    }

    return elf->dynsymnum;
}

uint64_t elf64GetSSymAddr(const Elf64Sym *sym)
{
    if (sym == NULL) {
        LOG_ERROR("Invalid arguments");
        return ELF64_INV_ARG;
    }

    return sym->st_value;
}


uint64_t elf64GetAddrSymByName(const Elf64File *elf64, const char *name)
{
    if (elf64Check(elf64) || name == NULL) {
        LOG_ERROR("Invalid arguments");
        return ELF64_INV_ARG;
    }

    // TODO: work only for static symbols, for dynamic need signal
    return elf64GetSymByName(elf64, name)->st_value;
}


uint64_t elf64GetSSymSize(const Elf64File *elf64, const Elf64Sym *sym)
{
    if (elf64 == NULL || sym == NULL) {
        LOG_ERROR("Invalid arguments");
        return ELF64_INV_ARG;
    }

    return sym->st_size;
}


uint64_t elf64GetSSymFileoff(const Elf64File *elf64, const Elf64Sym *sym)
{
    if (elf64Check(elf64) || sym == NULL) {
        LOG_ERROR("Invalid arguments");
        return ELF64_INV_ARG;
    }

    uint64_t shndx = sym->st_shndx;
    Elf64Shdr *sect = &elf64->sections[shndx];
    uint64_t diff = sect->sh_addr - sect->sh_offset;
    return sym->st_value - diff;
}


uint64_t elf64GetDSymIndxByName(const Elf64File *elf64, const char *name)
{
    if (elf64Check(elf64) || name == NULL) {
        LOG_ERROR("Invalid arguments");
        return ELF64_INV_ARG;
    }

    char *dynsymNameTab = elf64->dynSymNameTab;
    uint64_t symNum = elf64->dynsymnum;

    uint64_t i = 0;
    for (i = 0; i < symNum; ++i) {
        uint64_t tableOff = elf64->dynsym[i].st_name;
        char *symbolName = (char*)((uint64_t)dynsymNameTab + tableOff);
        if (!strcmp(symbolName, name))
            return i;
    }

    return ELF64_NO_SYMBOL;
}


uint64_t elf64GetAmountSeg(const Elf64File *elf64)
{
    if (elf64Check(elf64)) {
        LOG_ERROR("Invalid argument");
        return ELF64_INV_ARG;
    }

    return elf64->header->e_phnum;
}


Elf64Shdr *elf64GetSectByType(const Elf64File *elf64, const Elf64Word shType)
{
    if (  elf64 == NULL
       || elf64->header == NULL
       || elf64->sections == NULL
       ) {
        LOG_ERROR("Invalid arguments");
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


Elf64Shdr *elf64GetSectByName(const Elf64File *elf64, const char* name)
{
    if (  elf64 == NULL
       || elf64->header == NULL
       || elf64->sections == NULL
       || name == NULL
       ) {
        LOG_ERROR("Invalid arguments");
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

Elf64Shdr *elf64GetSectByAddr(const Elf64File *elf64, uint64_t addr)
{
    uint64_t i = 0;
    uint64_t shnum = elf64->header->e_shnum;
    for (i = 0; i < shnum; ++i) {
        uint64_t start = elf64->sections[i].sh_addr;
        uint64_t end = start + elf64->sections[i].sh_size;
        if (addr >= start && addr < end)
            return (Elf64Shdr*)(elf64->sections + i);
    }

    return NULL;
}

Elf64Shdr *elf64GetLastLoadableSect(const Elf64File *elf64)
{
    if (elf64Check(elf64)) {
        LOG_ERROR("Invalid argument");
        return NULL;
    }

    uint64_t i = elf64GetAmountSect(elf64) - 1;
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

void *elf64ReadSect(const Elf64File *elf64, const Elf64Shdr *sectionHeader)
{
    if (elf64 == NULL || IS_INV_FD(elf64->fd) || sectionHeader == NULL) {
        LOG_ERROR("Invalid arguments");
        return NULL;
    }

    /***
     * sh_offset -  contains the offset, in bytes, of the beginning
     *              of the section contents in the file.
     * sh_size   -  contains the size, in bytes, of the section.
     */
    FileD fd = elf64->fd;
    uint64_t shSize = sectionHeader->sh_size;
    uint64_t shOffset = sectionHeader->sh_offset;

    void *section = readFromFile(fd, (size_t*)&shOffset, shSize);
    if (section == NULL) {
        LOG_ERROR("Cannot read from file.");
        return NULL;
    }

    return section;
}


uint64_t elf64GetAmountSect(const Elf64File *elf64)
{
    if (elf64Check(elf64)) {
        LOG_ERROR("Invalid argument");
        return ELF64_INV_ARG;
    }

    return elf64->header->e_shnum;
}


const char* elf64GetSectName(const Elf64File *elf64, const Elf64Shdr *sect)
{
    if (elf64Check(elf64) || sect == NULL) {
        LOG_ERROR("Invalid argument");
        return NULL;
    }

    uint64_t nameIndx = sect->sh_name;
    return &elf64->sectNameTab[nameIndx];
}


uint64_t elf64GetSectSize(const Elf64Shdr *elf64Sect)
{
    if (elf64Sect == NULL) {
        LOG_ERROR("Invalid arguments");
        return ELF64_INV_ARG;
    }

    return elf64Sect->sh_size;
}


uint64_t elf64GetSectAddr(const Elf64Shdr *elf64Sect)
{
    if (elf64Sect == NULL) {
        LOG_ERROR("Invalid arguments");
        return ELF64_INV_ARG;
    }

    return elf64Sect->sh_addr;
}


uint64_t elf64GetSectFileoff(const Elf64Shdr *elf64Sect)
{
    if (elf64Sect == NULL) {
        LOG_ERROR("Invalid arguments");
        return ELF64_INV_ARG;
    }

    return elf64Sect->sh_offset;
}


uint64_t elf64GetRelocForAddr( const Elf64File *elf64
                             , const Elf64Shdr *sect
                             , uint64_t addr
                             )
{
    if (  elf64Check(elf64)
       || sect == NULL
       || addr == (uint64_t)-1
       ) {
        LOG_ERROR("Invalid arguments");
        return ELF64_INV_ARG;
    }

    const char *sectName = elf64GetSectName(elf64, sect);
    if (sectName == NULL) {
        LOG_ERROR("getSectName().");
        return ELF64_NO_SECTION;
    }

    uint64_t size = strlen(".rela");
    size += strlen(sectName);
    char *relaSectName = (char*)Malloc(size);
    if (relaSectName == NULL) {
        LOG_ERROR("Cannot alloc %"PRIu64" bytes.", size);
        return ELF64_NO_MEM;
    }

    char *point = copyString(".rela", relaSectName);
    point = copyString(sectName, point);

    Elf64Shdr *relaSect = elf64GetSectByName(elf64, relaSectName);
    if (relaSect == NULL) {
        LOG_ERROR("Cannot get the section %s.", relaSectName);
        return ELF64_NO_SECTION;
    }

    Elf64Rel *rela = elf64ReadSect(elf64, relaSect);
    if (rela == NULL) {
        LOG_ERROR("elf64ReadSect()");
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
            LOG("find addr %"PRIu64"\n", addr);
            break;
        }

        if (  IS_ELF64_FILE_EXEC(elf64)
           && rela[i].r_offset == addr
           ) {
            LOG("find addr %"PRIu64"\n", addr);
            break;
        }
    }

    if (i == relaAmount) {
        LOG_ERROR("There is no relocation info");
        return ELF64_NO_RELOCATION;
    }

    if (ELF64_R_TYPE(rela[i].r_info) == R_X86_64_64) {
        return (uint64_t)rela[i].r_addend;
    } else {
        LOG_ERROR("Unknown relocation type.");
        return ELF64_NO_RELOCATION;
    }
}


void *elf64GetRelocDataAddr(const Elf64File *elf64, const char *func)
{
    if (elf64Check(elf64) || func == NULL) {
        LOG_ERROR("Invalid arguments");
        return NULL;
    }

    /***
     * symbolIndex  -   index of target symbol in .dynsym section.
     */
    uint64_t symbolIndex = elf64GetDSymIndxByName(elf64, func);
    if (symbolIndex == (uint64_t)-1) {
        LOG_ERROR("Cannot get an index of a dynamic symbol %s.", func);
        return NULL;
    }

    /***
     * shSize       -   contains the size, in bytes, of the section.
     * relpltAmount -   amount of Elf64Rel structures in .rela.ptl section.
     */
    Elf64Shdr *relplt = elf64GetSectByName(elf64, RELAPLT);
    if (relplt == NULL) {
        LOG_ERROR("Cannot get the section " RELAPLT ".");
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

uint16_t elf64GetSymVersionByIndx(const Elf64File *elf, uint64_t indx)
{
    if (elf == NULL)
        return 0;

    Elf64Shdr *verSect = elf64GetSectByType(elf, SHT_GNU_versym);
    if (verSect == NULL)
        return 0;

    uint16_t *ver = elf64ReadSect(elf, verSect);
    if (ver == NULL)
        return 0;

    uint16_t res = ver[indx];
    Free(ver);
    return res;
}

const char *elf64GetVerNameBySymVersion(const Elf64File *elf, uint16_t ver)
{
    uint64_t i = 0;
    while (elf->dynamic[i++].d_tag != DT_VERNEEDNUM);
    uint64_t num = elf->dynamic[--i].d_un.d_val;

    Elf64Shdr *verneedSect = elf64GetSectByType(elf, SHT_GNU_verneed);
    if (verneedSect == NULL)
        return NULL;

    Elf64Verneed *verneed = elf64ReadSect(elf, verneedSect);
    if (verneed == NULL)
        return NULL;

    Elf64Verneed *p = verneed;
    for (i = 0; i < num; ++i) {
        Elf64Vernaux *aux = (Elf64Vernaux*)((size_t)p + p->vn_aux);
        uint64_t j = 0;
        for (j = 0; j < p->vn_cnt; ++j) {
            if (aux->vna_other == ver) {
                size_t off = aux->vna_name;
                Free(verneed);
                return elf->dynSymNameTab + off;
            }
            aux = (Elf64Vernaux*)((size_t)aux + aux->vna_next);
        }
        p = (Elf64Verneed*)((size_t)p + p->vn_next);
    }

    Free(verneed);
    return NULL;
}


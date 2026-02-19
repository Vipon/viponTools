/***
 * MIT License
 *
 * Copyright (c) 2021-2026 Konychev Valerii
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
#include "elf32Parse.h"

/* c standard header */
#include <inttypes.h>

/***
 * @brief parse machine acritecture
 *
 * @param[in,out] elf32 elf32File descriptor
 */
static
void elf32ParseArch(Elf32File *elf32)
{
    switch (elf32->header->e_machine) {
    case EM_386:
        elf32->arch = X86;
        break;
    case EM_ARM:
        elf32->arch = ARM;
        break;
    case EM_X86_64:
        elf32->arch = X86_64;
        break;
    case EM_AARCH64:
        elf32->arch = AARCH64;
        break;
    default:
        elf32->arch = UNKNOWN_ARCH;
        break;
    }
}

/***
 * @brief parse elf32 header
 *
 * @param[in,out] elf32 elf32File descriptor
 *
 * @return ELF32_OK or ELF32_NO_HEADER
 */
static
ELF32_ERROR elf32ParseHeader(Elf32File *elf32)
{
    size_t ehOff = 0;
    Elf32Ehdr *header = (Elf32Ehdr*)(elf32->faddr + ehOff);
    unsigned char *eIdent = header->e_ident;
    if (  eIdent[EI_MAG0] == '\x7f' && eIdent[EI_MAG1] == 'E'
       && eIdent[EI_MAG2] == 'L' && eIdent[EI_MAG3] == 'F'
       && eIdent[EI_CLASS] == ELFCLASS32) {
        elf32->header = header;
        elf32->type = header->e_type;
        elf32ParseArch(elf32);
        return ELF32_OK;
    } else {
        return ELF32_NO_HEADER;
    }
}

/***
 * @brief parse elf sections
 *
 * @param[in,out] elf32 pointer to Elf32File
 *
 * @return ELF32_OK or ELF32_NO_SECTIONS
 */
static
ELF32_ERROR elf32ParseSections(Elf32File *elf32)
{
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
    size_t eShOff = elf32->header->e_shoff;
    uint32_t shNum = elf32->header->e_shnum;
    if (shNum == SHN_UNDEF) {
        Elf32Shdr *sect0 = (Elf32Shdr*)(elf32->faddr + eShOff);
        if (sect0->sh_size == 0) {
            return ELF32_NO_SECTIONS;
        }
    }

    elf32->sections = (Elf32Shdr*)(elf32->faddr + eShOff);
    return ELF32_OK;
}

/***
 * @brief parse segments
 *
 * @param[in,out] elf32 pointer to Elf32File
 *
 * @return ELF32_OK or ELF32_NO_SEGMENTS
 */
static
ELF32_ERROR elf32ParseSegments(Elf32File *elf32)
{
    uint32_t phoff = elf32->header->e_phoff;
    uint32_t phnum = elf32->header->e_phnum;
    if (phnum == 0)
        return ELF32_NO_SEGMENTS;

    elf32->segments = (Elf32Phdr*)(elf32->faddr + phoff);
    return ELF32_OK;
}

/***
 * @brief parse symbol tab
 *
 * @param[in,out] elf32 pointer to Elf32File
 *
 * @return ELF32_OK or ELF32_NO_SYMTAB
 */
static
ELF32_ERROR elf32ParseSymTab(Elf32File *elf32)
{
    /***
     * SHT_SYMTAB - Contains a linker symbol table.
     */
    Elf32Shdr *symtab = elf32GetSectByType(elf32, SHT_SYMTAB);
    if (symtab == NULL)
        return ELF32_NO_SYMTAB;

    elf32->symtab = (Elf32Sym*)(elf32->faddr + symtab->sh_offset);
    elf32->symnum = symtab->sh_size/sizeof(Elf32Sym);
    return ELF32_OK;
}


/***
 * @brief parse dynamic symbol table
 *
 * @param[in,out] elf32 pointer to Elf32File
 *
 * @return ELF32_OK or ELF32_NO_DYNSYM
 */
static
ELF32_ERROR elf32ParseDynSym(Elf32File *elf32)
{
    /***
     * SHT_DYNSYM - Contains a dynamic loader symbol table.
     */
    Elf32Shdr *dynsym = elf32GetSectByType(elf32, SHT_DYNSYM);
    if (dynsym == NULL)
        return ELF32_NO_DYNSYM;

    elf32->dynsym = (Elf32Sym*)(elf32->faddr + dynsym->sh_offset);
    elf32->dynsymnum = dynsym->sh_size/sizeof(Elf32Sym);
    return ELF32_OK;
}


/***
 * @brief parse symbols static and dynamic
 *
 * @param[in,out] elf32 pointer to Elf32File
 *
 * @return ELF32_OK or ELF32_NO_SYMBOLS
 */
static
ELF32_ERROR elf32ParseSymbols(Elf32File *elf32)
{
    elf32ParseSymTab(elf32);
    elf32ParseDynSym(elf32);

    if (elf32->symtab == NULL && elf32->dynsym == NULL)
        return ELF32_NO_SYMBOLS;

    return ELF32_OK;
}

/***
 * @brief parse section name table
 *
 * @param[in,out] elf32 pointer to Elf32File
 *
 * @return ELF32_OK or ELF32_NO_SH_NAME_TAB
*/
static
ELF32_ERROR elf32ParseSectNameTab(Elf32File *elf32)
{
    if (elf32 == NULL || elf32->header == NULL || elf32->sections == NULL)
        return ELF32_INV_ARG;

    /***
     * e_shstrndx - contains the section header table index of the
     *              section containing the section name string table.
     *              If there is no section name string table,
     *              this field has the value SHN_UNDEF .
     */
    uint32_t shStrNdx = elf32->header->e_shstrndx;
    if (shStrNdx == SHN_UNDEF)
        return ELF32_NO_SH_NAME_TAB;

    Elf32Shdr *nameTab = &elf32->sections[shStrNdx];
    elf32->sectNameTab = (char*)(elf32->faddr + nameTab->sh_offset);
    return ELF32_OK;
}

/***
 * @brief parse symbol name table
 *
 * @param[in,out] elf32 pointer to Elf32File
 *
 * @return ELF32_OK or ELF32_NO_SYM_NAME_TAB
 */
static
ELF32_ERROR elf32ParseSymNameTab(Elf32File *elf32)
{
    Elf32Shdr *strtab = elf32GetSectByName(elf32, STRTAB);
    if (strtab == NULL)
        return ELF32_NO_SYM_NAME_TAB;

    elf32->symNameTab = (char*)(elf32->faddr + strtab->sh_offset);
    return ELF32_OK;
}

/***
 * @brief parse dynamic symbol name table
 *
 * @param[in,out] elf32 pointer to Elf32File
 *
 * @return ELF32_OK or ELF32_NO_DYN_SYM_NAME_TAB
 */
static
ELF32_ERROR elf32ParseDynSymNameTab(Elf32File *elf32)
{
    Elf32Shdr *dynSymTab = elf32GetSectByName(elf32, DYNSTR);
    if (dynSymTab == NULL)
        return ELF32_NO_DYN_SYM_NAME_TAB;

    elf32->dynSymNameTab = (char*)(elf32->faddr + dynSymTab->sh_offset);
    return ELF32_OK;
}

/***
 * @brief parse rel plt table
 *
 * @param[in,out] elf32 pointer to Elf32File
 *
 * @return ELF32_OK or ELF32_NO_RELPLT
 */
static
ELF32_ERROR elf32ParseRelPlt(Elf32File *elf32)
{
    Elf32Shdr *relplt = elf32GetSectByName(elf32, RELPLT);
    if (relplt == NULL)
        return ELF32_NO_RELPLT;

    elf32->relplt = (Elf32Rel*)(elf32->faddr + relplt->sh_offset);
    return ELF32_OK;
}

/***
 * @brief parse rela dyn table
 *
 * @param[in,out] elf32 pointer to Elf32File
 *
 * @return ELF32_OK or ELF32_NO_RELDYN
 */
static
ELF32_ERROR elf32ParseRelDyn(Elf32File *elf32)
{
    Elf32Shdr *reldyn = elf32GetSectByName(elf32, RELDYN);
    if (reldyn == NULL)
        return ELF32_NO_RELDYN;

    elf32->reldyn = (Elf32Rel*)(elf32->faddr + reldyn->sh_offset);
    return ELF32_OK;
}


Elf32File *elf32Parse(const char *fn)
{
    if (fn == NULL) {
        LOG_ERROR("Invalid arguments");
        return NULL;
    }

    Elf32File *elf32 = (Elf32File*) Calloc(1, sizeof(Elf32File));
    if (elf32 == NULL) {
        LOG_ERROR("Cannot allocate %zu bytes", sizeof(Elf32File));
        goto eexit0;
    }

    elf32->fd = open(fn, O_RDONLY);
    if (IS_INV_FD(elf32->fd)) {
        PERROR("open()");
        goto eexit1;
    }

    elf32->fs = get_file_size(elf32->fd);
    if (elf32->fs == (size_t) -1) {
        LOG_ERROR("Cannot get file size");
        goto eexit1;
    }

    elf32->faddr = (uint8_t*)map_file(elf32->fd, elf32->fs, PROT_READ);
    if (elf32->faddr == NULL) {
        LOG_ERROR("Cannot map file");
        goto eexit1;
    }

    uint32_t nameSize = (uint32_t)(strlen(fn) * sizeof(char));
    if ((elf32->fn = (char*) Calloc(nameSize, sizeof(char))) == NULL) {
        LOG_ERROR("Cannot allocate %u bytes", nameSize);
        goto eexit1;
    }

    strncpy(elf32->fn, fn, nameSize);

    if (elf32ParseHeader(elf32)) {
        LOG_ERROR("Cannot parse elf32 header");
        goto eexit1;
    }

    if (elf32ParseSections(elf32)) {
        if (IS_ELF32_FILE_OBJ(elf32)) {
            /* Object file must have sections for linker */
            LOG_ERROR("There is no section headers table");
            goto eexit1;
        }
    }

    if (elf32ParseSegments(elf32)) {
        if (IS_ELF32_FILE_EXEC(elf32)) {
            /* Exec file must have segments for loader */
            LOG_ERROR("There are no program headers");
            goto eexit1;
        }
    }

    if (elf32ParseSymbols(elf32)) {
        if (IS_ELF32_FILE_OBJ(elf32)) {
            /* Object file must have symbols for linker */
            LOG_ERROR("There is no symbols table");
            goto eexit1;
        }
    }

    if (elf32ParseSectNameTab(elf32)) {
        if (elf32->symtab || elf32->dynsym) {
            LOG_ERROR("There is no section name table");
            goto eexit1;
        }
    }

    if (elf32ParseSymNameTab(elf32)) {
        if (elf32->symtab || elf32->dynsym) {
            LOG_ERROR("Cannot parse symbol name table.");
            goto eexit1;
        }
    }

    if (elf32ParseDynSymNameTab(elf32)) {
        if (elf32->dynsym != NULL) {
            LOG_ERROR("Cannot parse dynamic symbol name table.");
            goto eexit1;
        }
    }

    if (elf32ParseRelPlt(elf32)) {
        if (elf32->dynsym) {
            LOG_ERROR("There is no " RELPLT " table");
            goto eexit1;
        }
    }

    if (elf32ParseRelDyn(elf32)) {
        if (elf32->dynsym) {
            LOG_ERROR("There is no " RELDYN " table");
            goto eexit1;
        }
    }

    return elf32;

eexit1:
    elf32Free(elf32);
eexit0:
    return NULL;
}


void elf32Free(Elf32File *elf32)
{
    if (elf32 == NULL) {
        return;
    }

    if (IS_VLD_FD(elf32->fd)) {
        close(elf32->fd);
        elf32->fd = 0;
    }
    if (elf32->fn != NULL) {
        Free(elf32->fn);
        elf32->fn = NULL;
    }
    if (elf32->faddr) {
        unmap_file(elf32->faddr, elf32->fs);
    }

    vt_memset_s(elf32, sizeof(Elf32File), 0xFF, sizeof(Elf32File));
    Free(elf32);
}


ELF32_ERROR elf32Check(const Elf32File *elf32)
{
    if (elf32 == NULL) {
        return ELF32_INV_ARG;
    }

    if (elf32->header == NULL) {
        return ELF32_NO_HEADER;
    }

    if (elf32->segments == NULL) {
        if (IS_ELF32_FILE_EXEC(elf32)) {
            return ELF32_NO_SEGMENTS;
        }
    }

    if (elf32->sections == NULL) {
        if (IS_ELF32_FILE_OBJ(elf32)) {
            return ELF32_NO_SECTIONS;
        }
    }

    if (elf32->symtab == NULL) {
        if (IS_ELF32_FILE_OBJ(elf32)) {
            return ELF32_NO_SYMTAB;
        }
    }

    if (elf32->relplt == NULL) {
        if (elf32->dynsym != NULL) {
            return ELF32_NO_RELPLT;
        }
    }

    if (elf32->reldyn == NULL) {
        if (elf32->dynsym != NULL) {
            return ELF32_NO_RELDYN;
        }
    }

    if (elf32->sectNameTab == NULL) {
        if (elf32->symtab) {
            return ELF32_NO_SH_NAME_TAB;
        }
    }

    if (elf32->symNameTab  == NULL) {
        if (elf32->symtab != NULL) {
            return ELF32_NO_SYM_NAME_TAB;
        }
    }

    if (elf32->dynSymNameTab  == NULL) {
        if (elf32->dynsym != NULL) {
            return ELF32_NO_DYN_SYM_NAME_TAB;
        }
    }

    return ELF32_OK;
}


ELF32_ERROR elf32PrintSymbol(const Elf32File *elf32, const Elf32Sym *sym)
{
    if (elf32Check(elf32) || sym == NULL)
        return ELF32_INV_ARG;

    /*
     *typedef struct {
     *       Elf32_Word      st_name;
     *       unsigned char   st_info;
     *       unsigned char   st_other;
     *       Elf32_Half      st_shndx;
     *       Elf32_Addr      st_value;
     *       Elf32_Xword     st_size;
     *   } Elf32_Sym;
     */
    printf("\tname:\t%s\n", elf32GetSymName(elf32, sym));
    printf("\t\tinfo:\t0x%"PRIx32"\n", sym->st_info);
    printf("\t\tother\t0x%"PRIx32"\n", sym->st_other);
    printf("\t\tshndx\t%"PRIx32"\n", sym->st_shndx);
    printf("\t\tvalue\t0x%"PRIx32"\n", sym->st_value);
    printf("\t\tsize\t0x%"PRIx32"\n", sym->st_size);

    return ELF32_OK;
}


ELF32_ERROR elf32PrintSymbols(const Elf32File *elf32)
{
    if (elf32Check(elf32))
        return ELF32_INV_ARG;

    uint32_t i = 0;
    uint32_t ssymNum = elf32GetAmountSSym(elf32);
    Elf32Sym *ssym = elf32GetSSymTab(elf32);

    printf("static symbols:\n");
    for (i = 0; i < ssymNum; ++i)
        elf32PrintSymbol(elf32, &ssym[i]);

    if (elf32->dynsym) {
        Elf32Sym *dsym = elf32->dynsym;
        Elf32Shdr *dynsymTab = elf32GetSectByType(elf32, SHT_DYNSYM);
        uint32_t shSize = dynsymTab->sh_size;
        uint32_t dsymNum = shSize/sizeof(Elf32Sym);

        printf("dynamic symbols:\n");
        for (i = 0; i < dsymNum; ++i)
            elf32PrintSymbol(elf32, &dsym[i]);
    }

    return ELF32_OK;
}


Elf32Sym *elf32GetSymByName(const Elf32File *elf32, const char *name)
{
    if (elf32Check(elf32) || name == NULL) {
        LOG_ERROR("Invalid arguments");
        return NULL;
    }

    char *symNameTab = elf32->symNameTab;
    uint32_t symNum = elf32->symnum;

    uint32_t i = 0;
    for (i = 0; i < symNum; ++i) {
        uint32_t tableOff = elf32->symtab[i].st_name;
        char *symName = (char*)((size_t)symNameTab + tableOff);
        if (!strcmp(symName, name))
            return (Elf32Sym*) &elf32->symtab[i];
    }

    char *dynsymNameTab = elf32->dynSymNameTab;
    symNum = elf32->dynsymnum;

    for (i = 0; i < symNum; ++i) {
        uint32_t tableOff = elf32->dynsym[i].st_name;
        char *symName = (char*)((size_t)dynsymNameTab + tableOff);
        if (!strcmp(symName, name))
            return (Elf32Sym*) &elf32->symtab[i];
    }

    WARNING("There is no symbol %s.", name);
    return NULL;
}


char *elf32GetSymName(const Elf32File *elf32, const Elf32Sym *sym)
{
    if (elf32Check(elf32) || sym == NULL) {
        LOG_ERROR("Invalid arguments");
        return NULL;
    }

    return (char*)((size_t)elf32->symNameTab + sym->st_name);
}


int elf32CmpSym(const void *a, const void *b)
{
    int32_t distance = (int32_t)(((const Elf32Sym*)a)->st_value - ((const Elf32Sym*)b)->st_value);
    if (distance > 0)
        return 1;
    else if (distance < 0)
        return -1;
    else
        return 0;
}


Elf32Sym *elf32GetSSymTab(const Elf32File *elf32)
{
    if (elf32 == NULL) {
        LOG_ERROR("Invalid arguments");
        return NULL;
    }

    return elf32->symtab;
}

uint32_t elf32GetAmountSSym(const Elf32File *elf32)
{
    if (elf32 == NULL) {
        LOG_ERROR("Invalid arguments");
        return ELF32_INV_ARG;
    }

    return elf32->symnum;
}


uint32_t elf32GetSSymAddr(const Elf32Sym *sym)
{
    if (sym == NULL) {
        LOG_ERROR("Invalid arguments");
        return ELF32_INV_ARG;
    }

    return sym->st_value;
}


uint32_t elf32GetAddrSymByName(const Elf32File *elf32, const char *name)
{
    if (elf32Check(elf32) || name == NULL) {
        LOG_ERROR("Invalid arguments");
        return ELF32_INV_ARG;
    }

    // TODO: work only for static symbols, for dynamic need signal
    return elf32GetSymByName(elf32, name)->st_value;
}


uint32_t elf32GetSSymSize(const Elf32File *elf32, const Elf32Sym *sym)
{
    if (elf32 == NULL || sym == NULL) {
        LOG_ERROR("Invalid arguments");
        return ELF32_INV_ARG;
    }

    return sym->st_size;
}


uint32_t elf32GetSSymFileoff(const Elf32File *elf32, const Elf32Sym *sym)
{
    if (elf32Check(elf32) || sym == NULL) {
        LOG_ERROR("Invalid arguments");
        return ELF32_INV_ARG;
    }

    uint32_t shndx = sym->st_shndx;
    Elf32Shdr *sect = &elf32->sections[shndx];
    uint32_t diff = sect->sh_addr - sect->sh_offset;
    return sym->st_value - diff;
}


uint32_t elf32GetDSymIndxByName(const Elf32File *elf32, const char *name)
{
    if (elf32Check(elf32) || name == NULL) {
        LOG_ERROR("Invalid arguments");
        return ELF32_INV_ARG;
    }

    char *dynsymNameTab = elf32->dynSymNameTab;
    uint32_t symNum = elf32->dynsymnum;

    uint32_t i = 0;
    for (i = 0; i < symNum; ++i) {
        uint32_t tableOff = elf32->dynsym[i].st_name;
        char *symbolName = (char*)((size_t)dynsymNameTab + tableOff);
        if (!strcmp(symbolName, name))
            return i;
    }

    return ELF32_NO_SYMBOL;
}


uint32_t elf32GetAmountSeg(const Elf32File *elf32)
{
    if (elf32Check(elf32)) {
        LOG_ERROR("Invalid argument");
        return ELF32_INV_ARG;
    }

    return elf32->header->e_phnum;
}


Elf32Shdr *elf32GetSectByType(const Elf32File *elf32, const Elf32Word shType)
{
    if (  elf32 == NULL
       || elf32->header == NULL
       || elf32->sections == NULL
       ) {
        LOG_ERROR("Invalid arguments");
        return NULL;
    }

    /***
     * e_shnum - contains the number of entries in the section
     *           header table.
     * sh_type - identifies the section type.
     */
    uint32_t i = 0;
    for (i = 0; i < elf32->header->e_shnum; ++i)
        if (elf32->sections[i].sh_type == shType)
            return (Elf32Shdr*) &elf32->sections[i];

    return NULL;
}


Elf32Shdr *elf32GetSectByName(const Elf32File *elf32, const char* name)
{
    if (  elf32 == NULL
       || elf32->header == NULL
       || elf32->sections == NULL
       || name == NULL
       ) {
        LOG_ERROR("Invalid arguments");
        return NULL;
    }

    char *sectNameTab = elf32->sectNameTab;
    /***
     * e_shnum - contains the number of entries in the section
     *           header table.
     * sh_name - contains the offset, in bytes, to the section name,
     *           relative to the start of the section name string table.
     */
    uint32_t i = 0;
    uint32_t shnum = elf32->header->e_shnum;
    for (i = 0; i < shnum; ++i) {
        uint32_t tableOff = elf32->sections[i].sh_name;
        char *sectName = (char*)((size_t)sectNameTab + tableOff);
        if (!strcmp(sectName, name))
            return (Elf32Shdr*) &elf32->sections[i];
    }

    return NULL;
}


Elf32Shdr *elf32GetLastLoadableSect(const Elf32File *elf32)
{
    if (elf32Check(elf32)) {
        LOG_ERROR("Invalid argument");
        return NULL;
    }

    uint32_t i = elf32GetAmountSect(elf32) - 1;
    if (i == (uint32_t)-1) {
        WARNING("Can not get section amount.");
        return NULL;
    }
    for (;i != (uint32_t)-1; --i)
        if (elf32->sections[i].sh_addr)
            return &elf32->sections[i];

    WARNING("There are no loadable sections.");
    return NULL;
}


void *elf32ReadSect(const Elf32File *elf32, const Elf32Shdr *sectionHeader)
{
    if (elf32 == NULL || IS_INV_FD(elf32->fd) || sectionHeader == NULL) {
        LOG_ERROR("Invalid arguments");
        return NULL;
    }

    /***
     * sh_offset -  contains the offset, in bytes, of the beginning
     *              of the section contents in the file.
     * sh_size   -  contains the size, in bytes, of the section.
     */
    FileD fd = elf32->fd;
    uint32_t shSize = sectionHeader->sh_size;
    size_t shOffset = sectionHeader->sh_offset;

    void *section = readFromFile(fd, &shOffset, shSize);
    if (section == NULL) {
        LOG_ERROR("Cannot read from file.");
        return NULL;
    }

    return section;
}


uint32_t elf32GetAmountSect(const Elf32File *elf32)
{
    if (elf32Check(elf32)) {
        LOG_ERROR("Invalid argument");
        return ELF32_INV_ARG;
    }

    return elf32->header->e_shnum;
}


const char* elf32GetSectName(const Elf32File *elf32, const Elf32Shdr *sect)
{
    if (elf32Check(elf32) || sect == NULL) {
        LOG_ERROR("Invalid argument");
        return NULL;
    }

    uint32_t nameIndx = sect->sh_name;
    return &elf32->sectNameTab[nameIndx];
}


uint32_t elf32GetSectSize(const Elf32Shdr *elf32Sect)
{
    if (elf32Sect == NULL) {
        LOG_ERROR("Invalid arguments");
        return ELF32_INV_ARG;
    }

    return elf32Sect->sh_size;
}


uint32_t elf32GetSectAddr(const Elf32Shdr *elf32Sect)
{
    if (elf32Sect == NULL) {
        LOG_ERROR("Invalid arguments");
        return ELF32_INV_ARG;
    }

    return elf32Sect->sh_addr;
}


uint32_t elf32GetSectFileoff(const Elf32Shdr *elf32Sect)
{
    if (elf32Sect == NULL) {
        LOG_ERROR("Invalid arguments");
        return ELF32_INV_ARG;
    }

    return elf32Sect->sh_offset;
}

/* !TODO: rework instead of .rela, should be .rel
uint32_t elf32GetRelocForAddr( const Elf32File *elf32
                             , const Elf32Shdr *sect
                             , uint32_t addr
                             )
{
    if (  elf32Check(elf32)
       || sect == NULL
       || addr == (uint32_t)-1
       ) {
        LOG_ERROR("Invalid arguments");
        return ELF32_INV_ARG;
    }

    const char *sectName = elf32GetSectName(elf32, sect);
    if (sectName == NULL) {
        LOG_ERROR("getSectName().");
        return ELF32_NO_SECTION;
    }

    uint32_t size = strlen(".rela");
    size += strlen(sectName);
    char *relaSectName = (char*)Malloc(size);
    if (relaSectName == NULL) {
        LOG_ERROR("Cannot alloc %u bytes.", size);
        return ELF32_NO_MEM;
    }

    char *point = copyString(".rela", relaSectName);
    point = copyString(sectName, point);

    Elf32Shdr *relaSect = elf32GetSectByName(elf32, relaSectName);
    if (relaSect == NULL) {
        LOG_ERROR("Cannot get the section %s.", relaSectName);
        return ELF32_NO_SECTION;
    }

    Elf32Rel *rela = elf32ReadSect(elf32, relaSect);
    if (rela == NULL) {
        LOG_ERROR("elf32ReadSect()");
        return ELF32_NO_SECTION;
    }

    uint32_t relaAmount = relaSect->sh_size / sizeof(Elf32Rel);
    uint32_t i = 0;
    for (i = 0; i < relaAmount; ++i) {*/
        /***
         * r_offset indicates the location at which the relocation should be
         * applied. For a relocatable file, this is the offset, in bytes, from
         * the beginning of the section to the beginning of the storage unit
         * being relocated. For an executable or shared object, this is the
         * virtual address of the storage unit being relocated.
         */
/*
        if (  IS_ELF32_FILE_OBJ(elf32)
           && rela[i].r_offset + sect->sh_addr == addr
           ) {
            LOG("find addr %x\n", addr);
            break;
        }

        if (  IS_ELF32_FILE_EXEC(elf32)
           && rela[i].r_offset == addr
           ) {
            LOG("find addr %x\n", addr);
            break;
        }
    }

    if (i == relaAmount) {
        LOG_ERROR("There is no relocation info");
        return ELF32_NO_RELOCATION;
    }

    if (ELF32_R_TYPE(rela[i].r_info) == R_386_32) {
        return (uint32_t)rela[i].r_addend;
    } else {
        LOG_ERROR("Unknown relocation type.");
        return ELF32_NO_RELOCATION;
    }
}
*/

void *elf32GetRelocDataAddr(const Elf32File *elf32, const char *func)
{
    if (elf32Check(elf32) || func == NULL) {
        LOG_ERROR("Invalid arguments");
        return NULL;
    }

    /***
     * symbolIndex  -   index of target symbol in .dynsym section.
     */
    uint32_t symbolIndex = elf32GetDSymIndxByName(elf32, func);
    if (symbolIndex == (uint32_t)-1) {
        LOG_ERROR("Cannot get an index of a dynamic symbol %s.", func);
        return NULL;
    }

    /***
     * shSize       -   contains the size, in bytes, of the section.
     * relpltAmount -   amount of Elf32Rel structures in .rela.ptl section.
     */
    Elf32Shdr *relplt = elf32GetSectByName(elf32, RELPLT);
    if (relplt == NULL) {
        LOG_ERROR("Cannot get the section " RELPLT ".");
        return NULL;
    }

    uint32_t relpltAmount = relplt->sh_size / sizeof(Elf32Rel);

    /***
     * r_info        -   This member gives both the symbol table index,
     *                   with respect to which the relocation must be made,
     *                   and the type of relocation to apply.
     * r_offset      -   This member gives the location at which to apply
     *                   the relocation action.
     * For __x86_32 allowed only PIC code, consequently relocation information
     * for all dynamic symbols are in .rela.plt section.
     * In this case r_offset is an address, where is address for relocation of
     * original function.
     */
    uint32_t i = 0;
    for (i = 0; i < relpltAmount; ++i)
        if (ELF32_R_SYM(elf32->relplt[i].r_info) == symbolIndex)
            return (void*) (size_t)elf32->relplt[i].r_offset;

    return NULL;
}


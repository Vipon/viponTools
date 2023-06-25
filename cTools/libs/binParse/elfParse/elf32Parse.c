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
#include "os.h"
#include "mem.h"
#include "file.h"
#include "comdef.h"
#include "string.h"
#include "elf32Parse.h"

/* c standard header */
#include <inttypes.h>

#ifdef __WIN__
// windows.h defines itsown ERROR
# undef ERROR
# define ERROR(...)
#endif

/***
 * Before:
 *  If you need a file position, you should to save it.
 * Description:
 *  Function returns elf32 header, which is readed from file with descriptor @fd.
 * Input:
 *  @fd - target elf32File descriptor.
 * Output:
 *  Success:
 *      ELF32_OK
 *  Fail:
 *      ELF32_INV_ARG, ELF32_NO_MEM, ELF32_NO_HEADER
 * After:
 *  need to free memory.
 */
static ELF32_ERROR elf32ParseHeader(Elf32File *elf32)
{
    if (elf32 == NULL || IS_INV_FD(elf32->fd))
        return ELF32_INV_ARG;

    FileD fd = elf32->fd;
    size_t ehOff = 0;
    uint32_t ehSize = sizeof(Elf32Ehdr);
    Elf32Ehdr *header = (Elf32Ehdr*) readFromFile(fd, &ehOff, ehSize);
    if (header == NULL)
        return ELF32_NO_MEM;

    unsigned char *eIdent = header->e_ident;
    if (  eIdent[EI_MAG0] == '\x7f' && eIdent[EI_MAG1] == 'E'
       && eIdent[EI_MAG2] == 'L' && eIdent[EI_MAG3] == 'F'
       && eIdent[EI_CLASS] == ELFCLASS32) {
        elf32->header = header;
        elf32->type = header->e_type;
        return ELF32_OK;
    } else {
        Free(header);
        return ELF32_NO_HEADER;
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
 *      ELF32_OK
 *  Fail:
 *      ELF32_INV_ARG, ELF32_NO_MEM, ELF32_NO_SECTIONS
 * After all:
 *  need to free memory.
 */
static ELF32_ERROR elf32ParseSections(Elf32File *elf32)
{
    /***
     * Sections are identified by an index into the section header table.
     */
    if (elf32 == NULL || elf32->header == NULL || IS_INV_FD(elf32->fd))
        return ELF32_INV_ARG;

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
    FileD fd = elf32->fd;
    size_t eShOff = elf32->header->e_shoff;
    uint32_t shNum = elf32->header->e_shnum;
    uint32_t shSize = 0;

    if (shNum != SHN_UNDEF)
        shSize = shNum * sizeof(Elf32Shdr);
    else {
        Elf32Shdr *sect0 = (Elf32Shdr*) readFromFile(fd, &eShOff, sizeof(Elf32Shdr));
        shNum = sect0->sh_size;
        if (shNum == 0) {
            free(sect0);
            return ELF32_NO_SECTIONS;
        } else
            shSize = shNum * sizeof(Elf32Shdr);

        free(sect0);
    }

    elf32->sections = (Elf32Shdr*) readFromFile(fd, &eShOff, shSize);
    if (elf32->sections == NULL)
        return ELF32_NO_MEM;

    return ELF32_OK;
}


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf32 - point to target elf32File structure with initialized fields: fd and
 *        elf32Header.
 * Output:
 *  Success:
 *      ELF32_OK
 *  Fail:
 *      ELF32_INV_ARG, ELF32_NO_MEM, ELF32_NO_SEGMENTS
 * After all:
 *  need to free memory.
 */
static ELF32_ERROR elf32ParseSegments(Elf32File *elf32)
{
    if (elf32 == NULL || IS_INV_FD(elf32->fd) || elf32->header == NULL)
        return ELF32_INV_ARG;

    size_t phoff = elf32->header->e_phoff;
    uint32_t phnum = elf32->header->e_phnum;
    if (phnum == 0)
        return ELF32_NO_SEGMENTS;

    elf32->segments = readFromFile(elf32->fd, &phoff, sizeof(Elf32Phdr)*phnum);
    if (elf32->segments == NULL)
        return ELF32_NO_MEM;

    return ELF32_OK;
}


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf32 - Elf32File structer with initialized fields: fd, elf32Header and
 *          sectionHeaders.
 * Output:
 *  Success:
 *      ELF32_OK
 *  Fail:
 *      ELF32_INV_ARG, ELF32_NO_MEM, ELF32_NO_SYMTAB
 * After:
 *  need to free memory
 */
static ELF32_ERROR elf32ParseSymTab(Elf32File *elf32)
{
    if (elf32 == NULL)
        return ELF32_INV_ARG;

    /***
     * SHT_SYMTAB - Contains a linker symbol table.
     */
    Elf32Shdr *symtab = elf32GetSectByType(elf32, SHT_SYMTAB);
    if (symtab == NULL)
        return ELF32_NO_SYMTAB;

    elf32->symtab = (Elf32Sym*) elf32ReadSect(elf32, symtab);
    if (elf32->symtab == NULL)
        return ELF32_NO_MEM;

    elf32->symnum = symtab->sh_size/sizeof(Elf32Sym);
    return ELF32_OK;
}


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  elf32 - Elf32File structer with initialized fields: fd, elf32Header and
 *        sectionHeaders.
 * Output:
 *  Success:
 *      ELF32_OK
 *  Fail:
 *      ELF32_INV_ARG, ELF32_NO_MEM, ELF32_NO_DYNSYM
 * After all:
 *  need to free memory
 */
static ELF32_ERROR elf32ParseDynSym(Elf32File *elf32)
{
    if (elf32 == NULL)
        return ELF32_INV_ARG;

    /***
     * SHT_DYNSYM - Contains a dynamic loader symbol table.
     */
    Elf32Shdr *dynsym = elf32GetSectByType(elf32, SHT_DYNSYM);
    if (dynsym == NULL)
        return ELF32_NO_DYNSYM;

    elf32->dynsym = (Elf32Sym*) elf32ReadSect(elf32, dynsym);
    if (elf32->dynsym == NULL)
        return ELF32_NO_MEM;


    elf32->dynsymnum = dynsym->sh_size/sizeof(Elf32Sym);
    return ELF32_OK;
}


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf32 - Elf32File structer with initialized fields: fd, elf32Header and
 *        sectionHeaders.
 * Output:
 *  Success:
 *      ELF32_OK
 *  Fail:
 *      ELF32_INV_ARG, ELF32_NO_MEM, ELF32_NO_SYMBOLS
 * After all:
 *  need to free memory
 */
static ELF32_ERROR elf32ParseSymbols(Elf32File *elf32)
{
    if (elf32 == NULL)
        return ELF32_INV_ARG;

    elf32ParseSymTab(elf32);
    elf32ParseDynSym(elf32);

    if (elf32->symtab == NULL && elf32->dynsym == NULL)
        return ELF32_NO_SYMBOLS;

    return ELF32_OK;
}


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf32 - Elf32File structer with initialized fields: fd, elf32Header and
 *          sectionHeaders.
 * Output:
 *  Success:
 *      ELF32_OK
 *  Fail:
 *      ELF32_INV_ARG, ELF32_NO_MEM, ELF32_NO_SYMTAB
 * After:
 *  need to free memory
 */
static ELF32_ERROR elf32ParseSortSymTab(Elf32File *elf32)
{
    if (elf32 == NULL)
        return ELF32_INV_ARG;

    /***
     * SHT_SYMTAB - Contains a linker symbol table.
     */
    Elf32Shdr *symtab = elf32GetSectByType(elf32, SHT_SYMTAB);
    if (symtab == NULL)
        return ELF32_NO_SYMTAB;

    uint32_t shSize = symtab->sh_size;
    uint32_t symNum = shSize/sizeof(Elf32Sym);
    Elf32Sym *sortSymtab = (Elf32Sym*) elf32ReadSect(elf32, symtab);

    qsort(sortSymtab, symNum, sizeof(Elf32Sym), elf32CmpSym);

    elf32->sortSymtab = sortSymtab;
    return ELF32_OK;
}


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf32 - Elf32File structer with initialized fields: fd, elf32Header and
 *        sectionHeaders.
 * Output:
 *  Success:
 *      ELF32_OK
 *  Fail:
 *      ELF32_INV_ARG, ELF32_NO_MEM, ELF32_NO_SH_NAME_TAB
 * After:
 *  need to free memory. You should to be careful, see Symbols structure.
*/
static ELF32_ERROR elf32ParseSectNameTab(Elf32File *elf32)
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
    elf32->sectNameTab = (char*) elf32ReadSect(elf32, nameTab);
    if (elf32->sectNameTab == NULL)
        return ELF32_NO_MEM;

    return ELF32_OK;
}


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf32 - Elf32File structer with initialized fields: fd, elf32Header and
 *        sectionHeaders.
 * Output:
 *  Success:
 *      ELF32_OK
 *  Fail:
 *      ELF32_INV_ARG, ELF32_NO_MEM, ELF32_NO_SYM_NAME_TAB
 * After:
 *  need to free memory
 */
static ELF32_ERROR elf32ParseSymNameTab(Elf32File *elf32)
{
    if (elf32 == NULL)
        return ELF32_INV_ARG;

    Elf32Shdr *strtab = elf32GetSectByName(elf32, STRTAB);
    if (strtab == NULL)
        return ELF32_NO_SYM_NAME_TAB;

    char *symNameTab = (char*) elf32ReadSect(elf32, strtab);
    if (symNameTab == NULL)
        return ELF32_NO_MEM;

    elf32->symNameTab = symNameTab;
    return ELF32_OK;
}


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf32 - Elf32File structer with initialized fields: fd, elf32Header and
 *        sectionHeaders.
 * Output:
 *  Success:
 *      ELF32_OK
 *  Fail:
 *      ELF32_INV_ARG, ELF32_NO_MEM, ELF32_NO_DYN_SYM_NAME_TAB
 * After:
 *  need to free memory
 */
static ELF32_ERROR elf32ParseDynSymNameTab(Elf32File *elf32)
{
    if (elf32 == NULL)
        return ELF32_INV_ARG;

    Elf32Shdr *dynSymTab = elf32GetSectByName(elf32, DYNSTR);
    if (dynSymTab == NULL)
        return ELF32_NO_DYN_SYM_NAME_TAB;

    char *dynSymNameTab = (char*) elf32ReadSect(elf32, dynSymTab);
    if (dynSymNameTab == NULL)
        return ELF32_NO_MEM;

    elf32->dynSymNameTab = dynSymNameTab;
    return ELF32_OK;
}


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf32 - Elf32File structer with initialized fields: fd, elf32Header and
 *          sectionHeaders.
 * Output:
 *  Success:
 *      ELF32_OK
 *  Fail:
 *      ELF32_INV_ARG, ELF32_NO_MEM, ELF32_NO_RELAPLT
 * After:
 *  need to free memory
 */
static ELF32_ERROR elf32ParseRelaPlt(Elf32File *elf32)
{
    if (elf32 == NULL || elf32->fn == NULL)
        return ELF32_INV_ARG;

    Elf32Shdr *relaplt = elf32GetSectByName(elf32, RELAPLT);
    if (relaplt == NULL)
        return ELF32_NO_RELAPLT;
    else {
        elf32->relaplt = (Elf32Rel*)elf32ReadSect(elf32, relaplt);
        if (elf32->relaplt == NULL)
            return ELF32_NO_MEM;
    }

    return ELF32_OK;
}


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf32 - Elf32File structer with initialized fields: fd, elf32Header and
 *          sectionHeaders.
 * Output:
 *  Success:
 *      ELF32_OK
 *  Fail:
 *      ELF32_INV_ARG, ELF32_NO_MEM, ELF32_NO_RELADYN
 * After:
 *  need to free memory
 */
static ELF32_ERROR elf32ParseRelaDyn(Elf32File *elf32)
{
    if (elf32 == NULL || elf32->fn == NULL)
        return ELF32_INV_ARG;

    Elf32Shdr *reladyn = elf32GetSectByName(elf32, RELADYN);
    if (reladyn == NULL)
        return ELF32_NO_RELADYN;
    else {
        elf32->reladyn = (Elf32Rel*)elf32ReadSect(elf32, reladyn);
        if (elf32->reladyn == NULL)
            return ELF32_NO_MEM;
    }

    return ELF32_OK;
}


Elf32File *elf32Parse(const char *fn)
{
    if (fn == NULL) {
        LOG_ERROR("Invalid arguments");
        return NULL;
    }

    LOG("start elf32Parse\n")
    FileD fd = open(fn, O_RDONLY);
    if (IS_INV_FD(fd)) {
        PERROR("open()");
        return NULL;
    }

    Elf32File *elf32 = (Elf32File*) Calloc(1, sizeof(Elf32File));
    if (elf32 == NULL) {
        LOG_ERROR("Cannot allocate %zu bytes", sizeof(Elf32File));
        goto eexit_0;
    }

    elf32->fd = fd;
    uint32_t nameSize = (uint32_t)(strlen(fn) * sizeof(char));
    if ((elf32->fn = (char*) Calloc(nameSize, sizeof(char))) == NULL) {
        LOG_ERROR("Cannot allocate %u bytes", nameSize);
        goto eexit_1;
    }

    strncpy(elf32->fn, fn, nameSize);

    if (elf32ParseHeader(elf32)) {
        LOG_ERROR("Cannot parse elf32 header");
        goto eexit_1;
    }

    if (elf32ParseSections(elf32)) {
        if (IS_ELF32_FILE_OBJ(elf32)) {
            /* Object file must have sections for linker */
            LOG_ERROR("There is no section headers table");
            goto eexit_1;
        } else
            WARNING("There is no section headers table");
    }

    if (elf32ParseSegments(elf32)) {
        if (IS_ELF32_FILE_EXEC(elf32)) {
            /* Exec file must have segments for loader */
            LOG_ERROR("There are no program headers");
            goto eexit_1;
        } else
            WARNING("There are no program headers");
    }

    if (elf32ParseSymbols(elf32)) {
        if (IS_ELF32_FILE_OBJ(elf32)) {
            /* Object file must have symbols for linker */
            LOG_ERROR("There is no symbols table");
            goto eexit_1;
        } else
            WARNING("There is no symbols table");
    } else
        elf32ParseSortSymTab(elf32);

    if (elf32ParseSectNameTab(elf32)) {
        if (elf32->symtab || elf32->dynsym) {
            LOG_ERROR("There is no section name table");
            goto eexit_1;
        } else
            WARNING("There is no section name table");
    }

    if (elf32ParseSymNameTab(elf32)) {
        if (elf32->symtab || elf32->dynsym) {
            LOG_ERROR("Cannot parse symbol name table.");
            goto eexit_1;
        } else
            WARNING("Cannot parse symbol name table.");
    }

    if (elf32ParseDynSymNameTab(elf32)) {
        if (elf32->dynsym != NULL) {
            LOG_ERROR("Cannot parse dynamic symbol name table.");
            goto eexit_1;
        } else
            WARNING("Cannot parse dynamic symbol name table.");
    }

    if (elf32ParseRelaPlt(elf32)) {
        if (elf32->dynsym) {
            LOG_ERROR("There is no " RELAPLT " table");
            goto eexit_1;
        } else
            WARNING("There is no " RELAPLT " table");
    }

    if (elf32ParseRelaDyn(elf32)) {
        if (elf32->dynsym) {
            LOG_ERROR("There is no " RELADYN " table");
            goto eexit_1;
        } else
            WARNING("There is no " RELADYN " table");
    }

    return elf32;

eexit_1:
    elf32Free(elf32);
    return NULL;
eexit_0:
    close(fd);
    return NULL;
}


void elf32Free(Elf32File *elf32)
{
    if (elf32 == NULL) {
        LOG_ERROR("Try to free NULL ptr stuct");
        return;
    }

    LOG("start elf32Free");

    if (IS_VLD_FD(elf32->fd)) {
        LOG("close file");
        close(elf32->fd);
        elf32->fd = 0;
    }

    if (elf32->fn != NULL) {
        LOG("free file name");
        Free(elf32->fn);
        elf32->fn = NULL;
    }

    if (elf32->header != NULL) {
        LOG("free header");
        Free(elf32->header);
        elf32->header = NULL;
    }

    if (elf32->segments != NULL) {
        LOG("free segments");
        Free(elf32->segments);
        elf32->segments = NULL;
    }

    if (elf32->sections != NULL) {
        LOG("free sections");
        Free(elf32->sections);
        elf32->sections = NULL;
    }

    if (elf32->symtab != NULL) {
        LOG("free symbols");
        Free(elf32->symtab);
        elf32->symtab = NULL;
    }

    if (elf32->dynsym != NULL) {
        LOG("free dynamic symbols");
        Free(elf32->dynsym);
        elf32->dynsym = NULL;
    }

    if (elf32->sortSymtab != NULL) {
        LOG("free sortSymtab");
        Free(elf32->sortSymtab);
        elf32->sortSymtab = NULL;
    }

    if (elf32->relaplt != NULL) {
        LOG("free relaplt");
        Free(elf32->relaplt);
        elf32->relaplt = NULL;
    }

    if (elf32->reladyn != NULL) {
        LOG("free reladyn");
        Free(elf32->reladyn);
        elf32->reladyn = NULL;
    }

    if (elf32->sectNameTab != NULL) {
        LOG("free sectNameTab");
        Free(elf32->sectNameTab);
        elf32->sectNameTab = NULL;
    }

    if (elf32->symNameTab != NULL) {
        LOG("free symNameTab");
        Free(elf32->symNameTab);
        elf32->symNameTab = NULL;
    }

    if (elf32->dynSymNameTab != NULL) {
        LOG("free dynSymNameTab");
        Free(elf32->dynSymNameTab);
        elf32->dynSymNameTab = NULL;
    }

    LOG("end elf32Free");
    Free(elf32);
}


ELF32_ERROR elf32Check(const Elf32File *elf32)
{
    if (elf32 == NULL) {
        LOG_ERROR("Invalid arguments");
        return ELF32_INV_ARG;
    }

    if (elf32->header == NULL) {
        LOG_ERROR("Uninitialized field elf32->header.");
        return ELF32_NO_HEADER;
    }

    if (elf32->segments == NULL) {
        if (IS_ELF32_FILE_EXEC(elf32)) {
            LOG_ERROR("Uninitialized field elf32->segments.");
            return ELF32_NO_SEGMENTS;
        } else
            WARNING("Uninitialized field elf32->segments.");
    }

    if (elf32->sections == NULL) {
        if (IS_ELF32_FILE_OBJ(elf32)) {
            LOG_ERROR("Uninitialized field elf32->sections.");
            return ELF32_NO_SECTIONS;
        } else
            WARNING("Uninitialized field elf32->sections.");
    }

    if (elf32->symtab == NULL) {
        if (IS_ELF32_FILE_OBJ(elf32)) {
            LOG_ERROR("Uninitialized field elf32->symtab.");
            return ELF32_NO_SYMTAB;
        } else
            WARNING("Uninitialized field elf32->symtab.");
    }

    if (elf32->sortSymtab == NULL) {
        if (IS_ELF32_FILE_OBJ(elf32)) {
            LOG_ERROR("Uninitialized field elf32->sortSymtab.");
            return ELF32_NO_SYMTAB;
        } else
            WARNING("Uninitialized field elf32->sortSymtab.");
    }

    if (elf32->dynsym == NULL)
        WARNING("Uninitialized field elf32->dynsym.\n");

    if (elf32->relaplt == NULL) {
        if (elf32->dynsym != NULL) {
            LOG_ERROR("Uninitialized field elf32->relTabs.relaplt.");
            return ELF32_NO_RELAPLT;
        } else
            WARNING("Uninitialized field elf32->relTabs.relaplt.\n");
    }

    if (elf32->reladyn == NULL) {
        if (elf32->dynsym != NULL) {
            LOG_ERROR("Uninitialized field elf32->relTabs.reladyn.");
            return ELF32_NO_RELADYN;
        } else
            WARNING("Uninitialized field elf32->relTabs.reladyn.\n");
    }

    if (elf32->sectNameTab == NULL) {
        if (elf32->symtab) {
            LOG_ERROR("Uninitialized field elf32->sectNameTab.");
            return ELF32_NO_SH_NAME_TAB;
        } else
            WARNING("Uninitialized field elf32->sectNameTab.");
    }

    if (elf32->symNameTab  == NULL) {
        if (elf32->symtab != NULL) {
            LOG_ERROR("Uninitialized field elf32->symNameTab.");
            return ELF32_NO_SYM_NAME_TAB;
        } else {
            WARNING("Uninitialized field elf32->symNameTab.\n");
        }
    }

    if (elf32->dynSymNameTab  == NULL) {
        if (elf32->dynsym != NULL) {
            LOG_ERROR("Uninitialized field elf32->dynSymNameTab.");
            return ELF32_NO_DYN_SYM_NAME_TAB;
        } else {
            WARNING("Uninitialized field elf32->dynSymNameTab.\n");
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


Elf32Sym *elf32GetSSymSortTab(const Elf32File *elf32)
{
    if (elf32 == NULL || elf32->sortSymtab == NULL) {
        LOG_ERROR("Invalid arguments");
        return NULL;
    }

    return elf32->sortSymtab;
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
    for (i = 0; i < relaAmount; ++i) {
        /***
         * r_offset indicates the location at which the relocation should be
         * applied. For a relocatable file, this is the offset, in bytes, from
         * the beginning of the section to the beginning of the storage unit
         * being relocated. For an executable or shared object, this is the
         * virtual address of the storage unit being relocated.
         */
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
    Elf32Shdr *relplt = elf32GetSectByName(elf32, RELAPLT);
    if (relplt == NULL) {
        LOG_ERROR("Cannot get the section " RELAPLT ".");
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
        if (ELF32_R_SYM(elf32->relaplt[i].r_info) == symbolIndex)
            return (void*) (size_t)elf32->relaplt[i].r_offset;

    return NULL;
}


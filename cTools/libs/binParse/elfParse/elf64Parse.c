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


/*
 * Before:
 *  If you need a file position, you should to save it.
 * Description:
 *  Function returns elf64 header, which is readed from file with descriptor @fd.
 * Input:
 *  @fd - target elf64File descriptor.
 * Output:
 *  Success:
 *      point to elf64Header structure.
 *  Fail:
 *      NULL point.
 * After:
 *  need to free memory.
 */
static Elf64Ehdr *elf64ParseHeader(const int fd)
{
    if (fd < 0) {
        ERROR("Invalid arguments");
        return NULL;
    }

    size_t ehOff = 0;
    size_t ehSize = sizeof(Elf64Ehdr);
    Elf64Ehdr *header = (Elf64Ehdr*) readFromFile(fd, &ehOff, ehSize);
    if (header == NULL) {
        ERROR("readFromFile()");
        return NULL;
    }

    unsigned char *eIdent = header->e_ident;
    if (eIdent[EI_MAG0] == '\x7f' && eIdent[EI_MAG1] == 'E' &&
        eIdent[EI_MAG2] == 'L' && eIdent[EI_MAG3] == 'F')
        return header;
    else {
        STDERROR_PRINT_DEBUG("It isn't ELF file.\n");
        Free(header);
        return NULL;
    }
}


/*
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf64 - point to target elf64File structure with initialized fields: fd and
 *        elf64Header.
 * Output:
 *  Success:
 *      point to elf64 section headers table.
 *  Fail:
 *      NULL point.
 * After all:
 *  need to free memory.
 */
static Elf64Shdr *elf64ParseSectionHeadersTable(const Elf64File *elf64)
{
    /*
     * Sections are identified by an index into the section header table.
     */
    if (elf64 == NULL || elf64->elf64Header == NULL || elf64->fd < 0) {
        ERROR("Invalid arguments");
        return NULL;
    }

    /*
     * e_shoff - contains the file offset, in bytes, of the section header
     *           table.
     * e_shnum - contains the number of entries in the section header table.
     * TODO:
     * If the number of sections is greater than or equal to SHN_LORESERVE,
     * e_shnum has the value SHN_UNDEF. The actual number of section header
     * table entries is contained in the shSize field of the section header
     * at index 0.
     * Otherwise, the shSize member of the initial entry contains the value
     * zero.
     */
    int fd = elf64->fd;
    size_t eShOff = elf64->elf64Header->e_shoff;
    size_t shSize = elf64->elf64Header->e_shnum * sizeof(Elf64Shdr);

    return (Elf64Shdr*) readFromFile(fd, &eShOff, shSize);
}


/*
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf64 - point to target elf64File structure with initialized fields: fd and
 *        elf64Header.
 * Output:
 *  Success:
 *      point to elf64 segments headers table.
 *  Fail:
 *      NULL point.
 * After all:
 *  need to free memory.
 */
static Elf64Phdr *elf64ParseSegments(const Elf64File *elf64)
{
    if(elf64 == NULL || elf64->fd < 0 || elf64->elf64Header == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    size_t phoff = elf64->elf64Header->e_phoff;
    size_t phnum = elf64->elf64Header->e_phnum;

    return readFromFile(elf64->fd, &phoff, sizeof(Elf64Phdr)*phnum);
}


/*
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf64 - Elf64File structer with initialized fields: fd, elf64Header and
 *          sectionHeaders.
 * Output:
 *  Success:
 *      point to elf64 symbol table.
 *  Fail:
 *      NULL point.
 * After:
 *  need to free memory
 */
static Elf64Sym *elf64ParseSymbolTable(const Elf64File *elf64)
{
    if(elf64 == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    /*
     * SHT_SYMTAB - Contains a linker symbol table.
     */
    Elf64Shdr *symbolTable = elf64GetSectionByType(elf64, SHT_SYMTAB);
    if(symbolTable == NULL) {
        STDERROR_PRINT_DEBUG("Cannot get section by type SHT_SYMTAB.");
        return NULL;
    }

    return (Elf64Sym*) elf64ReadSection(elf64, symbolTable);
}


/*
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  elf64 - Elf64File structer with initialized fields: fd, elf64Header and
 *        sectionHeaders.
 * Output:
 *  Success:
 *      point to elf64 dynamic symbol table.
 *  Fail:
 *      NULL point.
 * After all:
 *  need to free memory
 */
static Elf64Sym *elf64ParseDynamicSymbolTable(const Elf64File *elf64)
{
    if(elf64 == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    /*
     * SHT_DYNSYM - Contains a dynamic loader symbol table.
     */
    Elf64Shdr *dynamicSymbolTable = elf64GetSectionByType(elf64, SHT_DYNSYM);
    if(dynamicSymbolTable == NULL) {
        WARNING("There is no SHT_DYNSYM section.\n");
        return NULL;
    }

    return (Elf64Sym*) elf64ReadSection(elf64, dynamicSymbolTable);
}


/*
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  elf64 - Elf64File structer with initialized fields: fd, elf64Header and
 *        sectionHeaders.
 * Output:
 *  Success:
 *      point to Symbols structure, that contains symtab and dynsym tables.
 *  Fail:
 *      NULL point.
 * After all:
 *  need to free memory
 */
static Symbols *elf64ParseSymbols(const Elf64File *elf64)
{
    if(elf64 == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    Symbols *sym = (Symbols*) Malloc(sizeof(Symbols));
    if(sym == NULL) {
        STDERROR_PRINT_DEBUG("Cannot allocate %zu bytes.", sizeof(Symbols));
        return NULL;
    }

    sym->symtab = elf64ParseSymbolTable(elf64);
    if(sym->symtab == NULL) {
        STDERROR_PRINT_DEBUG("Cannot parse a symbol table.");
        Free(sym);
        return NULL;
    }

    sym->dynsym = elf64ParseDynamicSymbolTable(elf64);
    if(sym->dynsym == NULL) {
        WARNING("Cannot parse a dymanic symbol table.\n");
        /*Free(sym->symtab);
        Free(sym);
        return NULL;*/
    }

    return sym;
}


static Elf64Sym *elf64ParseSortSymbolTable(const Elf64File *elf64)
{
    if(elf64 == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    /*
     * SHT_SYMTAB - Contains a linker symbol table.
     */
    Elf64Shdr *symbolTable = elf64GetSectionByType(elf64, SHT_SYMTAB);
    if(symbolTable == NULL) {
        STDERROR_PRINT_DEBUG("Cannot get section by type SHT_SYMTAB.");
        return NULL;
    }

    size_t shSize = symbolTable->sh_size;
    size_t symNum = shSize/sizeof(Elf64Sym);
    Elf64Sym *sortSymtab = (Elf64Sym*) elf64ReadSection(elf64, symbolTable);

    qsort(sortSymtab, symNum, sizeof(Elf64Sym), elf64CmpSym);

    return sortSymtab;
}


/*
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf64 - Elf64File structer with initialized fields: fd, elf64Header and
 *          sectionHeaders.
 * Output:
 *  Success:
 *      point to RelTables structure, that contains .rela.ptl and .rela.dyn
 *      sections.
 *  Fail:
 *      NULL point.
 * After:
 *  need to free memory
 */
static RelTables *elf64ParseRelocationTables(const Elf64File *elf64)
{
    if(elf64 == NULL || elf64->fn == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    RelTables *relTables = (RelTables*) Malloc(sizeof(RelTables));
    if(relTables == NULL) {
        STDERROR_PRINT_DEBUG("Cannot allocate %zu bytes.", sizeof(RelTables));
        return NULL;
    }

    Elf64Shdr *relaplt = elf64GetSectionByName(elf64, REL_PLT);
    if(relaplt == NULL) {
        WARNING("There is no" REL_PLT " section.\n");
        relTables->relaplt = NULL;
    } else {
        relTables->relaplt = (Elf64Rel*)elf64ReadSection(elf64, relaplt);
        if(relTables->relaplt == NULL) {
            WARNING_DEBUG("Cannot read .rela.plt section from file %s", elf64->fn);
            Free(relTables);
            return NULL;
        }
    }

    Elf64Shdr *reladyn = elf64GetSectionByName(elf64, REL_DYN);
    if(reladyn == NULL) {
        WARNING("There is no" REL_DYN " section.\n");
        relTables->reladyn = NULL;
    } else {
        relTables->reladyn = (Elf64Rel*)elf64ReadSection(elf64, reladyn);
        if(relTables->reladyn == NULL) {
            WARNING_DEBUG("Cannot read .rela.dyn section from file %s", elf64->fn);
            Free(relTables->relaplt);
            Free(relTables);
            return NULL;
        }
    }

    return relTables;
}


/*
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf64 - Elf64File structer with initialized fields: fd, elf64Header and
 *        sectionHeaders.
 * Output:
 *  Success:
 *      point to elf64 section name table.
 *  Fail:
 *      NULL point.
 * After:
 *  need to free memory. You should to be careful, see Symbols structure.
*/
static char *elf64ParseSectionNameTable(const Elf64File *elf64)
{
    if(elf64 == NULL || elf64->elf64Header == NULL
                   || elf64->sectionHeaders == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    /*
     * e_shstrndx - contains the section header table index of the
     *              section containing the section name string table.
     *              If there is no section name string table,
     *              this field has the value SHN_UNDEF .
     */
    size_t shStrNdx = elf64->elf64Header->e_shstrndx;
    return (char*) elf64ReadSection(elf64, &elf64->sectionHeaders[shStrNdx]);
}


/*
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf64 - Elf64File structer with initialized fields: fd, elf64Header and
 *        sectionHeaders.
 * Output:
 *  Success:
 *      point to elf64 symbol name table.
 *  Fail:
 *      NULL point.
 * After:
 *  need to free memory
 */
static char *elf64ParseSymbolNameTable(const Elf64File *elf64)
{
    if(elf64 == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    Elf64Shdr *symbolTable = elf64GetSectionByName(elf64, STRTAB);
    if(symbolTable == NULL) {
        STDERROR_PRINT_DEBUG("Cannot get section " STRTAB);
        return NULL;
    }

    return (char*) elf64ReadSection(elf64, symbolTable);
}


/*
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf64 - Elf64File structer with initialized fields: fd, elf64Header and
 *        sectionHeaders.
 * Output:
 *  Success:
 *      point to elf64 dynamic symbol name table.
 *  Fail:
 *      NULL point.
 * After:
 *  need to free memory
 */
static char *elf64ParseDynamicSymbolNameTable(const Elf64File *elf64)
{
    if(elf64 == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    Elf64Shdr *dynamicSymbolTable = elf64GetSectionByName(elf64, DYNSTR);
    if(dynamicSymbolTable == NULL) {
        if (elf64->symbols->dynsym == NULL) {
            WARNING("There is no " DYNSTR " section.\n");
        } else {
            WARNING_DEBUG("There is no " DYNSTR " section.");
        }

        return NULL;
    }

    return (char*) elf64ReadSection(elf64, dynamicSymbolTable);
}


Elf64File *elf64Parse(const char *fn)
{
    if(fn == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    //PRINTF("start elf64Parse.\n");
    int fd = 0;
    if((fd = open(fn, O_RDONLY)) < 0) {
        PERROR("open()");
        return NULL;
    }

    Elf64File *elf64 = (Elf64File*) Calloc(1, sizeof(Elf64File));
    if(elf64 == NULL) {
        ERROR("Cannot allocate %zu bytes", sizeof(Elf64File));
        goto eexit_0;
    }

    elf64->fd = fd;
    size_t nameSize = strlen(fn) * sizeof(char);
    if((elf64->fn = (char*) Calloc(nameSize, sizeof(char))) == NULL) {
        ERROR("Cannot allocate %zu bytes", nameSize);
        goto eexit_1;
    }

    //PRINTF("%s\n", fn);
    strncpy(elf64->fn, fn, nameSize);
    //PRINTF("%s\n", elf64->fn);

    if ((elf64->elf64Header = elf64ParseHeader(fd)) == NULL) {
        STDERROR_PRINT_DEBUG("Cannot parse elf64 header.");
        goto eexit_2;
    }

    elf64->fileType = elf64->elf64Header->e_type;

    if ((elf64->sectionHeaders = elf64ParseSectionHeadersTable(elf64)) == NULL) {
        STDERROR_PRINT_DEBUG("Cannot parse section headers table.");
        goto eexit_3;
    }
    if ((elf64->segments = elf64ParseSegments(elf64)) == NULL) {
        STDERROR_PRINT_DEBUG("Cannot parse segments.");
        goto eexit_4;
    }
    if ((elf64->symbols = elf64ParseSymbols(elf64)) == NULL) {
        STDERROR_PRINT_DEBUG("Cannot parse symbols.");
        goto eexit_5;
    }
    if ((elf64->sortSymtab = elf64ParseSortSymbolTable(elf64)) == NULL) {
        STDERROR_PRINT_DEBUG("Cannot parse sorted symbol table.");
        goto eexit_6;
    }
    if ((elf64->relTables = elf64ParseRelocationTables(elf64)) == NULL) {
        STDERROR_PRINT_DEBUG("Cannot parse relocation table.");
        goto eexit_7;
    }
    if ((elf64->sectionNameTable = elf64ParseSectionNameTable(elf64)) == NULL) {
        STDERROR_PRINT_DEBUG("Cannot parse section name table.");
        goto eexit_8;
    }
    if ((elf64->symbolNameTable = elf64ParseSymbolNameTable(elf64)) == NULL) {
        STDERROR_PRINT_DEBUG("Cannot parse symbol name table.");
        goto eexit_9;
    }

    elf64->dynamicSymbolNameTable = elf64ParseDynamicSymbolNameTable(elf64);
    if (elf64->dynamicSymbolNameTable == NULL && elf64->symbols->dynsym != NULL) {
        STDERROR_PRINT_DEBUG("Cannot parse dynamic symbol name table.");
        goto eexit_10;
    }

    return elf64;

eexit_10:
    Free(elf64->symbolNameTable);
eexit_9:
    Free(elf64->sectionNameTable);
eexit_8:
    Free(elf64->relTables);
eexit_7:
    Free(elf64->sortSymtab);
eexit_6:
    Free(elf64->symbols->symtab);
    Free(elf64->symbols->dynsym);
    Free(elf64->symbols);
eexit_5:
    Free(elf64->segments);
eexit_4:
    Free(elf64->sectionHeaders);
eexit_3:
    Free(elf64->elf64Header);
eexit_2:
    Free(elf64->fn);
eexit_1:
    Free(elf64);
eexit_0:
    close(fd);
    return NULL;
}


void elf64Free(Elf64File *elf64)
{
    //PRINTF("start free_elf64\n");
    if (elf64 == NULL) {
        ERROR("Invalid arguments");
        return;
    }

    if (elf64->fd >= 0) {
        //PRINTF("close file\n");
        close(elf64->fd);
        elf64->fd = 0;
    }
    if (elf64->fn != NULL) {
        //PRINTF("free file name\n");
        Free(elf64->fn);
        elf64->fn = NULL;
    }
    if (elf64->elf64Header != NULL) {
        //PRINTF("free elf64Header\n");
        Free(elf64->elf64Header);
        elf64->elf64Header = NULL;
    }
    if (elf64->segments != NULL) {
        //PRINTF("free segments\n");
        Free(elf64->segments);
        elf64->segments = NULL;
    }
    if (elf64->sectionHeaders != NULL) {
        //PRINTF("free sectionHeaders\n");
        Free(elf64->sectionHeaders);
        elf64->sectionHeaders = NULL;
    }
    if (elf64->symbols != NULL) {
        //PRINTF("free symbols\n");
        if (elf64->symbols->symtab != NULL)
            Free(elf64->symbols->symtab);
        if (elf64->symbols->dynsym != NULL)
            Free(elf64->symbols->dynsym);
        Free(elf64->symbols);
        elf64->symbols = NULL;
    }
    if (elf64->sortSymtab != NULL) {
        //PRINTF("free sortSymtab\n");
        Free(elf64->sortSymtab);
        elf64->sortSymtab = NULL;
    }
    if (elf64->relTables != NULL) {
        //PRINTF("free relTables\n");
        if (elf64->relTables->relaplt != NULL)
            Free(elf64->relTables->relaplt);
        if (elf64->relTables->reladyn != NULL)
            Free(elf64->relTables->reladyn);
        Free(elf64->relTables);
        elf64->relTables = NULL;
    }
    if (elf64->sectionNameTable != NULL) {
        //PRINTF("free sectionNameTable\n");
        Free(elf64->sectionNameTable);
        elf64->sectionNameTable = NULL;
    }
    if (elf64->symbolNameTable != NULL) {
        //PRINTF("freeSymbolNameTable\n");
        Free(elf64->symbolNameTable);
        elf64->symbolNameTable = NULL;
    }
    if (elf64->dynamicSymbolNameTable != NULL) {
        //PRINTF("free dynamicSymbolNameTable\n");
        Free(elf64->dynamicSymbolNameTable);
        elf64->dynamicSymbolNameTable = NULL;
    }

    //PRINTF("end free_elf64\n");
    Free(elf64);
}


int elf64FullCheckFile(const Elf64File *elf64)
{
    if (elf64                             == NULL) {
        ERROR("Invalid arguments");
        return -1;
    }
    if (elf64->fn                         == NULL) {
        STDERROR_PRINT_DEBUG("Uninitialized field elf64->fn.");
        return -2;
    }
    if (elf64->fd                         <   0  ) {
        STDERROR_PRINT_DEBUG("Uninitialized field elf64->fd.");
        return -3;
    }
    if (elf64->elf64Header                 == NULL) {
        STDERROR_PRINT_DEBUG("Uninitialized field elf64->elf64Header.");
        return -4;
    }
    if (elf64->segments                   == NULL) {
        STDERROR_PRINT_DEBUG("Uninitialized field elf64->segments.");
        return -5;
    }
    if (elf64->sectionHeaders            == NULL) {
        STDERROR_PRINT_DEBUG("Uninitialized field elf64->sectionHeaders.");
        return -6;
    }
    if (elf64->symbols                    == NULL) {
        STDERROR_PRINT_DEBUG("Uninitialized field elf64->symbols.");
        return -7;
    }
    if (elf64->symbols->symtab            == NULL) {
        STDERROR_PRINT_DEBUG("Uninitialized field elf64->symbols->symtab.");
        return -8;
    }
    if (elf64->sortSymtab                == NULL) {
        STDERROR_PRINT_DEBUG("Uninitialized field elf64->symbols->symtab.");
        return -9;
    }
    if (elf64->symbols->dynsym            == NULL) {
        WARNING("Uninitialized field elf64->symbols->dynsym.\n");
        //return -10;
    }
    if (elf64->relTables                 == NULL) {
        STDERROR_PRINT_DEBUG("Uninitialized field elf64->relTables.");
        return -11;
    }
    if (elf64->relTables->relaplt        == NULL) {
        if (elf64->symbols->dynsym != NULL) {
            STDERROR_PRINT_DEBUG("Uninitialized field " \
                                                "elf64->relTables->relaplt.");
            return -12;
        } else {
            WARNING("Uninitialized field elf64->relTables->relaplt.\n");
        }
    }
    if (elf64->relTables->reladyn        == NULL) {
        if (elf64->symbols->dynsym != NULL) {
            STDERROR_PRINT_DEBUG("Uninitialized field " \
                                                "elf64->relTables->reladyn.");
            return -13;
        } else {
            WARNING("Uninitialized field elf64->relTables->reladyn.\n");
        }
    }
    if (elf64->sectionNameTable         == NULL) {
        STDERROR_PRINT_DEBUG("Uninitialized field elf64->sectionNameTable.");
        return -14;
    }
    if (elf64->dynamicSymbolNameTable  == NULL) {
        if (elf64->symbols->dynsym != NULL) {
            STDERROR_PRINT_DEBUG("Uninitialized field " \
                                            "elf64->dynamicSymbolNameTable.");
            return -15;
        } else {
            WARNING("Uninitialized field elf64->dynamicSymbolNameTable.\n");
        }
    }

    return 0;
}


int elf64PrintSymbol(const Elf64File *elf64, const Elf64Sym *sym)
{
    if(elf64FullCheckFile(elf64) || sym == NULL) {
        ERROR("Invalid arguments");
        return -1;
    }

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

    return 0;
}


int elf64PrintSymbols(const Elf64File *elf64)
{
    if(elf64FullCheckFile(elf64)) {
        ERROR("Invalid arguments");
        return -1;
    }

    size_t i = 0;
    size_t ssymNum = elf64GetAmountSSym(elf64);
    Elf64Sym *ssym = elf64GetSSymTable(elf64);

    printf("static symbols:\n");
    for (i = 0; i < ssymNum; ++i)
        elf64PrintSymbol(elf64, &ssym[i]);

    if (elf64->symbols->dynsym) {
        Elf64Sym *dsym = elf64->symbols->dynsym;
        Elf64Shdr *dynsymTable = elf64GetSectionByType(elf64, SHT_DYNSYM);
        size_t shSize = dynsymTable->sh_size;
        size_t dsymNum = shSize/sizeof(Elf64Sym);

        printf("dynamic symbols:\n");
        for (i = 0; i < dsymNum; ++i)
            elf64PrintSymbol(elf64, &dsym[i]);
    }

    return 0;
}


Elf64Sym *elf64GetSymByName(const Elf64File *elf64, const char *name)
{
    if(elf64FullCheckFile(elf64) || name == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    /*
     * SHT_SYMTAB - Contains a linker symbol table.
     */
    Elf64Shdr *symTable = elf64GetSectionByType(elf64, SHT_SYMTAB);
    if(symTable == NULL) {
        STDERROR_PRINT_DEBUG("Cannot get section SHT_SYMTAB.");
        return NULL;
    }

    char *symNameTable = elf64->symbolNameTable;
    size_t shSize = symTable->sh_size;
    size_t symNum = shSize/sizeof(Elf64Sym);

    size_t i = 0;
    for(i = 0; i < symNum; ++i) {
        size_t tableOff = elf64->symbols->symtab[i].st_name;
        char *symName = (char*)((size_t)symNameTable + tableOff);
        if(!strcmp(symName, name))
            return (Elf64Sym*) &elf64->symbols->symtab[i];
    }

    /*
     * SHT_DYNSYM - Contains a dynamic loader symbol table.
     */
    Elf64Shdr *dynsymTable = elf64GetSectionByType(elf64, SHT_DYNSYM);
    if(dynsymTable == NULL) {
        STDERROR_PRINT_DEBUG("Cannot get section SHT_DYNSYM.");
        return NULL;
    }

    char *dynsymNameTable = elf64->dynamicSymbolNameTable;
    shSize = dynsymTable->sh_size;
    symNum = shSize/sizeof(Elf64Sym);

    for(i = 0; i < symNum; ++i) {
        size_t tableOff = elf64->symbols->dynsym[i].st_name;
        char *symName = (char*)((size_t)dynsymNameTable + tableOff);
        if(!strcmp(symName, name))
            return (Elf64Sym*) &elf64->symbols->symtab[i];
    }

    STDERROR_PRINT_DEBUG("There is no symbol %s.", name);
    return NULL;
}


char *elf64GetSymName(const Elf64File *elf64, const Elf64Sym *sym)
{
    if (elf64FullCheckFile(elf64) || sym == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    return (char*)((size_t)elf64->symbolNameTable + sym->st_name);
}


int elf64CmpSym(const void *a, const void *b)
{
    intptr_t distance = (intptr_t)(((const Elf64Sym*)a)->st_value - ((const Elf64Sym*)b)->st_value);
    if (distance > 0)
        return 1;
    else if (distance < 0)
        return -1;
    else
        return 0;
}


Elf64Sym *elf64GetSSymTable(const Elf64File *elf64)
{
    if(elf64 == NULL || elf64->symbols == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    return elf64->symbols->symtab;
}


Elf64Sym *elf64GetSSymSortTable(const Elf64File *elf64)
{
    if(elf64 == NULL || elf64->sortSymtab == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    return elf64->sortSymtab;
}


size_t elf64GetAmountSSym(const Elf64File *elf64)
{
    if (elf64 == NULL) {
        ERROR("Invalid arguments");
        return (size_t)-1;
    }

    Elf64Shdr *symbolTable = elf64GetSectionByType(elf64, SHT_SYMTAB);
    if(symbolTable == NULL) {
        STDERROR_PRINT_DEBUG("Cannot get section SHT_SYMTAB.");
        return (size_t)-1;
    }

    size_t shSize = symbolTable->sh_size;
    size_t symNum = shSize/sizeof(Elf64Sym);
    return symNum;
}


size_t elf64GetSSymAddr(const Elf64Sym *sym)
{
    if (sym == NULL) {
        ERROR("Invalid arguments");
        return (size_t)-1;
    }

    return sym->st_value;
}


size_t elf64GetAddrSymByName(const Elf64File *elf64, const char *name)
{
    if (elf64FullCheckFile(elf64) || name == NULL) {
        ERROR("Invalid arguments");
        return (size_t)-1;
    }

    // TODO: work only for static symbols, for dynamic need signal
    return elf64GetSymByName(elf64, name)->st_value;
}


size_t elf64GetSSymSize(const Elf64File *elf64, const Elf64Sym *sym)
{
    if (elf64 == NULL || sym == NULL) {
        ERROR("Invalid arguments");
        return (size_t)-1;
    }

    return sym->st_size;
}


size_t elf64GetSSymFileoff(const Elf64File *elf64, const Elf64Sym *sym)
{
    if (elf64FullCheckFile(elf64) || sym == NULL) {
        ERROR("Invalid arguments");
        return (size_t)-1;
    }

    size_t shndx = sym->st_shndx;
    Elf64Shdr *sect = &elf64->sectionHeaders[shndx];
    size_t diff = sect->sh_addr - sect->sh_offset;
    return sym->st_value - diff;
}


size_t elf64GetDSymIndex(const Elf64File *elf64, const char *name)
{
    if(elf64FullCheckFile(elf64) || name == NULL) {
        ERROR("Invalid arguments");
        return (size_t)-1;
    }

    /*
     * SHT_DYNSYM - Contains a dynamic loader symbol table.
     */
    Elf64Shdr *dynsymTable = elf64GetSectionByType(elf64, SHT_DYNSYM);
    if(dynsymTable == NULL) {
        STDERROR_PRINT_DEBUG("Cannot get section SHT_DYNSYM.");
        return (size_t)-1;
    }

    char *dynsymNameTable = elf64->dynamicSymbolNameTable;
    size_t shSize = dynsymTable->sh_size;
    size_t symNum = shSize/sizeof(Elf64Sym);

    size_t i = 0;
    for(i = 0; i < symNum; ++i) {
        size_t tableOff = elf64->symbols->dynsym[i].st_name;
        char *symbolName = (char*)((size_t)dynsymNameTable + tableOff);
        if(!strcmp(symbolName, name))
            return i;
    }

    return (size_t)-1;
}


size_t elf64GetAmountSegment(const Elf64File *elf64)
{
    if (elf64FullCheckFile(elf64)) {
        STDERROR_PRINT_DEBUG("Invalid argument.");
        return (size_t)-1;
    }

    return elf64->elf64Header->e_phnum;
}


Elf64Shdr *elf64GetSectionByType(const Elf64File *elf64, const Elf64Word shType)
{
    if(elf64 == NULL || elf64->elf64Header == NULL
                   || elf64->sectionHeaders == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    /*
     * e_shnum - contains the number of entries in the section
     *           header table.
     * sh_type - identifies the section type.
     */
    size_t i = 0;
    for(i = 0; i < elf64->elf64Header->e_shnum; ++i)
        if(elf64->sectionHeaders[i].sh_type == shType)
            return (Elf64Shdr*) &elf64->sectionHeaders[i];

    return NULL;
}


Elf64Shdr *elf64GetSectionByName(const Elf64File *elf64, const char* name)
{
    if(elf64 == NULL || elf64->elf64Header == NULL
                   || name == NULL || elf64->sectionHeaders == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    char *sectionNameTable = elf64ParseSectionNameTable(elf64);
    /*
     * e_shnum - contains the number of entries in the section
     *           header table.
     * sh_name - contains the offset, in bytes, to the section name,
     *           relative to the start of the section name string table.
     */
    size_t i = 0;
    size_t shnum = elf64->elf64Header->e_shnum;
    for(i = 0; i < shnum; ++i) {
        size_t tableOff = elf64->sectionHeaders[i].sh_name;
        char *sectionName = (char*)((size_t)sectionNameTable + tableOff);
        if(!strcmp(sectionName, name)) {
            Free(sectionNameTable);
            return (Elf64Shdr*) &elf64->sectionHeaders[i];
        }
    }

    Free(sectionNameTable);
    return NULL;
}


Elf64Shdr *elf64GetLastLoadableSection(const Elf64File *elf64)
{
    if (elf64FullCheckFile(elf64)) {
        STDERROR_PRINT_DEBUG("Invalid argument.");
        return NULL;
    }

    size_t i = elf64GetAmountSection(elf64) - 1;
    if (i == (size_t)-1) {
        STDERROR_PRINT_DEBUG("Can not get section amount.");
        return NULL;
    }
    for (;i != (size_t)-1; --i)
        if (elf64->sectionHeaders[i].sh_addr)
            return &elf64->sectionHeaders[i];

    STDERROR_PRINT_DEBUG("There are no loadable sections.");
    return NULL;
}


void *elf64ReadSection(const Elf64File *elf64, const Elf64Shdr *sectionHeader)
{
    if (elf64 == NULL || elf64->fd < 0 || sectionHeader == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    /*
     * sh_offset -  contains the offset, in bytes, of the beginning
     *              of the section contents in the file.
     * sh_size   -  contains the size, in bytes, of the section.
     */
    int fd = elf64->fd;
    size_t shSize = sectionHeader->sh_size;
    size_t shOffset = sectionHeader->sh_offset;

    void *section = readFromFile(fd, &shOffset, shSize);
    if (section == NULL) {
        STDERROR_PRINT_DEBUG("Cannot read from file.");
        return NULL;
    }

    return section;
}


size_t elf64GetAmountSection(const Elf64File *elf64)
{
    if (elf64FullCheckFile(elf64)) {
        STDERROR_PRINT_DEBUG("Invalid argument.");
        return (size_t)-1;
    }

    return elf64->elf64Header->e_shnum;
}


const char* elf64GetSectionName(const Elf64File *elf64, const Elf64Shdr *sect)
{
    if (elf64FullCheckFile(elf64) || sect == NULL) {
        STDERROR_PRINT_DEBUG("Invalid argument.");
        return NULL;
    }

    size_t nameIndx = sect->sh_name;
    return &elf64->sectionNameTable[nameIndx];
}


size_t elf64GetSectionSize(const Elf64Shdr *elf64Sect)
{
    if (elf64Sect == NULL) {
        ERROR("Invalid arguments");
        return (size_t)-1;
    }

    return elf64Sect->sh_size;
}


size_t elf64GetSectionVaddr(const Elf64Shdr *elf64Sect)
{
    if (elf64Sect == NULL) {
        ERROR("Invalid arguments");
        return (size_t)-1;
    }

    return elf64Sect->sh_addr;
}


size_t elf64GetSectionFileoff(const Elf64Shdr *elf64Sect)
{
    if (elf64Sect == NULL) {
        ERROR("Invalid arguments");
        return (size_t)-1;
    }

    return elf64Sect->sh_offset;
}


size_t elf64GetRelocationForAddr(const Elf64File *elf64, const Elf64Shdr *sect, size_t addr)
{
    if(elf64FullCheckFile(elf64) || sect == NULL || addr == (size_t)-1) {
        ERROR("Invalid arguments");
        return (size_t)-1;
    }

    const char *sectName = elf64GetSectionName(elf64, sect);
    if (sectName == NULL) {
        STDERROR_PRINT_DEBUG("getSectionName().");
        return (size_t)-1;
    }

    size_t size = strlen(".rela");
    size += strlen(sectName);
    char *relaSectName = (char*)Malloc(size);
    if (relaSectName == NULL) {
        STDERROR_PRINT_DEBUG("Cannot alloc %zu bytes.", size);
        return (size_t)-1;
    }

    char *point = copyString(".rela", relaSectName);
    point = copyString(sectName, point);

    Elf64Shdr *relaSect = elf64GetSectionByName(elf64, relaSectName);
    if (relaSect == NULL) {
        STDERROR_PRINT_DEBUG("Cannot get the section %s.", relaSectName);
        return (size_t)-1;
    }

    Elf64Rel *rela = elf64ReadSection(elf64, relaSect);
    if (rela == NULL) {
        STDERROR_PRINT_DEBUG("elf64ReadSection()");
        return (size_t)-1;
    }

    size_t relaAmount = relaSect->sh_size / sizeof(Elf64Rel);
    size_t i = 0;
    for (i = 0; i < relaAmount; ++i) {
        /*
         * r_offset indicates the location at which the relocation should be
         * applied. For a relocatable file, this is the offset, in bytes, from
         * the beginning of the section to the beginning of the storage unit
         * being relocated. For an executable or shared object, this is the
         * virtual address of the storage unit being relocated.
         */
        if (IS_ELF64_FILE_REL(elf64) && rela[i].r_offset + sect->sh_addr == addr) {
            //PRINTF("find addr %zx\n", addr);
            break;
        }
        if (IS_ELF64_FILE_EXEC(elf64) && rela[i].r_offset == addr) {
            //PRINTF("find addr %zx\n", addr);
            break;
        }
    }

    if (i == relaAmount) {
        STDERROR_PRINT_DEBUG("There is no relocation info");
        return (size_t)-1;
    }

    if (ELF64_R_TYPE(rela[i].r_info) == R_X86_64_64) {
        return (size_t)rela[i].r_addend;
    } else {
        STDERROR_PRINT_DEBUG("Unknown relocation type.");
        return (size_t)-1;
    }

    return 0;
}


void *elf64Hook(const Elf64File *elf64, const char *func, const void *hand)
{
    if(elf64FullCheckFile(elf64) || func == NULL || hand == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    /*
     * symbolIndex -   index of target symbol in .dynsym section.
     */
    size_t symbolIndex = elf64GetDSymIndex(elf64, func);
    if (symbolIndex == (size_t)-1) {
        STDERROR_PRINT_DEBUG("Cannot get an index of a dynamic symbol %s.", func);
        return NULL;
    }

    /*
     * shSize      -   contains the size, in bytes, of the section.
     * relpltAmount-   amount of Elf64Rel structures in .rela.ptl section.
     */
    Elf64Shdr *relplt = elf64GetSectionByName(elf64, REL_PLT);
    if (relplt == NULL) {
        STDERROR_PRINT_DEBUG("Cannot get the section " REL_PLT ".");
        return NULL;
    }

    size_t relpltAmount = relplt->sh_size / sizeof(Elf64Rel);

    /*
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
    size_t i = 0;
    for(i = 0; i < relpltAmount; ++i)
        if(ELF64_R_SYM(elf64->relTables->relaplt[i].r_info) == symbolIndex){
            relAddr = (void*) *(size_t*) elf64->relTables->relaplt[i].r_offset;
            *(size_t*) (elf64->relTables->relaplt[i].r_offset) = (size_t) hand;

            return relAddr;
        }

    return NULL;
}


void *elf64GetRelocationDataAddr(const Elf64File *elf64, const char *func)
{
    if(elf64FullCheckFile(elf64) || func == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    /*
    * symbolIndex  -   index of target symbol in .dynsym section.
    */
    size_t symbolIndex = elf64GetDSymIndex(elf64, func);
    if (symbolIndex == (size_t)-1) {
        STDERROR_PRINT_DEBUG("Cannot get an index of a dynamic symbol %s.", func);
        return NULL;
    }

    /*
     * shSize       -   contains the size, in bytes, of the section.
     * relpltAmount -   amount of Elf64Rel structures in .rela.ptl section.
     */
    Elf64Shdr *relplt = elf64GetSectionByName(elf64, REL_PLT);
    if (relplt == NULL) {
        STDERROR_PRINT_DEBUG("Cannot get the section " REL_PLT ".");
        return NULL;
    }

    size_t relpltAmount = relplt->sh_size / sizeof(Elf64Rel);

    /*
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
    size_t i = 0;
    for(i = 0; i < relpltAmount; ++i)
        if(ELF64_R_SYM(elf64->relTables->relaplt[i].r_info) == symbolIndex)
            return (void*) elf64->relTables->relaplt[i].r_offset;

    return NULL;
}


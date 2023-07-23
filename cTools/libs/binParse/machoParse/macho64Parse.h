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

#ifndef __MACHO64_PARSE_H
#define __MACHO64_PARSE_H

#include "arch.h"
#include "file.h"
#include "macho64.h"

/* C standard headers */
#include <stddef.h>
#include <stdint.h>
#include <assert.h>

/*
 * If symbol is defined in specific section, it's static symbol.
 */
#define IS_MACHO64_SYM_STATIC(sym)     ((sym.n_type & N_SECT) == N_SECT)

/*
 * If symbol is defined in __text section and we could to refer to it
 * from other files, it is function.
 */
#define IS_MACHO64_SYM_FUNC(sym)       (sym.n_type == (N_SECT | N_EXT) &&\
                                        sym.n_sect == 1)

/*
 * If symbol is defined in section (not __Text,__text) and we
 * could to refer to it from other files, it's a global data.
 */
#define IS_MACHO64_SYM_GDATA(sym)      (sym.n_type == (N_SECT | N_EXT) &&\
                                        sym.n_sect != 1)

/*
 * If this symbol is defined in special section and we couldn't to refer
 * to it from other files, it is label or static function in mach-o.
 */
#define IS_MACHO64_SYM_LABEL(sym)      (sym.n_type == N_SECT)

/*
 * Symbol could be just UNDEFINED(N_UNDF) or external (N_EXT | N_UNDF). In both
 * cases symbol is undefined in this file and has no specific section.
 */
#define IS_MACHO64_UNDEF_SYM(sym)                                      \
    ((sym.n_type == (N_EXT | N_UNDF) || sym.n_type == N_UNDF) &&    \
        sym.n_sect == NO_SECT)

/*
 * Defines for checking type of binary files
 */
#define IS_MACHO64_FILE_OBJ(mf)        (mf->type == MH_OBJECT)
#define IS_MACHO64_FILE_EXEC(mf)       (mf->type == MH_EXECUTE)

#define MACHO64_SYM_PREF    "_"
#define MACHO64_SECT_PREF   "__"

typedef enum {  PAGEZERO_NSEG = 0,
                TEXT_NSEG,
                DATA_CONST_NSEG,
                DATA_NSEG,
                OBJC_NSEG,
                IMPORT_NSEG,
                LINKEDIT_NSEG,

                /* Into the object file there is only one segment. */
                UNNAMED_NSEG,

                MAX_NSEG } nseg; /* number of a segment */

typedef struct Macho64File {
    char              *fn;
    FileD             fd;
    uint64_t          hOff; // file offset to mach-o header
    uint32_t          type;
    Arch              arch;
    Macho64Header     *header;
    uint8_t           *lcom;
    SymtabCommand     *symtabCmd;
    DysymtabCommand   *dynsymCmd;
    Macho64Sym        *symtab;
    Macho64Sym        *sortSymtab;
    uint32_t          *indirectSymtab;
    char              *symNameTab;
    Macho64Seg        *segments[MAX_NSEG];
    MachoLinkEditData *funcStarts;
    uint32_t          numDyLibCom;
    MachoDylibCommand **dylibCom;
    MachoLinkEditData *sign;
} Macho64File;

#define FOREACH_LOAD_COMMAND(mf, code)                      \
    DEF_GUARD (                                             \
        uint32_t i = 0;                                     \
        uint32_t cmdsize = 0;                               \
        uint32_t ncmds = mf->header->ncmds;                 \
        LoadCommand *lcom = (LoadCommand*)(void*)mf->lcom;  \
        for (i = 0; i < ncmds; ++i) {                       \
            lcom = (LoadCommand*) ((size_t)lcom + cmdsize); \
            cmdsize = lcom->cmdsize;                        \
            code;                                           \
        }                                                   \
    )

#ifdef __WIN__
typedef enum : uint64_t {
#else
typedef enum {
#endif /* __WIN__ */
    MACHO64_NO_ERROR = (uint64_t)-20,
    MACHO64_NO_RELOCATION,
    MACHO64_NO_FILE_TYPE,
    MACHO64_NO_SECTION,
    MACHO64_NO_SYMBOL,
    MACHO64_NO_DYN_SYM_NAME_TAB,
    MACHO64_NO_SH_NAME_TAB,
    MACHO64_NO_SEGMENTS,
    MACHO64_NO_SECTIONS,
    MACHO64_NO_INDIRECT_SYM_TAB,
    MACHO64_NO_DYSYMTAB_CMD,
    MACHO64_NO_SYM_NAME_TAB,
    MACHO64_NO_SORT_SYMTAB,
    MACHO64_NO_SYMTAB,
    MACHO64_NO_SYMTAB_CMD,
    MACHO64_NO_LOAD_COMMAND,
    MACHO64_NO_HEADER,
    MACHO64_NO_FAT_HEADER,
    MACHO64_NO_MEM,
    MACHO64_INV_ARG,
    MACHO64_OK = 0
} MACHO64_ERROR;

static_assert(sizeof(MACHO64_ERROR) == 8, "MACHO64_ERROR must be 64 bit");
static_assert(((int64_t)MACHO64_INV_ARG) < 0, "ERRORS must be negative");

#define IS_MACHO64_ERROR(val) \
    (val <= MACHO64_INV_ARG && val > MACHO64_NO_ERROR)

/***
 * Description:
 *  Function parses binary mach-o file and initializes a Macho64File structure
 *  from file @fn
 * Input:
 *  @fn  - name of file what you want to parse
 * Output:
 *  Success:
 *      point to initialized Macho64File structure
 *  Fail:
 *      NULL point
 * After:
 *  Need to call macho64Free
 */
EXPORT_FUNC
Macho64File *macho64Parse(const char *fn);
/***
 * !TODO: rework, currently should be internal use only, but also need for
 * fatMacho64Parse
 */
EXPORT_FUNC
MACHO64_ERROR _macho64Parse(Macho64File *mf, uint64_t off);

/***
 * Before:
 *  You must completed all jobs with this Macho64File, otherwise you will free
 *  all information about this file including sections, symbols etc
 * Description:
 *  Free memory of Macho64File structure @mf
 * Input:
 *  @mf - point to Macho64File structer, that is necessary to free
 * After:
 *  @mf should be assigned to = NULL
 */
EXPORT_FUNC
void macho64Free(Macho64File *mf);

/***
 * Before:
 *  You must completed all jobs with this Macho64File, otherwise you will free
 *  all information about this file including sections, symbols etc
 * Description:
 *  Clean all internal structure of @mf, but pointer will be valid.
 * Input:
 *  @mf - point to Macho64File structure should be cleaned
 * After:
 *  @mf should be freed after
 */
EXPORT_FUNC
void macho64Clean(Macho64File *mf);

/***
 * Description:
 *  Fully check Macho64File structure
 * Input:
 *  @mf - Macho64File structure, that is nedded to check.
 * Output:
 *  Success:
 *      MACHO64_OK
 *  Fail:
 *      MACHO64_INV_ARG, MACHO64_NO_HEADER, MACHO64_NO_LOAD_COMMAND,
 *      MACHO64_NO_SYMTAB_CMD, MACHO64_NO_SYMTAB, MACHO64_NO_SORT_SYMTAB,
 *      MACHO64_NO_SYM_NAME_TAB, MACHO64_NO_INDIRECT_SYM_TAB, MACHO64_NO_SEGMENTS
 */
EXPORT_FUNC
MACHO64_ERROR macho64Check(const Macho64File *mf);

/***
 * Description:
 *  Function prints an indirect symbol table from @mf with information
 * Input:
 *  @mf - point to target Macho64File
 * Output:
 *  Success:
 *      MACHO64_OK
 *  Fail:
 *      MACHO64_INV_ARG
 */
EXPORT_FUNC
MACHO64_ERROR macho64PrintIndirectSymTab(const Macho64File *mf);

/***
 * Description:
 *  Function returns a point to the mach-o symbol with target @indx
 * Input:
 *  @mf - point to the targer structure Macho64File
 *  @indx - index of target symbol
 * Output:
 *  Success:
 *      point to symbols(maybe dynamic) from Macho64File structure
 *  Fail:
 *      NULL point
 */
EXPORT_FUNC
Macho64Sym *macho64GetSymByIndx(const Macho64File *mf, uint64_t indx);

/***
 * Description:
 *  Function returns a point to the mach-o symbol with target @name
 * Input:
 *  @mf - point to the targer structure Macho64File
 *  @name - name of target symbol
 * Output:
 *  Success:
 *      point to symbols(maybe dynamic) from Macho64File structure
 *  Fail:
 *      NULL point
 */
EXPORT_FUNC
Macho64Sym *macho64GetSymByName(const Macho64File *mf, const char *name);

/***
 * Description:
 *  Function returns a point to the mach-o symbol with target @addr
 * Input:
 *  @mf - point to the targer structure Macho64File
 *  @addr - address of target symbol
 * Output:
 *  Success:
 *      point to symbols(maybe dynamic) from Macho64File structure
 *  Fail:
 *      NULL point
 */
EXPORT_FUNC
Macho64Sym *macho64GetSSymByAddr(const Macho64File *mf, uint64_t addr);

/***
 * Description:
 *  Function returns a name of a symbol @ms
 * Input:
 *  @mf - point to the targer structure Macho64File
 *  @ms - point to symbol structure Macho64Sym
 * Output:
 *  Success:
 *      point to name of static symbol
 *  Fail:
 *      NULL point
 */
EXPORT_FUNC
char *macho64GetSymName(const Macho64File *mf, const Macho64Sym *ms);

/***
 * Description:
 *  Function returns an index of a mach-o symbol with @name in a symbols table
 * Input:
 *  @mf - point to the targer Macho64File
 *  @name - name of the symbol
 * Output:
 *  Success:
 *      index of the symbol with @name
 *  Fail:
 *      -1
 */
EXPORT_FUNC
uint64_t macho64GetSymIndxByName(const Macho64File *mf, const char *name);

// !TODO
EXPORT_FUNC
uint64_t macho64GetDSymIndx(const Macho64File *mf, const char *name);

/***
 * Description:
 *  Function for work with qsort. Function compares addresses of symbols and
 * Output:
 *  1 - if (a->addr > b->addr)
 *  -1 - if (a->addr < b->addr)
 *  0 - if (a->addr == b->addr)
 */
EXPORT_FUNC
int macho64CmpSym(const void *a, const void *b);

/***
 * Description:
 *  Function returns point to table of static symbols from Macho64File structure
 * Input:
 *  @mf - point to the targer structure Macho64File
 * Output:
 *  Success:
 *      point to table of static symbols from Macho64File structure
 *  Fail:
 *      NULL point
 */
EXPORT_FUNC
Macho64Sym *macho64GetSSymTab(const Macho64File *mf);
EXPORT_FUNC
Macho64Sym *macho64GetSSymSortTab(const Macho64File *mf);

/***
 * Description:
 *  Function offset from start of binary file @mf to a symtab
 * Input:
 *  @mf - point to the targer structure Macho64File
 * Output:
 *  Success:
 *      Offset from start of nimary file to static symbols table
 *  Fail:
 *      -1
 */
EXPORT_FUNC
uint64_t macho64GetSSymTabFileoff(const Macho64File *mf);

/***
 * Description:
 *  Function returns an amount of static symbols from mach-o binary file
 * Input:
 *  @mf - point to the targer Macho64File
 * Output:
 *  Success:
 *      amount of static symbols from the Macho64File structure
 *  Fail:
 *      -1
 */
EXPORT_FUNC
uint64_t macho64GetAmountSSym(const Macho64File *mf);

/***
 * Description:
 *  Function returns address of a mach-o static symbol
 * Input:
 *  @ms - point to the targer structure Macho64Sym
 * Output:
 *  Success:
 *      addr - of the static symbol ms
 *      0 - for dynamic symbol
 *  Fail:
 *      -1
 */
EXPORT_FUNC
uint64_t macho64GetSSymAddr(const Macho64Sym *ms);

// TODO
EXPORT_FUNC
uint64_t macho64GetAddrSymByName(const Macho64File *mf, const char *name);

/***
 * Description:
 *  Function sets new @addr for symbol @ms
 * Input:
 *  @ms - point to the targer structure Macho64Sym
 *  @addr - new addr for symbol
 * Output:
 *  Always success
 */
EXPORT_FUNC
void macho64SetSSymAddr(Macho64Sym *ms, uint64_t addr);

/***
 * Description:
 *  Functions return size of a mach-o symbol. If you don't know a type of
 *  symbol, you should call macho64GetSSymSize. Otherwise, macho64GetFuncSize for
 *  function and macho64GetGDataSize for global data
 * Input:
 *  @mf - point to the targer Macho64File
 *  @ms - point to the targer structure Macho64Sym
 * Output:
 *  Success:
 *      size - for static symbol
 *  Fail:
 *      MACHO64_INV_ARG, MACHO64_NO_SYMBOL
 */
EXPORT_FUNC
uint64_t macho64GetFuncSize(const Macho64File *mf, const Macho64Sym *ms);
EXPORT_FUNC
uint64_t macho64GetGDataSize(const Macho64File *mf, const Macho64Sym *ms);
EXPORT_FUNC
uint64_t macho64GetSSymSize(const Macho64File *mf, const Macho64Sym *ms);

/***
 * Description:
 *  Function returns file position for the static symbol @sym
 * Input:
 *  @mf - point to the targer Macho64File
 *  @sym - point to symbol structure
 * Output:
 *  Success:
 *      file position of the static symbol
 *  Fail:
 *      MACHO64_INV_ARG, MACHO64_NO_FILE_TYPE
 */
EXPORT_FUNC
uint64_t macho64GetSSymFileoff(const Macho64File *mf, const Macho64Sym *sym);

/***
 * Description:
 *  Function returns an amount of segments in the binary file
 * Input:
 *  @mf - point to the targer Macho64File
 * Output:
 *  Success:
 *      amount of segments in the binary file
 *  Fail:
 *      MACHO64_INV_ARG
 */
EXPORT_FUNC
uint64_t macho64GetAmountSeg(const Macho64File *mf);

/***
 * Description:
 *  Function returns a data segment descriptor from the binary file
 * Input:
 *  @mf - point to the targer Macho64File
 * Output:
 *  Success:
 *      pointer to the data segment descriptor
 *  Fail:
 *      NULL
 */
EXPORT_FUNC
Macho64Seg *macho64GetDataSeg(const Macho64File *mf);

/***
 * Description:
 *  Function returns a descriptor of the last segment from the binary file
 * Input:
 *  @mf - point to the targer Macho64File
 * Output:
 *  Success:
 *      pointer to a descriptor of the last segment
 *  Fail:
 *      NULL
 */
EXPORT_FUNC
Macho64Seg *macho64GetLastSeg(const Macho64File *mf);

/***
 * Description:
 *  Function returns a size of a segment
 * Input:
 *  @seg - pointer to a segment descriptor
 * Output:
 *  Success:
 *      size of a segment
 *  Fail:
 *      MACHO64_INV_ARG
 */
EXPORT_FUNC
uint64_t macho64GetSegSize(const Macho64Seg *seg);

/***
 * Description:
 *  Function returns a virtual address of a segment
 * Input:
 *  @seg - pointer to a segment descriptor
 * Output:
 *  Success:
 *      virtual address size of a segment
 *  Fail:
 *      MACHO64_INV_ARG
 */
EXPORT_FUNC
uint64_t macho64GetSegAddr(const Macho64Seg *seg);

/***
 * Description:
 *  Function returns a file possition of a segment
 * Input:
 *  @seg - pointer to a segment descriptor
 * Output:
 *  Success:
 *      virtual address size of a segment
 *  Fail:
 *      MACHO64_INV_ARG
 */
EXPORT_FUNC
uint64_t macho64GetSegFileoff(const Macho64Seg *seg);

/***
 * Description:
 *  Function returns point to the mach-o section with target @name
 * Input:
 *  @mf - point to the targer structure Macho64File
 *  @name - name of target section
 * Output:
 *  Success:
 *      point to Macho64Sect structure from Macho64File structure
 *  Fail:
 *      NULL point
 */
EXPORT_FUNC
Macho64Sect *macho64GetSectByName(const Macho64File *mf, const char *name);

/***
 * Description:
 *  Function returns point to the mach-o section contains target @addr
 * Input:
 *  @mf - point to the targer structure Macho64File
 *  @addr - addr in the target section
 * Output:
 *  Success:
 *      point to Macho64Sect structure from Macho64File structure
 *  Fail:
 *      NULL point
 */
EXPORT_FUNC
Macho64Sect *macho64GetSectByAddr(const Macho64File *mf, uint64_t addr);

/***
 * Description:
 *  Function returns point to the mach-o section with target @indx
 * Input:
 *  @mf - point to the targer structure Macho64File
 *  @indx - indx of target section
 * Output:
 *  Success:
 *      point to Macho64Sect structure from Macho64File structure
 *  Fail:
 *      NULL point
 */
EXPORT_FUNC
Macho64Sect *macho64GetSectByIndx(const Macho64File *mf, uint64_t indx);

/***
 * Description:
 *  Function returns a descriptor of the last section from a segment @seg
 * Input:
 *  @seg - segment descriptor
 * Output:
 *  Success:
 *      point to descriptor of the last section from bin_file structer
 *  Fail:
 *      NULL point
 */
EXPORT_FUNC
Macho64Sect *macho64GetAllSect(const Macho64Seg *seg);
EXPORT_FUNC
Macho64Sect *macho64GetLastSect(const Macho64Seg *seg);
EXPORT_FUNC
Macho64Sect *macho64GetLastLoadableSect(const Macho64File *mf);

/***
 * Description:
 *  Function returns pointer to a read from mach-o file section
 * Input:
 *  @mf - Macho64File descriptor
 *  @sect - section descriptor
 * Output:
 *  Success:
 *      point to a read section
 *  Fail:
 *    NULL point
 */
EXPORT_FUNC
void *macho64ReadSect(const Macho64File *mf, const Macho64Sect *sect);

/***
 * Description:
 *  Function returns number of sections from mach-o file
 * Input:
 *  @mf - Macho64File descriptor
 * Output:
 *  Success:
 *      number os sections
 *  Fail:
 *      MACHO64_INV_ARG
 */
EXPORT_FUNC
uint64_t macho64GetAmountSect(const Macho64File *mf);

/***
 * Description:
 *  Function returns name of the section @sect
 * Input:
 *  @mf - point to the targer Macho64File
 *  @sect - point to section descriptor
 * Output:
 *  Success:
 *      pointer to a section name
 *  Fail:
 *      NULL
 */
EXPORT_FUNC
const char* macho64GetSectName(const Macho64File *mf, const Macho64Sect *sect);

/***
 * Description:
 *  Function returns a size of a section
 * Input:
 *  @sect - point to section descriptor
 * Output:
 *  Success:
 *      size of a section
 *  Fail:
 *      MACHO64_INV_ARG
 */
EXPORT_FUNC
uint64_t macho64GetSectSize(const Macho64Sect *sect);

/***
 * Description:
 *  Function returns a virtual address of a section
 * Input:
 *  @sect - point to section descriptor
 * Output:
 *  Success:
 *      virtual address of a section
 *  Fail:
 *      MACHO64_INV_ARG
 */
EXPORT_FUNC
uint64_t macho64GetSectAddr(const Macho64Sect *sect);

/***
 * Description:
 *  Function returns a file offset of a section
 * Input:
 *  @sect - point to section descriptor
 * Output:
 *  Success:
 *      file offset of a section
 *  Fail:
 *      MACHO64_INV_ARG
 */
EXPORT_FUNC
uint64_t macho64GetSectFileoff(const Macho64Sect *sect);

/***
 *  Description:
 *      Function returns position in a mach-o file of a relocation for section
 *  Input:
 *      @sect - pointer to a section
 *  Output:
 *      File position in a mach-o file of a relocation
 */
EXPORT_FUNC
uint64_t macho64GetSectRelocFileoff(const Macho64Sect *sect);

/***
 *  Description:
 *      Function returns number of relocations for section
 *  Input:
 *      @sect - pointer to a section
 *  Output:
 *      number of relocations
 */
EXPORT_FUNC
uint64_t macho64GetSectRelocNum(const Macho64Sect *sect);

EXPORT_FUNC
uint64_t macho64GetRelocAddr(const Macho64Sect *sect, const MachoRelocInfo *rel);


EXPORT_FUNC
void macho64ChangeRelocAddr(MachoRelocInfo *rel, int32_t diff);

/***
 * Description:
 *  Function returns a relocations number for data, that is located at @addr in
 *  section @ms
 * Input:
 *  @mf - binary file descriptor.
 *  @ms - point to section descriptor.
 *  @addr - address of relocatable data.
 * Output:
 *  Success:
 *      relocation offset.
 *  Fail:
 *      MACHO64_INV_ARG, MACHO64_NO_MEM, MACHO64_NO_RELOCATION
 */
EXPORT_FUNC
uint64_t macho64GetRelocForAddr(const Macho64File *mf, const Macho64Sect *sect, uint64_t addr);

/***
 * Description:
 *  Function returns an index of a dynamic mach-o symbol in symbols table
 * Input:
 *  @mf - point to the targer Macho64File
 *  @name - name of the symbol
 * Output:
 *  Success:
 *      index of the symbol with name
 *  Fail:
 *      MACHO64_INV_ARG, MACHO64_NO_SYMBOL
 */
EXPORT_FUNC
uint64_t macho64GetDSymIndxByName(const Macho64File *mf, const char *name);

/***
 *  Before:
 *      If you need a file position, you should to save it
 *  Input:
 *      @mf - mach-o descriptor
 *      @func - name of function, that is nedded to hooked
 *  Output:
 *      Success:
 *          Relocation addr for @func
 *      Fail:
 *          NULL
 */
EXPORT_FUNC
void *macho64GetRelocDataAddr(const Macho64File *mf, const char *func);

EXPORT_FUNC
const char *macho64GetDylibName(const MachoDylibCommand *dl);

EXPORT_FUNC
uint64_t macho64GetImportSymbolPosInSectByIndx( const Macho64File *mf
                                              , const Macho64Sect *importSect
                                              , uint64_t indx
                                              );
#endif /* __MACHO64_PARSE_H */


/***
 * MIT License
 *
 * Copyright (c) 2021-2024 Konychev Valerii
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
    size_t            fs;
    uint8_t           *faddr;
    uint64_t          base_addr;
    uint64_t          hOff; // file offset to mach-o header
    uint32_t          type;
    Arch              arch;
    Macho64Header     *header;
    uint8_t           *lcom;
    SymtabCommand     *symtabCmd;
    DysymtabCommand   *dynsymCmd;
    Macho64Sym        *symtab;
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
    MACHO64_NO_ERROR = (uint64_t)-22,
    MACHO64_NO_CODE_SIG,
    MACHO64_NO_DYLIB_COM,
    MACHO64_NO_FUNC_STARTS,
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
 * @brief Parse macho64 binary file
 *
 * @param[in] fn Binary file name. C string with terminal null
 *
 * @return Pointer to Macho64File structer or NULL if fail
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
 * @warning You must completed all jobs with this Macho64File, otherwise you
 *          will free all information about this file including sections, symbols
 *          etc
 *
 * @brief Free memory of Macho64File struct
 *
 * @param[in] mf pointer to Macho64File structer
 */
EXPORT_FUNC
void macho64Free(Macho64File *mf);

/***
 * @warning You must completed all jobs with this Macho64File, otherwise you
 *          will free all information about this file including sections, symbols
 *          etc
 *
 * @brief Clean Macho64File struct without free memory
 *
 * @param[in] mf pointer to Macho64File structer
 */
EXPORT_FUNC
void macho64Clean(Macho64File *mf);

/***
 * @brief Fully check Macho64File structure
 *
 * @param[in] mf Macho64File structure
 *
 * @return MACHO64_OK or
 *         MACHO64_INV_ARG, MACHO64_NO_HEADER, MACHO64_NO_LOAD_COMMAND,
 *         MACHO64_NO_SYMTAB_CMD, MACHO64_NO_SYMTAB, MACHO64_NO_SORT_SYMTAB,
 *         MACHO64_NO_SYM_NAME_TAB, MACHO64_NO_INDIRECT_SYM_TAB,
 *         MACHO64_NO_SEGMENTS
 */
EXPORT_FUNC
MACHO64_ERROR macho64Check(const Macho64File *mf);

/***
 * @brief prints an indirect symbol table from @mf with information
 *
 * @param[in] mf - point to target Macho64File
 *
 * @return MACHO64_OK or MACHO64_INV_ARG
 */
EXPORT_FUNC
MACHO64_ERROR macho64PrintIndirectSymTab(const Macho64File *mf);

/***
 * @brief returns a point to the mach-o symbol with indx
 *
 * @param[in] mf pointer to the targer structure Macho64File
 * @param[in] indx index of target symbol
 *
 * @return pointer to symbol with index or NULL
 */
EXPORT_FUNC
Macho64Sym *macho64GetSymByIndx(const Macho64File *mf, uint64_t indx);

/***
 * @brief Function returns pointer to the symbol with name
 *
 * @param[in] mf pointer to Macho64File structer
 * @param[in] name symbol name needed to find
 *
 * @return pointer to Macho64Sym or NULL if fail
 */
EXPORT_FUNC
EXPORT_FUNC
Macho64Sym *macho64GetSymByName(const Macho64File *mf, const char *name);

/***
 * @brief returns a point to the mach-o symbol with addr
 *
 * @param[in] mf  Macho64File pointer
 * @param[in] addr address of target symbol
 *
 * @return point to symbols or NULL
 */
EXPORT_FUNC
Macho64Sym *macho64GetSSymByAddr(const Macho64File *mf, uint64_t addr);

/***
 * @brief Function returns name of the symbol
 *
 * @param[in] mf pointer to the target Macho64File
 * @param[in] ms pointer to symbol structure
 *
 * @return pointer to name of symbol or NULL if fail
 */
EXPORT_FUNC
char *macho64GetSymName(const Macho64File *mf, const Macho64Sym *ms);

/***
 * @brief returns index of symbol with name
 *
 * @param[in] mf Macho64File pointer
 * @param[in] name symbol name
 *
 * @return index of the symbol or -1
 */
EXPORT_FUNC
uint64_t macho64GetSymIndxByName(const Macho64File *mf, const char *name);

/***
 * @brief Function for work with qsort. Functions compare addresses of symbols
 *        and returns 1/-1/0 if a->addr >/</== b->addr.
 *
 * @param[in] a pointer to a fist symbol
 * @param[in] b pointer to a second symbol
 */
EXPORT_FUNC
int macho64CmpSym(const void *a, const void *b);

/***
 * @brief Function returns pointer to the static symbols table
 *
 * @param[in] mf Macho64File pointer
 *
 * @return pointer to static symbol table or NULL if fail
 */
EXPORT_FUNC
Macho64Sym *macho64GetSSymTab(const Macho64File *mf);

/***
 * @brief returnd file offset to a symtab
 *
 * @param[in] mf Macho64File pointer
 *
 * @return file offset or -1
 */
EXPORT_FUNC
uint64_t macho64GetSSymTabFileoff(const Macho64File *mf);

/***
 * @brief Function returns amount of static symbols
 *
 * @param[in] mf pointer to the target Macho64File
 *
 * @return amount of static symbols or -1
 */
EXPORT_FUNC
uint64_t macho64GetAmountSSym(const Macho64File *mf);

/***
 * @brief returns addr of static symbol without ASLR
 *
 * @param[in] ms pointer to the target static Macho64Sym
 *
 * @return address of -1
 */
EXPORT_FUNC
uint64_t macho64GetSSymAddr(const Macho64Sym *ms);

/***
 * @brief Return addr of static symbol with name
 *
 * @param[in] mf Macho64File pointer
 * @param[in] name name of static symbol
 *
 * @return static symbol address of -1
 */
EXPORT_FUNC
uint64_t macho64GetAddrSymByName(const Macho64File *mf, const char *name);

/***
 * @brief sets new addr for symbol ms
 *
 * @param[in,out] ms Macho64Syn pointer
 * @param[in] addr new addr for symbol
 */
EXPORT_FUNC
void macho64SetSSymAddr(Macho64Sym *ms, uint64_t addr);

/***
 * @brief
 *  Functions return size of a mach-o symbol. If you don't know a type of
 *  symbol, you should call macho64GetSSymSize. Otherwise, macho64GetFuncSize for
 *  function and macho64GetGDataSize for global data
 *
 * @param[in] mf Macho64File pointer
 * @param[in] ms Macho64Sym pointer
 *
 * @return size for static symbol or MACHO64_INV_ARG, MACHO64_NO_SYMBOL
 */
EXPORT_FUNC
uint64_t macho64GetFuncSize(const Macho64File *mf, const Macho64Sym *ms);
EXPORT_FUNC
uint64_t macho64GetGDataSize(const Macho64File *mf, const Macho64Sym *ms);
EXPORT_FUNC
uint64_t macho64GetSSymSize(const Macho64File *mf, const Macho64Sym *ms);

/***
 * @brief returns file position for the static symbol
 *
 * @param[in] mf Macho64File pointer
 * @param[in] ms Macho64Sym pointer
 *
 * @return file position or MACHO64_INV_ARG, MACHO64_NO_FILE_TYPE
 */
EXPORT_FUNC
uint64_t macho64GetSSymFileoff(const Macho64File *mf, const Macho64Sym *sym);

/***
 * @brief returns an amount of segments
 *
 * @param[in] mf Macho64File pointer
 *
 * @return amount of segments
 */
EXPORT_FUNC
uint64_t macho64GetAmountSeg(const Macho64File *mf);

/***
 * @brief returns __DATA segment descriptor from the binary file
 *
 * @param[in] mf Macho64File pointer
 *
 * @return __DATA segment pointer or NULL
 */
EXPORT_FUNC
Macho64Seg *macho64GetDataSeg(const Macho64File *mf);

/***
 * @brief eturns a descriptor of the last segment from the binary file
 *
 * @param[in] mf Macho64File pointer
 *
 * @return pointer of the last segment descriptor or NULL
 */
EXPORT_FUNC
Macho64Seg *macho64GetLastSeg(const Macho64File *mf);

/***
 * @brief returns a size of a segment
 *
 * @param[in] seg pointer to a segment descriptor
 *
 * @return size of a segment or -1
 */
EXPORT_FUNC
uint64_t macho64GetSegSize(const Macho64Seg *seg);

/***
 * @brief eturns a virtual address of a segment
 *
 * @param[in] seg pointer to a segment descriptor
 *
 * @return virtual address size of a segment or -1
 */
EXPORT_FUNC
uint64_t macho64GetSegAddr(const Macho64Seg *seg);

/***
 * @brief returns a file possition of a segment
 *
 * @param[in] seg pointer to a segment descriptor
 *
 * @return virtual address size of a segment or -1
 */
EXPORT_FUNC
uint64_t macho64GetSegFileoff(const Macho64Seg *seg);

/***
 * @brief returns a descriptor of a section with name
 *
 * @param[in] mf pointer to the target Macho64File
 * @param[in] name section name
 *
 * @return pointer to a section of NULL
 */
EXPORT_FUNC
Macho64Sect *macho64GetSectByName(const Macho64File *mf, const char *name);

/***
 * @brief eturns point to the mach-o section contains target addr
 *
 * @param[in] mf Macho64File pointer
 * @param[in] addr addr in the target section
 *
 * @return pointer to section descriptor of NULL
 */
EXPORT_FUNC
Macho64Sect *macho64GetSectByAddr(const Macho64File *mf, uint64_t addr);

/***
 * @brief returns point to the mach-o section with target indx
 *
 * @param[in] mf Macho64File pointer
 * @param[in] indx indx of target section
 *
 * @return pointer to section descriptor of NULL
 */
EXPORT_FUNC
Macho64Sect *macho64GetSectByIndx(const Macho64File *mf, uint64_t indx);

/***
 * @brief returns sections on segment
 *
 * @param[in] seg segment descriptor
 *
 * @return pointer to a sections in segment or NULL
 */
EXPORT_FUNC
Macho64Sect *macho64GetAllSect(const Macho64Seg *seg);

/***
 * @brief returns a descriptor of the last section from a segment
 *
 * @param[in] seg segment descriptor
 *
 * @return pointer to a section descriptor or NULL
 */
EXPORT_FUNC
Macho64Sect *macho64GetLastSect(const Macho64Seg *seg);

/***
 * @brief returns last loadable section in binary file
 *
 * @param[in] mf pointer to the target Macho64File
 *
 * @return pointer to a section of NULL
 */
EXPORT_FUNC
Macho64Sect *macho64GetLastLoadableSect(const Macho64File *mf);

/***
 * @brief Reads section contents
 *
 * @param[in] mf pointer to the target Macho64Sect
 * @param[in] sect section descriptor
 *
 * @return pointer to a content or NULL
 * @warning need to free memory
 */
EXPORT_FUNC
void *macho64ReadSect(const Macho64File *mf, const Macho64Sect *sect);

/***
 * @brief returns an amount of sections
 *
 * @param[in] mf pointer to the target Macho64File
 *
 * @return amount of sections
 */
EXPORT_FUNC
uint64_t macho64GetAmountSect(const Macho64File *mf);

/***
 * @brief returns name of the section
 *
 * @param[in] mf pointer to the target Macho64File
 * @param[in] sect section descriptor
 *
 * @return pointer of NULL
 */
EXPORT_FUNC
const char* macho64GetSectName(const Macho64File *mf, const Macho64Sect *sect);

/***
 * @brief returns a size of a section
 *
 * @param[in] sect pointer to section descriptor
 *
 * @return section size or -1
 */
EXPORT_FUNC
uint64_t macho64GetSectSize(const Macho64Sect *sect);

/***
 * @brief returns a virtual address of a section start
 *
 * @param[in] sect pointer to section descriptor
 *
 * @return addr or -1
 */
EXPORT_FUNC
uint64_t macho64GetSectAddr(const Macho64Sect *sect);

/***
 * @brief returns a file offset of a section.
 *
 * @param[in] sect pointer to section descriptor.
 *
 * @return file offset to a section start
 */
EXPORT_FUNC
uint64_t macho64GetSectFileoff(const Macho64Sect *sect);

/***
 * @brief returns position in a mach-o file of a relocation for section
 *
 * @param[in] sect pointer to a section
 *
 * @return file position in a mach-o file of a relocation
 */
EXPORT_FUNC
uint64_t macho64GetSectRelocFileoff(const Macho64Sect *sect);

/***
 * @brief returns number of relocations for section
 *
 * @param[in] sect pointer to a section
 *
 * @return number of relocations
 */
EXPORT_FUNC
uint64_t macho64GetSectRelocNum(const Macho64Sect *sect);

EXPORT_FUNC
uint64_t macho64GetRelocAddr(const Macho64Sect *sect, const MachoRelocInfo *rel);

EXPORT_FUNC
void macho64ChangeRelocAddr(MachoRelocInfo *rel, int32_t diff);

/***
 * @brief
 *  Function returns a relocations number for data, that is located at @addr in
 *  section @ms
 *
 * @param[in] mf - binary file descriptor.
 *  @ms - point to section descriptor.
 *  @addr - address of relocatable data.
 *
 * @return
 *      relocation offset.
 *  Fail:
 *      MACHO64_INV_ARG, MACHO64_NO_MEM, MACHO64_NO_RELOCATION
 */
EXPORT_FUNC
uint64_t macho64GetRelocForAddr(const Macho64File *mf, const Macho64Sect *sect, uint64_t addr);

/***
 * @brief
 *  Function returns an index of a dynamic mach-o symbol in symbols table
 *
 * @param[in] mf - point to the targer Macho64File
 *  @name - name of the symbol
 *
 * @return
 *      index of the symbol with name
 *  Fail:
 *      MACHO64_INV_ARG, MACHO64_NO_SYMBOL
 */
EXPORT_FUNC
uint64_t macho64GetDSymIndxByName(const Macho64File *mf, const char *name);

/***
 *  Before:
 *      If you need a file position, you should to save it
 *
 * @param[in] mf - mach-o descriptor
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


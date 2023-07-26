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

#ifndef __FAT_MACHO64_PARSE_H
#define __FAT_MACHO64_PARSE_H

#include "os.h"
#include "arch.h"
#include "comdef.h"
#include "macho64Parse.h"

typedef struct FatMacho64File {
    char        *fn;
    FileD       fd;
    FatHeader   fatHead;
    // can be fat_arch or fat_arch_64, depends on fatHead.magic
    // only one will be not NULL
    FatArch     *fatArch;
    FatArch64   *fatArch64;
    Macho64File *mf;
} FatMacho64File;

#ifdef FAT_MACHO64_PARSE_SHARED_LIB
EXPORT_VAR
#else /* FAT_MACHO64_PARSE_SHARED_LIB */
# ifndef STATIC_LIB
IMPORT_VAR
# endif /* STATIC_LIB*/
#endif /* FAT_MACHO64_PARSE_SHARED_LIB */
extern Arch fatMacho64ParseArch;

/***
 * Description:
 *  Function parses binary mach-o fat file and initializes a FatMacho64File
 *  structure from file @fn
 * Input:
 *  @fn  - name of file what you want to parse
 * Output:
 *  Success:
 *      point to initialized FatMacho64File structure
 *  Fail:
 *      NULL point
 * After:
 *  Need to call fatMacho64Free
 */
EXPORT_FUNC
FatMacho64File *fatMacho64Parse(const char *fn);

/***
 * Before:
 *  You must completed all jobs with this FatMacho64File, otherwise you will
 *  free all information about this file including sections, symbols etc
 * Description:
 *  Free memory from FatMacho64File structure @ff
 * Input:
 *  @ff - point to FatMacho64File structer, that is necessary to free
 * After:
 *  @ff should be assigned to = NULL
 */
EXPORT_FUNC
void fatMacho64Free(FatMacho64File *ff);

/***
 * Description:
 *  Fully check Macho64File structure
 * Input:
 *  @ff - Macho64File structure, that is nedded to check.
 * Output:
 *  Success:
 *      MACHO64_OK
 *  Fail:
 *      MACHO64_INV_ARG, MACHO64_NO_HEADER, MACHO64_NO_LOAD_COMMAND,
 *      MACHO64_NO_SYMTAB_CMD, MACHO64_NO_SYMTAB, MACHO64_NO_SORT_SYMTAB,
 *      MACHO64_NO_SYM_NAME_TAB, MACHO64_NO_INDIRECT_SYM_TAB, MACHO64_NO_SEGMENTS
 *      MACHO64_NO_FAT_HEADER
 */
EXPORT_FUNC
MACHO64_ERROR fatMacho64Check(const FatMacho64File *ff);

EXPORT_FUNC
Macho64File* fatMacho64GetMacho64ByArch(FatMacho64File *ff, Arch arch);

EXPORT_FUNC
const Macho64File* fatMacho64GetConstMacho64ByArch(const FatMacho64File *ff, Arch arch);

EXPORT_FUNC
Macho64Sym *fatMacho64GetSymByName(const FatMacho64File *ff, const char *name);

EXPORT_FUNC
char *fatMacho64GetSymName(const FatMacho64File *ff, const Macho64Sym *ms);

static INLINE
int fatMacho64CmpSym(const void *a, const void *b)
{
    return macho64CmpSym(a, b);
}

EXPORT_FUNC
Macho64Sym *fatMacho64GetSSymTab(const FatMacho64File *ff);

EXPORT_FUNC
Macho64Sym *fatMacho64GetSSymSortTab(const FatMacho64File *ff);

EXPORT_FUNC
uint64_t fatMacho64GetAmountSSym(const FatMacho64File *ff);

static INLINE
uint64_t fatMacho64GetSSymAddr(const Macho64Sym *ms)
{
    return macho64GetSSymAddr(ms);
}

EXPORT_FUNC
uint64_t fatMacho64GetAddrSymByName(const FatMacho64File *ff, const char *name);

EXPORT_FUNC
uint64_t fatMacho64GetSSymSize(const FatMacho64File *ff, const Macho64Sym *ms);

EXPORT_FUNC
uint64_t fatMacho64GetSSymFileoff(const FatMacho64File *ff, const Macho64Sym *sym);

EXPORT_FUNC
uint64_t fatMacho64GetAmountSeg(const FatMacho64File *ff);

EXPORT_FUNC
Macho64Sect *fatMacho64GetSectByName(const FatMacho64File *ff, const char *name);

EXPORT_FUNC
Macho64Sect *fatMacho64GetLastLoadableSect(const FatMacho64File *ff);

EXPORT_FUNC
void *fatMacho64ReadSect(const FatMacho64File *ff, const Macho64Sect *sect);

EXPORT_FUNC
uint64_t fatMacho64GetAmountSect(const FatMacho64File *ff);

EXPORT_FUNC
const char* fatMacho64GetSectName(const FatMacho64File *ff, const Macho64Sect *sect);

static INLINE
uint64_t fatMacho64GetSectSize(const Macho64Sect *sect)
{
    return macho64GetSectSize(sect);
}

static INLINE
uint64_t fatMacho64GetSectAddr(const Macho64Sect *sect)
{
    return macho64GetSectAddr(sect);
}

static INLINE
uint64_t fatMacho64GetSectFileoff(const Macho64Sect *sect)
{
    return macho64GetSectFileoff(sect);
}

#endif /* __FAT_MACHO64_PARSE_H */

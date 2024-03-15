/***
 * MIT License
 *
 * Copyright (c) 2023-2024 Konychev Valerii
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
    size_t      fs;
    uint8_t     *faddr;
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
 * @brief parse binary mach-o fat file
 *
 * @param[in] fn name of file what you want to parse
 *
 * @return point to initialized FatMacho64File structure or NULL
 */
EXPORT_FUNC
FatMacho64File *fatMacho64Parse(const char *fn);

/***
 * @warning You must completed all jobs with this FatMacho64File, otherwise
 *          you will free all information about this file including sections,
 *          symbols etc
 *
 * @brief free memory from FatMacho64File structure ff
 *
 * @param[in] ff pointer to FatMacho64File struct
 */
EXPORT_FUNC
void fatMacho64Free(FatMacho64File *ff);

/***
 * @brief Fully check FatMacho64File structure
 *
 * @param[in] ff FatMacho64File structure, that is nedded to check
 *
 * @return MACHO64_OK or
 *         MACHO64_INV_ARG, MACHO64_NO_HEADER, MACHO64_NO_LOAD_COMMAND,
 *         MACHO64_NO_SYMTAB_CMD, MACHO64_NO_SYMTAB, MACHO64_NO_SYM_NAME_TAB,
 *         MACHO64_NO_INDIRECT_SYM_TAB, MACHO64_NO_SEGMENTS
 *         MACHO64_NO_FAT_HEADER
 */
EXPORT_FUNC
MACHO64_ERROR fatMacho64Check(const FatMacho64File *ff);

/***
 * @brief Return pointer to Macho64File according to architecture
 *
 * @param[in] ff fat macho file
 * @param[in] arch architecture spicifier
 *
 * @return pointer to Macho64File
*/
EXPORT_FUNC
Macho64File* fatMacho64GetMacho64ByArch(FatMacho64File *ff, Arch arch);

/***
 * @brief Return const pointer to Macho64File according to architecture
 *
 * @param[in] ff fat macho file
 * @param[in] arch architecture spicifier
 *
 * @return const pointer to Macho64File
*/
EXPORT_FUNC
const Macho64File* fatMacho64GetConstMacho64ByArch( const FatMacho64File *ff
                                                  , Arch arch
                                                  );

/***
 * @brief Function returns pointer to the symbol with name
 *
 * @param[in] ff pointer to FatMacho64File structer
 * @param[in] name symbol name needed to find
 *
 * @return pointer to Macho64Sym or NULL if fail
 */
EXPORT_FUNC
Macho64Sym *fatMacho64GetSymByName(const FatMacho64File *ff, const char *name);

/***
 * @brief Function returns name of the symbol
 *
 * @param[in] ff pointer to the target FatMacho64File
 * @param[in] ms pointer to symbol structure
 *
 * @return pointer to name of symbol or NULL if fail
 */
EXPORT_FUNC
char *fatMacho64GetSymName(const FatMacho64File *ff, const Macho64Sym *ms);

/***
 * @brief Function for work with qsort. Functions compare addresses of symbols
 *        and returns 1/-1/0 if @a->addr >/</== b->addr.
 *
 * @param[in] a pointer to a fist symbol
 * @param[in] b pointer to a second symbol
 */
static INLINE
int fatMacho64CmpSym(const void *a, const void *b)
{
    return macho64CmpSym(a, b);
}

/***
 * @brief Function returns pointer to the static symbols table
 *
 * @param[in] ff pointer to the target FatMacho64File
 *
 * @return pointer to static symbol table or NULL if fail
 */
EXPORT_FUNC
Macho64Sym *fatMacho64GetSSymTab(const FatMacho64File *ff);

/***
 * @brief Function returns amount of static symbols
 *
 * @param[in] ff pointer to the target FatMacho64File
 *
 * @return amount of static symbols or -1
 */
EXPORT_FUNC
uint64_t fatMacho64GetAmountSSym(const FatMacho64File *ff);

/***
 * @brief returns addr of static symbol without ASLR
 *
 * @param[in] ms pointer to the target static Macho64Sym
 *
 * @return address of -1
 */
static INLINE
uint64_t fatMacho64GetSSymAddr(const Macho64Sym *ms)
{
    return macho64GetSSymAddr(ms);
}

/***
 * @brief Return addr of static symbol with name
 *
 * @param[in] elf64 pointer to Elf64File structer
 * @param[in] name name of static symbol
 *
 * @return static symbol address of -1
 */
EXPORT_FUNC
uint64_t fatMacho64GetAddrSymByName(const FatMacho64File *ff, const char *name);

/***
 * @brief returns size of static symbol
 *
 * @param[in] ff pointer to FatMacho64File structer
 * @param[in] ms pointer to the target static Macho64Sym
 *
 * @return size of static symbol or -1
 */
EXPORT_FUNC
uint64_t fatMacho64GetSSymSize(const FatMacho64File *ff, const Macho64Sym *ms);

/***
 * @brief returns file position for the static symbol
 *
 * @param[in] ff pointer to the target FatMacho64File
 * @param[in] ms pointer to symbol
 *
 * @return file offset to start of symbol or -1
 */
EXPORT_FUNC
uint64_t fatMacho64GetSSymFileoff(const FatMacho64File *ff, const Macho64Sym *ms);

/***
 * @brief returns an amount of segments
 *
 * @param[in] ff pointer to the target FatMacho64File
 *
 * @return amount of segments
 */
EXPORT_FUNC
uint64_t fatMacho64GetAmountSeg(const FatMacho64File *ff);

/***
 * @brief returns a descriptor of a section with name
 *
 * @param[in] ff pointer to the target FatMacho64File
 * @param[in] name section name
 *
 * @return pointer to a section of NULL
 */
EXPORT_FUNC
Macho64Sect *fatMacho64GetSectByName(const FatMacho64File *ff, const char *name);

/***
 * @brief returns last loadable section in binary file
 *
 * @param[in] ff pointer to the target FatMacho64File
 *
 * @return pointer to a section of NULL
 */
EXPORT_FUNC
Macho64Sect *fatMacho64GetLastLoadableSect(const FatMacho64File *ff);

/***
 * @brief Reads section contents
 *
 * @param[in] ff pointer to the target FatMacho64File
 * @param[in] sect section descriptor
 *
 * @return pointer to a content or NULL
 * @warning need to free memory
 */
EXPORT_FUNC
void *fatMacho64ReadSect(const FatMacho64File *ff, const Macho64Sect *sect);

/***
 * @brief returns an amount of sections
 *
 * @param[in] ff pointer to the target FatMacho64File
 *
 * @return amount of sections
 */
EXPORT_FUNC
uint64_t fatMacho64GetAmountSect(const FatMacho64File *ff);

/***
 * @brief returns name of the section
 *
 * @param[in] ff pointer to the target FatMacho64File
 * @param[in] sect section descriptor
 *
 * @return pointer of NULL
 */
EXPORT_FUNC
const char* fatMacho64GetSectName(const FatMacho64File *ff, const Macho64Sect *sect);

/***
 * @brief returns a size of a section
 *
 * @param[in] sect pointer to section descriptor
 *
 * @return section size or -1
 */
static INLINE
uint64_t fatMacho64GetSectSize(const Macho64Sect *sect)
{
    return macho64GetSectSize(sect);
}

/***
 * @brief returns a virtual address of a section start
 *
 * @param[in] sect pointer to section descriptor
 *
 * @return addr or -1
 */
static INLINE
uint64_t fatMacho64GetSectAddr(const Macho64Sect *sect)
{
    return macho64GetSectAddr(sect);
}

/***
 * @brief returns a file offset of a section.
 *
 * @param[in] sect pointer to section descriptor.
 *
 * @return file offset to a section start
 */
static INLINE
uint64_t fatMacho64GetSectFileoff(const Macho64Sect *sect)
{
    return macho64GetSectFileoff(sect);
}

#endif /* __FAT_MACHO64_PARSE_H */


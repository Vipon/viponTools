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

#ifndef _ELF64_PARSE_H
#define _ELF64_PARSE_H

#include "os.h"
#include "file.h"

/* Unix headers */
#include <elf.h>

/* C standard headers */
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

/***
 * Elf64_Word:
 * size      -   4 bytes
 * alignment -   4 bytes
 */
typedef Elf64_Word Elf64Word;

/***
 *   typedef struct
 *   {
 *       unsigned char   e_ident[16];
 *       Elf64_Half      e_type;
 *       Elf64_Half      e_machine;
 *       Elf64_Word      e_version;
 *       Elf64_Addr      e_entry;
 *       Elf64_Off       e_phoff;
 *       Elf64_Off       e_shoff;     // Sect header offset
 *       Elf64_Word      e_flags;
 *       Elf64_Half      e_ehsize;
 *       Elf64_Half      e_phentsize;
 *       Elf64_Half      e_phnum;
 *       Elf64_Half      e_shentsize;
 *       Elf64_Half      e_shnum;     // Number of section header entries
 *       Elf64_Half      e_shstrndx;  // Sect name string table index
 *   } Elf64_Ehdr;
 */
typedef Elf64_Ehdr Elf64Ehdr;

/***
 *  typedef struct
 *  {
 *      Elf64_Word    p_type;   // Segment type
 *      Elf64_Word    p_flags;  // Segment flags
 *      Elf64_Off     p_offset; // Segment file offset
 *      Elf64_Addr    p_vaddr;  // Segment virtual address
 *      Elf64_Addr    p_paddr;  // Segment physical address
 *      Elf64_Xword   p_filesz; // Segment size in file
 *      Elf64_Xword   p_memsz;  // Segment size in memory
 *      Elf64_Xword   p_align;  // Segment alignment
 *  } Elf64_Phdr;
 */
typedef Elf64_Phdr Elf64Phdr;

/***
 *   typedef struct {
 *       Elf64_Word      sh_name;      // Sect name
 *       Elf64_Word      sh_type;      // Sect type
 *       Elf64_Xword     sh_flags;
 *       Elf64_Addr      sh_addr;
 *       Elf64_Off       sh_offset;    // Offset in file
 *       Elf64_Xword     sh_size;      // Size of section
 *       Elf64_Word      sh_link;
 *       Elf64_Word      sh_info;
 *       Elf64_Xword     sh_addralign;
 *       Elf64_Xword     sh_entsize;
 *   } Elf64_Shdr;
 */
typedef Elf64_Shdr  Elf64Shdr;

/***
 *   typedef struct {
 *       Elf64_Word      st_name;
 *       unsigned char   st_info;
 *       unsigned char   st_other;
 *       Elf64_Half      st_shndx;
 *       Elf64_Addr      st_value;
 *       Elf64_Xword     st_size;
 *   } Elf64_Sym;
 */
typedef Elf64_Sym   Elf64Sym;

/***
 *   typedef struct {
 *       Elf64_Addr      r_offset;
 *       Elf64_Xword     r_info;
 *       Elf64_Sxword    r_addend;
 *   } Elf64_Rela;
 *
 *   x64, there are only relocations of type RELA
 */
typedef Elf64_Rela  Elf64Rel;

typedef Elf64_Dyn Elf64Dyn;

typedef Elf64_Verneed Elf64Verneed;
typedef Elf64_Vernaux Elf64Vernaux;

/***
 * This section holds RELA type relocation information for all sections of a
 * shared library except the PLT.
 */
#define RELADYN     ".rela.dyn"
/***
 * .rela.plt:
 *   .rela   -  Relocation information.
 *   .plt    -  The procedure linkage table.
 */
#define RELAPLT     ".rela.plt"
/***
 * .symtab   -  This section holds a symbol table.
 */
#define SYMTAB      ".symtab"
/***
 * .dynsym   -  This section holds the dynamic linking symbol table.
 */
#define DYNSYM      ".dynsym"
/***
 * .strtab  -   This section holds strings, most commonly the strings that
 *              represent the names associated with symbol table entries.
 */
#define STRTAB      ".strtab"
/***
 * .dynstr  -   This section holds strings needed for dynamic linking,
 *              most commonly the strings that represent the names
 *              associated with symbol table entries.
 */
#define DYNSTR      ".dynstr"

/***
 * STT_FUNC The symbol is associated with a function or other executable code.
 */
#define IS_ELF64_SYM_FUNC(sym)  (ELF64_ST_TYPE(sym.st_info) == STT_FUNC)

/***
 * STT_OBJECT The symbol is associated with a data object, such as a variable,
 * an array, etc.
 */
#define IS_ELF64_SYM_DATA(sym)  (ELF64_ST_TYPE(sym.st_info) == STT_OBJECT)

/***
 * STT_NOTYPE The symbol’s type is not specified.
 */
#define IS_ELF64_SYM_LABEL(sym) (ELF64_ST_TYPE(sym.st_info) == STT_NOTYPE)

/***
 * sym.st_name - show positon into the symbol string table. If st_name equal
 * to zero, it mean symbol has no name - STN_UNDEF.
 */
#define IS_ELF64_SYM_UNDEF(sym) (sym.st_name == 0)

/***
 * Defines for checking type of binary files
 */
#define IS_ELF64_FILE_OBJ(elf)  (elf->type == ET_REL)
#define IS_ELF64_FILE_EXEC(elf) (elf->type == ET_EXEC)

typedef struct {
    char        *fn;
    FileD       fd;
    size_t      fs;
    uint8_t     *faddr;
    uint32_t    type;
    Arch        arch;
    Elf64Ehdr   *header;
    Elf64Shdr   *sections;
    Elf64Phdr   *segments;
    Elf64Sym    *symtab;
    uint64_t    symnum;
    Elf64Sym    *dynsym;
    uint64_t    dynsymnum;
    Elf64Rel    *relaplt;
    Elf64Rel    *reladyn;
    Elf64Dyn    *dynamic;
    char        *sectNameTab;
    char        *symNameTab;
    char        *dynSymNameTab;
    char        *dtStrTab;

} Elf64File;

#ifdef __WIN__
typedef enum : uint64_t {
#else
typedef enum {
#endif /* __WIN__ */
    ELF64_NO_DT_STRTAB = (uint64_t)-18,
    ELF64_NO_DYNAMIC,
    ELF64_NO_RELOCATION,
    ELF64_NO_SECTION,
    ELF64_NO_SYMBOL,
    ELF64_NO_RELADYN,
    ELF64_NO_RELAPLT,
    ELF64_NO_DYN_SYM_NAME_TAB,
    ELF64_NO_SYM_NAME_TAB,
    ELF64_NO_SH_NAME_TAB,
    ELF64_NO_DYNSYM,
    ELF64_NO_SYMTAB,
    ELF64_NO_SYMBOLS,
    ELF64_NO_SEGMENTS,
    ELF64_NO_SECTIONS,
    ELF64_NO_HEADER,
    ELF64_NO_MEM,
    ELF64_INV_ARG,
    ELF64_OK = 0
} ELF64_ERROR;

static_assert(sizeof(ELF64_ERROR) == 8, "ELF64_ERROR must be 64 bit");
static_assert(((int64_t)ELF64_INV_ARG) < 0, "ERRORS must be negative");

/***
 * @brief Parse elf64 binary file
 *
 * @param[in] fn Binary file name. C string with terminal null
 *
 * @return Pointer to Elf64File structer or NULL if fail
 */
EXPORT_FUNC
Elf64File *elf64Parse(const char *fn);

/***
 * @brief You must complete all jobs with this elf64, otherwise you will free
 *        all information about this file including sections, symbols etc
 * @param[in] elf64 pointer to Elf64File structer
 */
EXPORT_FUNC
void elf64Free(Elf64File *elf64);

/***
 * @brief Check if elf64 points to a valid structure
 *
 * @param[in] elf64 pointer to Elf64File structure to check
 *
 * @return ELF64_OK or number of error (see ELF64ERROR)
 */
EXPORT_FUNC
ELF64_ERROR elf64Check(const Elf64File *elf64);

/***
 * @brief Function returns pointer to the symbol with name
 *
 * @param[in] elf64 pointer to Elf64File structer
 * @param[in] name symbol name needed to find
 *
 * @return pointer to Elf64Sym or NULL if fail
 */
EXPORT_FUNC
Elf64Sym *elf64GetSymByName(const Elf64File *elf64, const char *name);

/***
 * @brief Function returns name of the symbol
 *
 * @param[in] elf64 pointer to the target elf64File
 * @param[in] sym pointer to symbol structure
 *
 * @return pointer to name of symbol or NULL if fail
 */
EXPORT_FUNC
char *elf64GetSymName(const Elf64File *elf64, const Elf64Sym *sym);

/***
 * @brief Function returns name of dynamic symbol
 *
 * @param[in] elf64 pointer to the target elf64File
 * @param[in] sym pointer to symbol structure
 *
 * @return pointer to name of symbol or NULL if fail
 */
EXPORT_FUNC
char *elf64GetDSymName(const Elf64File *elf64, const Elf64Sym *sym);

/***
 * @brief Function for work with qsort. Functions compare addresses of symbols
 *        and returns 1/-1/0 if @a->addr >/</== b->addr.
 *
 * @param[in] a pointer to a fist symbol
 * @param[in] b pointer to a second symbol
 */
EXPORT_FUNC
int elf64CmpSym(const void *a, const void *b);

/***
 * @brief Function returns pointer to the static symbols table
 *
 * @param[in] elf64 pointer to the target Elf64File
 *
 * @return pointer to static symbol table or NULL if fail
 */
EXPORT_FUNC
Elf64Sym *elf64GetSSymTab(const Elf64File *elf64);

/***
 * @brief Function returns pointer to the dynamic symbols table
 *
 * @param[in] elf64 pointer to the target Elf64File
 *
 * @return pointer to dynamic symbol table or NULL if fail
 */
EXPORT_FUNC
Elf64Sym *elf64GetDSymTab(const Elf64File *elf64);

/***
 * @brief Function returns amount of static symbols
 *
 * @param[in] elf64 pointer to the target Elf64File
 *
 * @return amount of static symbols or -1
 */
EXPORT_FUNC
uint64_t elf64GetAmountSSym(const Elf64File *elf64);

/***
 * @brief Function returns amount of dynamic symbols
 *
 * @param[in] elf64 pointer to the target Elf64File
 *
 * @return amount of dynamic symbols or -1
 */
EXPORT_FUNC
uint64_t elf64GetAmountDSym(const Elf64File *elf64);

/***
 * @brief returns addr of static symbol without ASLR
 *
 * @param[in] sym pointer to the target static Elf64Sym
 *
 * @return address of -1
 */
EXPORT_FUNC
uint64_t elf64GetSSymAddr(const Elf64Sym *sym);

/***
 * @brief Return addr of static symbol with name
 *
 * @param[in] elf64 pointer to Elf64File structer
 * @param[in] name name of static symbol
 *
 * @return static symbol address of -1
 */
EXPORT_FUNC
uint64_t elf64GetAddrSymByName(const Elf64File *elf64, const char *name);

/***
 * @brief returns size of static symbol
 *
 * @param[in] elf64 pointer to Elf64File structer
 * @param[in] sym pointer to the target static Elf64Sym
 *
 * @return size of static symbol or -1
 */
EXPORT_FUNC
uint64_t elf64GetSSymSize(const Elf64File *elf64, const Elf64Sym *sym);

/***
 * @brief returns file position for the static symbol
 *
 * @param[in] elf64 pointer to the target Elf64File
 * @param[in] sym pointer to symbol
 *
 * @return file offset to start of symbol or -1
 */
EXPORT_FUNC
uint64_t elf64GetSSymFileoff(const Elf64File *elf64, const Elf64Sym *sym);

/***
 * @brief returns index of symbol with name in dynamic table
 *
 * @param[in] elf64 pointer to Elf64File
 * @param[in] name target dynamic symbol name
 *
 * @return Index of symbol with name in dynamic table or -1
 */
EXPORT_FUNC
uint64_t elf64GetDSymIndxByName(const Elf64File *elf64, const char *name);

/***
 * @brief returns an amount of segments
 *
 * @param[in] elf64 pointer to the target Elf64File
 *
 * @return amount of segments
 */
EXPORT_FUNC
uint64_t elf64GetAmountSeg(const Elf64File *elf64);

/***
 * @brief returns pointer to the fist section with type
 *
 * @param[in] elf64 pointer to the target Elf64File
 * @param[in] sh_type section type
 *
 * @return pointer to section of NULL
 */
EXPORT_FUNC
Elf64Shdr *elf64GetSectByType(const Elf64File *elf64, const Elf64Word type);

/***
 * @brief returns a descriptor of a section with name
 *
 * @param[in] elf64 pointer to the target Elf64File
 * @param[in] name section name
 *
 * @return pointer to a section of NULL
 */
EXPORT_FUNC
Elf64Shdr *elf64GetSectByName(const Elf64File *elf64, const char* name);

/***
 * @brief returns a descriptor of a section contains addr
 *
 * @param[in] elf64 pointer to the target Elf64File
 * @param[in] addr address without ASLR
 *
 * @return pointer to a section of NULL
 */
EXPORT_FUNC
Elf64Shdr *elf64GetSectByAddr(const Elf64File *elf64, uint64_t addr);

/***
 * @brief returns last loadable section in binary file
 *
 * @param[in] elf64 pointer to the target Elf64File
 *
 * @return pointer to a section of NULL
 */
EXPORT_FUNC
Elf64Shdr *elf64GetLastLoadableSect(const Elf64File *elf64);

/***
 * @brief Reads section contents
 *
 * @param[in] elf64 pointer to the target Elf64File
 * @param[in] sect section descriptor
 *
 * @return pointer to a content or NULL
 * @warning need to free memory
 */
EXPORT_FUNC
void *elf64ReadSect(const Elf64File *elf64, const Elf64Shdr *sect);

/***
 * @brief returns an amount of sections
 *
 * @param[in] elf64 pointer to the target Elf64File
 *
 * @return amount of sections
 */
EXPORT_FUNC
uint64_t elf64GetAmountSect(const Elf64File *elf64);

/***
 * @brief returns name of the section
 *
 * @param[in] elf64 pointer to the target Elf64File
 * @param[in] sect section descriptor
 *
 * @return pointer of NULL
 */
EXPORT_FUNC
const char* elf64GetSectName(const Elf64File *elf64, const Elf64Shdr *sect);

/***
 * @brief returns a size of a section
 *
 * @param[in] sect pointer to section descriptor
 *
 * @return section size or -1
 */
EXPORT_FUNC
uint64_t elf64GetSectSize(const Elf64Shdr *sect);

/***
 * @brief returns a virtual address of a section start
 *
 * @param[in] sect pointer to section descriptor
 *
 * @return addr or -1
 */
EXPORT_FUNC
uint64_t elf64GetSectAddr(const Elf64Shdr *sect);

/***
 * @brief returns a file offset of a section.
 *
 * @param[in] sect pointer to section descriptor.
 *
 * @return file offset to a section start
 */
EXPORT_FUNC
uint64_t elf64GetSectFileoff(const Elf64Shdr *sect);

/***
 * @brief returns addend for addr in section
 *
 * @param[in] elf64 binary file descriptor
 * @param[in] sect pointer to section descriptor
 * @param[in] addr address of relocatable data
 *
 * @return addend or -1
 */
EXPORT_FUNC
uint64_t elf64GetRelocForAddr(const Elf64File *elf64, const Elf64Shdr *sect, uint64_t addr);

/***
 * @brief returns memory space position of actual dynamic func address
 *
 * @param[in] elf64 Elf64File structer
 * @param[in] func function name
 *
 * @return vaddr or NULL
 */
EXPORT_FUNC
void *elf64GetRelocDataAddr(const Elf64File *elf64, const char *func);

/***
 * @brief returns symbol version info
 *
 * @param[in] elf64 elf file descriptor
 * @param[in] inds index of dynamic symbol in dynamic table
 *
 * @return symbol version info
*/
EXPORT_FUNC
uint16_t elf64GetSymVersionByIndx(const Elf64File *elf64, uint64_t indx);

/***
 * @brief returns version name by varsion info
 *
 * @param[in] elf64 elf file descriptor
 * @param[in] ver version info
 *
 * @return pointer to version name or NULL
*/
EXPORT_FUNC
const char *elf64GetVerNameBySymVersion(const Elf64File *elf, uint16_t ver);

#endif /* _ELF64_PARSE_H */


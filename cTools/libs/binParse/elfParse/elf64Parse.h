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

#ifndef _ELF64_PARSE_H
#define _ELF64_PARSE_H

/* Unix headers */
#include <elf.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>

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
 *       Elf64_Off       e_shoff;       // Section header offset
 *       Elf64_Word      e_flags;
 *       Elf64_Half      e_ehsize;
 *       Elf64_Half      e_phentsize;
 *       Elf64_Half      e_phnum;
 *       Elf64_Half      e_shentsize;
 *       Elf64_Half      e_shnum;       // Number of section header entries
 *       Elf64_Half      e_shstrndx;    // Section name string table index
 *   } Elf64_Ehdr;
 */
typedef Elf64_Ehdr  Elf64Ehdr;
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
typedef Elf64_Phdr  Elf64Phdr;
/***
 *   typedef struct {
 *       Elf64_Word      sh_name;        // Section name
 *       Elf64_Word      sh_type;        // Section type
 *       Elf64_Xword     sh_flags;
 *       Elf64_Addr      sh_addr;
 *       Elf64_Off       sh_offset;      // Offset in file
 *       Elf64_Xword     sh_size;        // Size of section
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
/***
 * #define ELF64_R_SYM(info) ((info)>>32)
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
 * Symbol table entry type
 *  ELF64_ST_TYPE(info)
 */

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
    int         fd;
    uint32_t    type;
    Elf64Ehdr   *header;
    Elf64Shdr   *sections;
    Elf64Phdr   *segments;
    Elf64Sym    *symtab;
    uint64_t    symnum;
    Elf64Sym    *dynsym;
    uint64_t    dynsymnum;
    Elf64Sym    *sortSymtab;
    Elf64Rel    *relaplt;
    Elf64Rel    *reladyn;
    char        *sectNameTab;
    char        *symNameTab;
    char        *dynSymNameTab;
} Elf64File;


typedef enum {
    ELF64_NO_RELOCATION = (uint64_t)-16,
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
 * Input:
 *  @fn - binary file name. C string with terminal null.
 * Output:
 *  Success:
 *      point to Elf64File structer.
 *  Fail:
 *      Null point.
 * After:
 *  need to call function void elf64Free(Elf64File *elf64).
 */
Elf64File *elf64Parse(const char *fn);


/***
 * Before:
 *  You must completed all jobs with this elf64, otherwise you will free all
 *  information about this file including sections, symbols etc.
 * Input:
 *  @elf64 - point to Elf64File structer, that is necessary to free.
 * After:
 *  you should to assigned elf64 = NULL.
 */
void elf64Free(Elf64File *elf64);


/***
 * Input:
 *  @elf64 - Elf64File structure, that is nedded to check.
 * Output:
 *  Success:
 *      If all fields are initialized, function will return 0.
 *  Fail:
 *      -1.
 */
ELF64_ERROR elf64FullCheckFile(const Elf64File *elf64);


/***
 * Description:
 *  Function prints all symbols in @elf64 with information.
 * Input:
 *  @elf64 - point to target Elf64File.
 * Output:
 *  Success:
 *      0.
 *  Fail:
 *      -1.
 */
ELF64_ERROR elf64PrintSymbol(const Elf64File *elf64, const Elf64Sym *sym);
ELF64_ERROR elf64PrintSymbols(const Elf64File *elf64);


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Description:
 *  Function returns pointer to the symbol with @name.
 * Input:
 *  @elf64 - Elf64File structer with initialized fields: fd, elf64Header,
 *        sectionHeaders and symbols.
 *  @name - target section name.
 * Output:
 *  Success:
 *      point to elf64 symbols(maybe dynamic) from Elf64File structer.
 *  Fail:
 *      NULL point.
 */
Elf64Sym *elf64GetSymByName(const Elf64File *elf64, const char *name);


/***
 * Description:
 *  Function returns name of the symbol @sym.
 * Input:
 *  @elf64 -   point to the targer elf64File.
 *  @sym -   point to symbol structure.
 * Output:
 *  Success:
 *      point to name of symbol.
 *  Fail:
 *      NULL point.
 */
char *elf64GetSymName(const Elf64File *elf64, const Elf64Sym *sym);


/***
 * Description:
 *  Function for work with qsort. Functions compare addresses of symbols and
 *  returns 1/-1/0 if a->addr >/</== b->addr.
 */
int elf64CmpSym(const void *a, const void *b);


/***
 * Description:
 *  Function returns pointer to the static symbols table.
 * Input:
 *  @elf64 - point to the targer elf64File.
 * Output:
 *  Success:
 *      point to table of static symbols from elf64File structer.
 *  Fail:
 *      NULL point.
 */
Elf64Sym *elf64GetSSymTable(const Elf64File *elf64);
Elf64Sym *elf64GetSSymSortTable(const Elf64File *elf64);


/***
 * Description:
 *  Function returns amount of static symbols.
 * Input:
 *  @elf64 -   point to the targer elf64File.
 * Output:
 *  Success:
 *      amount of static symbols from the elf64 structer.
 *  Fail:
 *      -1.
 */
uint64_t elf64GetAmountSSym(const Elf64File *elf64);


/***
 * Description:
 *  Function returns addr of static symbol without randomization address space.
 * Input:
 *  @sym - point to the targer static Elf64Sym.
 * Output:
 *  Success:
 *      addr of the static symbol sym.
 *  Fail:
 *      -1.
 */
uint64_t elf64GetSSymAddr(const Elf64Sym *sym);


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf64 - Elf64File structer with initialized fields: fd, elf64Header,
 *        sectionHeaders and symbols.
 *  @name  - target section name.
 * Output:
 *  Success:
 *      virtual addres of elf64 symbols from Elf64File structer.
 *  Fail:
 *      -1.
 */
uint64_t elf64GetAddrSymByName(const Elf64File *elf64, const char *name);


/***
 * Description:
 *  Function returns size of static symbol without randomization address space.
 * Input:
 *  @sym - point to the targer static Elf64Sym.
 * Output:
 *  Success:
 *      size of the static symbol sym.
 *  Fail:
 *      -1.
 */
uint64_t elf64GetSSymSize(const Elf64File *elf64, const Elf64Sym *sym);


/***
 * Description:
 *  Function returns file position for the static symbol @sym.
 * Input:
 *  @elf64 - point to the targer Elf64File.
 *  @sym - point to symbol structure.
 * Output:
 *  Success:
 *      file position of the static symbol.
 *  Fail:
 *      -1.
 */
uint64_t elf64GetSSymFileoff(const Elf64File *elf64, const Elf64Sym *sym);


/***
 * Before:
 *   If you need a file position, you should to save it.
 * Input:
 *   elf64 - Elf64File structer with initialized fields: fd, elf64Header,
 *         sectionHeaders and symbols.
 *   name - target symbol name.
 * Output:
 *  Success:
 *      index of dynamic symbol from Symbols structer in Elf64File structer.
 *  Fail:
 *      -1.
 */
uint64_t elf64GetDSymIndex(const Elf64File *elf64, const char *name);


/***
 *  Function returns an amount of segments in the binary file.
 * Input:
 *  @elf64 - point to the targer Elf64File.
 * Output:
 *  Success:
 *      amount of segments in the binary file.
 *  Fail:
 *      -1.
 */
uint64_t elf64GetAmountSegment(const Elf64File *elf64);


/***
 * Input:
 *  sh_type - this member categorizes the section’s contents and semantics.
 * Output:
 *  Success:
 *      point to elf64 section header from Elf64File structer
 *  Fail:
 *      NULL point.
 */
Elf64Shdr *elf64GetSectionByType(const Elf64File *elf64, const Elf64Word sh_type);


/***
 * Description:
 *  Function returns a descriptor of a section with @name.
 * Input:
 *  @elf64 - Elf64File structer with initialized fields: fd, elf64Header and
 *        sectionHeaders.
 *  @name - target section name.
 * Output:
 *  Success:
 *      point to elf64 section header from Elf64File structer
 *  Fail:
 *      NULL point.
 */
Elf64Shdr *elf64GetSectionByName(const Elf64File *elf64, const char* name);
Elf64Shdr *elf64GetLastLoadableSection(const Elf64File *elf64);


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  elf64 - Elf64File structer with initialized field fd.
 *  sectionHeader - header of section is related with elf64 file.
 * Output:
 *  Success:
 *      point to elf64 section data.
 *  Fail:
 *      NULL point.
 * After:
 *  need to free memory
 */
void *elf64ReadSection(const Elf64File *elf64, const Elf64Shdr *sectionHeader);


/***
 * Description:
 *  Function returns an amount of sections in the binary file.
 */
uint64_t elf64GetAmountSection(const Elf64File *elf64);


/***
 * Description:
 *  Function returns name of the section @sect.
 * Input:
 *  @elf64 - point to the targer Elf64File.
 *  @sect - point to section descriptor.
 * Output:
 *  Success:
 *      point to name of section.
 *  Fail:
 *      NULL.
 */
const char* elf64GetSectionName(const Elf64File *elf64, const Elf64Shdr *sect);


/***
 * Description:
 *  Function returns a size of a section.
 * Input:
 *  @sect - point to section descriptor.
 * Output:
 *  Success:
 *      section size.
 *  Fail:
 *      -1.
 */
uint64_t elf64GetSectionSize(const Elf64Shdr *elf64Sect);


/***
 * Description:
 *  Function returns a virtual address of a section.
 * Input:
 *  @sect - point to section descriptor.
 * Output:
 *  Success:
 *      section virtual addr without randomization of an address space.
 *  Fail:
 *      -1.
 */
uint64_t elf64GetSectionVaddr(const Elf64Shdr *sect);


/***
 * Description:
 *  Function returns a file offset of a section.
 * Input:
 *  @sect - point to section descriptor.
 * Output:
 *  Success:
 *      position of section in an executable file.
 *  Fail:
 *      -1.
 */
uint64_t elf64GetSectionFileoff(const Elf64Shdr *sect);


/***
 * Description:
 *  Function returns a relocation number for data, that is located at @addr in
 *  section @sect.
 * Input:
 *  @elf64 - binary file descriptor.
 *  @sect - point to section descriptor.
 *  @addr - address of relocatable data.
 * Output:
 *  Success:
 *      relocation offset.
 *  Fail:
 *      NULL.
 */
uint64_t elf64GetRelocationForAddr(const Elf64File *elf64, const Elf64Shdr *sect, uint64_t addr);


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  elf64 - Elf64File structer with initialized field fd.
 *  func - name of function, that is nedded to hooked.
 *  hand - address of handler function.
 * Output:
 *  Success:
 *      Old relocation addr.
 *  Fail:
 *      NULL point.
 */
void *elf64Hook(const Elf64File *elf64, const char *func, const void *hand);


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  elf64 - Elf64File structer with initialized field fd.
 *  func - name of function, that is nedded.
 * Output:
 *  Success:
 *      Relocation addr.
 *  Fail:
 *      NULL point.
 */
void *elf64GetRelocationDataAddr(const Elf64File *elf64, const char *func);

#endif /* _ELF64_PARSE_H */


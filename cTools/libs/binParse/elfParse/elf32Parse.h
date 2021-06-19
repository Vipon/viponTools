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

#ifndef _ELF32_PARSE_H
#define _ELF32_PARSE_H

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
 * Elf32_Word:
 * size      -   4 bytes
 * alignment -   4 bytes
 */
typedef Elf32_Word Elf32Word;
/***
 *   typedef struct
 *   {
 *       unsigned char   e_ident[16];
 *       Elf32_Half      e_type;
 *       Elf32_Half      e_machine;
 *       Elf32_Word      e_version;
 *       Elf32_Addr      e_entry;
 *       Elf32_Off       e_phoff;
 *       Elf32_Off       e_shoff;       // Sect header offset
 *       Elf32_Word      e_flags;
 *       Elf32_Half      e_ehsize;
 *       Elf32_Half      e_phentsize;
 *       Elf32_Half      e_phnum;
 *       Elf32_Half      e_shentsize;
 *       Elf32_Half      e_shnum;       // Number of section header entries
 *       Elf32_Half      e_shstrndx;    // Sect name string table index
 *   } Elf32_Ehdr;
 */
typedef Elf32_Ehdr  Elf32Ehdr;
/***
 *  typedef struct
 *  {
 *      Elf32_Word    p_type;   // Segment type
 *      Elf32_Word    p_flags;  // Segment flags
 *      Elf32_Off     p_offset; // Segment file offset
 *      Elf32_Addr    p_vaddr;  // Segment virtual address
 *      Elf32_Addr    p_paddr;  // Segment physical address
 *      Elf32_Xword   p_filesz; // Segment size in file
 *      Elf32_Xword   p_memsz;  // Segment size in memory
 *      Elf32_Xword   p_align;  // Segment alignment
 *  } Elf32_Phdr;
 */
typedef Elf32_Phdr  Elf32Phdr;
/***
 *   typedef struct {
 *       Elf32_Word      sh_name;        // Sect name
 *       Elf32_Word      sh_type;        // Sect type
 *       Elf32_Xword     sh_flags;
 *       Elf32_Addr      sh_addr;
 *       Elf32_Off       sh_offset;      // Offset in file
 *       Elf32_Xword     sh_size;        // Size of section
 *       Elf32_Word      sh_link;
 *       Elf32_Word      sh_info;
 *       Elf32_Xword     sh_addralign;
 *       Elf32_Xword     sh_entsize;
 *   } Elf32_Shdr;
 */
typedef Elf32_Shdr  Elf32Shdr;
/***
 *   typedef struct {
 *       Elf32_Word      st_name;
 *       unsigned char   st_info;
 *       unsigned char   st_other;
 *       Elf32_Half      st_shndx;
 *       Elf32_Addr      st_value;
 *       Elf32_Xword     st_size;
 *   } Elf32_Sym;
 */
typedef Elf32_Sym   Elf32Sym;
/***
 *   typedef struct {
 *       Elf32_Addr      r_offset;
 *       Elf32_Xword     r_info;
 *       Elf32_Sxword    r_addend;
 *   } Elf32_Rela;
 *
 *   x32, there are only relocations of type RELA
 */
typedef Elf32_Rela  Elf32Rel;
/***
 * #define ELF32_R_SYM(info) ((info)>>32)
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
 *  ELF32_ST_TYPE(info)
 */

/***
 * STT_FUNC The symbol is associated with a function or other executable code.
 */
#define IS_ELF32_SYM_FUNC(sym)  (ELF32_ST_TYPE(sym.st_info) == STT_FUNC)

/***
 * STT_OBJECT The symbol is associated with a data object, such as a variable,
 * an array, etc.
 */
#define IS_ELF32_SYM_DATA(sym)  (ELF32_ST_TYPE(sym.st_info) == STT_OBJECT)

/***
 * STT_NOTYPE The symbol’s type is not specified.
 */
#define IS_ELF32_SYM_LABEL(sym) (ELF32_ST_TYPE(sym.st_info) == STT_NOTYPE)

/***
 * sym.st_name - show positon into the symbol string table. If st_name equal
 * to zero, it mean symbol has no name - STN_UNDEF.
 */
#define IS_ELF32_SYM_UNDEF(sym) (sym.st_name == 0)

/***
 * Defines for checking type of binary files
 */
#define IS_ELF32_FILE_OBJ(elf)  (elf->type == ET_REL)
#define IS_ELF32_FILE_EXEC(elf) (elf->type == ET_EXEC)

typedef struct {
    char        *fn;
    int         fd;
    uint32_t    type;
    Elf32Ehdr   *header;
    Elf32Shdr   *sections;
    Elf32Phdr   *segments;
    Elf32Sym    *symtab;
    uint32_t    symnum;
    Elf32Sym    *dynsym;
    uint32_t    dynsymnum;
    Elf32Sym    *sortSymtab;
    Elf32Rel    *relaplt;
    Elf32Rel    *reladyn;
    char        *sectNameTab;
    char        *symNameTab;
    char        *dynSymNameTab;
} Elf32File;


typedef enum {
    ELF32_NO_RELOCATION = (uint32_t)-16,
    ELF32_NO_SECTION,
    ELF32_NO_SYMBOL,
    ELF32_NO_RELADYN,
    ELF32_NO_RELAPLT,
    ELF32_NO_DYN_SYM_NAME_TAB,
    ELF32_NO_SYM_NAME_TAB,
    ELF32_NO_SH_NAME_TAB,
    ELF32_NO_DYNSYM,
    ELF32_NO_SYMTAB,
    ELF32_NO_SYMBOLS,
    ELF32_NO_SEGMENTS,
    ELF32_NO_SECTIONS,
    ELF32_NO_HEADER,
    ELF32_NO_MEM,
    ELF32_INV_ARG,
    ELF32_OK = 0
} ELF32_ERROR;

static_assert(sizeof(ELF32_ERROR) == 4, "ELF32_ERROR must be 32 bit");
static_assert(((int32_t)ELF32_INV_ARG) < 0, "ERRORS must be negative");

/***
 * Input:
 *  @fn - binary file name. C string with terminal null.
 * Output:
 *  Success:
 *      point to Elf32File structer.
 *  Fail:
 *      Null point.
 * After:
 *  need to call function void elf32Free(Elf32File *elf32).
 */
Elf32File *elf32Parse(const char *fn);


/***
 * Before:
 *  You must completed all jobs with this elf32, otherwise you will free all
 *  information about this file including sections, symbols etc.
 * Input:
 *  @elf32 - point to Elf32File structer, that is necessary to free.
 * After:
 *  you should to assigned elf32 = NULL.
 */
void elf32Free(Elf32File *elf32);


/***
 * Input:
 *  @elf32 - Elf32File structure, that is nedded to check.
 * Output:
 *  Success:
 *      If all fields are initialized, function will return 0.
 *  Fail:
 *      -1.
 */
ELF32_ERROR elf32Check(const Elf32File *elf32);


/***
 * Description:
 *  Function prints all symbols in @elf32 with information.
 * Input:
 *  @elf32 - point to target Elf32File.
 * Output:
 *  Success:
 *      0.
 *  Fail:
 *      -1.
 */
ELF32_ERROR elf32PrintSymbol(const Elf32File *elf32, const Elf32Sym *sym);
ELF32_ERROR elf32PrintSymbols(const Elf32File *elf32);


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Description:
 *  Function returns pointer to the symbol with @name.
 * Input:
 *  @elf32 - Elf32File structer with initialized fields: fd, elf32Header,
 *        sectionHeaders and symbols.
 *  @name - target section name.
 * Output:
 *  Success:
 *      point to elf32 symbols(maybe dynamic) from Elf32File structer.
 *  Fail:
 *      NULL point.
 */
Elf32Sym *elf32GetSymByName(const Elf32File *elf32, const char *name);


/***
 * Description:
 *  Function returns name of the symbol @sym.
 * Input:
 *  @elf32 -   point to the targer elf32File.
 *  @sym -   point to symbol structure.
 * Output:
 *  Success:
 *      point to name of symbol.
 *  Fail:
 *      NULL point.
 */
char *elf32GetSymName(const Elf32File *elf32, const Elf32Sym *sym);


/***
 * Description:
 *  Function for work with qsort. Functions compare addresses of symbols and
 *  returns 1/-1/0 if a->addr >/</== b->addr.
 */
int elf32CmpSym(const void *a, const void *b);


/***
 * Description:
 *  Function returns pointer to the static symbols table.
 * Input:
 *  @elf32 - point to the targer elf32File.
 * Output:
 *  Success:
 *      point to table of static symbols from elf32File structer.
 *  Fail:
 *      NULL point.
 */
Elf32Sym *elf32GetSSymTab(const Elf32File *elf32);
Elf32Sym *elf32GetSSymSortTab(const Elf32File *elf32);


/***
 * Description:
 *  Function returns amount of static symbols.
 * Input:
 *  @elf32 -   point to the targer elf32File.
 * Output:
 *  Success:
 *      amount of static symbols from the elf32 structer.
 *  Fail:
 *      -1.
 */
uint32_t elf32GetAmountSSym(const Elf32File *elf32);


/***
 * Description:
 *  Function returns addr of static symbol without randomization address space.
 * Input:
 *  @sym - point to the targer static Elf32Sym.
 * Output:
 *  Success:
 *      addr of the static symbol sym.
 *  Fail:
 *      -1.
 */
uint32_t elf32GetSSymAddr(const Elf32Sym *sym);


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  @elf32 - Elf32File structer with initialized fields: fd, elf32Header,
 *        sectionHeaders and symbols.
 *  @name  - target section name.
 * Output:
 *  Success:
 *      virtual addres of elf32 symbols from Elf32File structer.
 *  Fail:
 *      -1.
 */
uint32_t elf32GetAddrSymByName(const Elf32File *elf32, const char *name);


/***
 * Description:
 *  Function returns size of static symbol without randomization address space.
 * Input:
 *  @sym - point to the targer static Elf32Sym.
 * Output:
 *  Success:
 *      size of the static symbol sym.
 *  Fail:
 *      -1.
 */
uint32_t elf32GetSSymSize(const Elf32File *elf32, const Elf32Sym *sym);


/***
 * Description:
 *  Function returns file position for the static symbol @sym.
 * Input:
 *  @elf32 - point to the targer Elf32File.
 *  @sym - point to symbol structure.
 * Output:
 *  Success:
 *      file position of the static symbol.
 *  Fail:
 *      -1.
 */
uint32_t elf32GetSSymFileoff(const Elf32File *elf32, const Elf32Sym *sym);


/***
 * Before:
 *   If you need a file position, you should to save it.
 * Input:
 *   elf32 - Elf32File structer with initialized fields: fd, elf32Header,
 *         sectionHeaders and symbols.
 *   name - target symbol name.
 * Output:
 *  Success:
 *      index of dynamic symbol from Symbols structer in Elf32File structer.
 *  Fail:
 *      -1.
 */
uint32_t elf32GetDSymIndex(const Elf32File *elf32, const char *name);


/***
 *  Function returns an amount of segments in the binary file.
 * Input:
 *  @elf32 - point to the targer Elf32File.
 * Output:
 *  Success:
 *      amount of segments in the binary file.
 *  Fail:
 *      -1.
 */
uint32_t elf32GetAmountSegment(const Elf32File *elf32);


/***
 * Input:
 *  sh_type - this member categorizes the section’s contents and semantics.
 * Output:
 *  Success:
 *      point to elf32 section header from Elf32File structer
 *  Fail:
 *      NULL point.
 */
Elf32Shdr *elf32GetSectByType(const Elf32File *elf32, const Elf32Word sh_type);


/***
 * Description:
 *  Function returns a descriptor of a section with @name.
 * Input:
 *  @elf32 - Elf32File structer with initialized fields: fd, elf32Header and
 *        sectionHeaders.
 *  @name - target section name.
 * Output:
 *  Success:
 *      point to elf32 section header from Elf32File structer
 *  Fail:
 *      NULL point.
 */
Elf32Shdr *elf32GetSectByName(const Elf32File *elf32, const char* name);
Elf32Shdr *elf32GetLastLoadableSect(const Elf32File *elf32);


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  elf32 - Elf32File structer with initialized field fd.
 *  sectionHeader - header of section is related with elf32 file.
 * Output:
 *  Success:
 *      point to elf32 section data.
 *  Fail:
 *      NULL point.
 * After:
 *  need to free memory
 */
void *elf32ReadSect(const Elf32File *elf32, const Elf32Shdr *sectionHeader);


/***
 * Description:
 *  Function returns an amount of sections in the binary file.
 */
uint32_t elf32GetAmountSect(const Elf32File *elf32);


/***
 * Description:
 *  Function returns name of the section @sect.
 * Input:
 *  @elf32 - point to the targer Elf32File.
 *  @sect - point to section descriptor.
 * Output:
 *  Success:
 *      point to name of section.
 *  Fail:
 *      NULL.
 */
const char* elf32GetSectName(const Elf32File *elf32, const Elf32Shdr *sect);


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
uint32_t elf32GetSectSize(const Elf32Shdr *elf32Sect);


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
uint32_t elf32GetSectVaddr(const Elf32Shdr *sect);


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
uint32_t elf32GetSectFileoff(const Elf32Shdr *sect);


/***
 * Description:
 *  Function returns a relocation number for data, that is located at @addr in
 *  section @sect.
 * Input:
 *  @elf32 - binary file descriptor.
 *  @sect - point to section descriptor.
 *  @addr - address of relocatable data.
 * Output:
 *  Success:
 *      relocation offset.
 *  Fail:
 *      NULL.
 */
uint32_t elf32GetRelocationForAddr(const Elf32File *elf32, const Elf32Shdr *sect, uint32_t addr);


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  elf32 - Elf32File structer with initialized field fd.
 *  func - name of function, that is nedded to hooked.
 *  hand - address of handler function.
 * Output:
 *  Success:
 *      Old relocation addr.
 *  Fail:
 *      NULL point.
 */
void *elf32Hook(const Elf32File *elf32, const char *func, const void *hand);


/***
 * Before:
 *  If you need a file position, you should to save it.
 * Input:
 *  elf32 - Elf32File structer with initialized field fd.
 *  func - name of function, that is nedded.
 * Output:
 *  Success:
 *      Relocation addr.
 *  Fail:
 *      NULL point.
 */
void *elf32GetRelocationDataAddr(const Elf32File *elf32, const char *func);

#endif /* _ELF32_PARSE_H */


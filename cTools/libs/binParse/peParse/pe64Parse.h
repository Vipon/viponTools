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

#ifndef __PE_64_PARSE
#define __PE_64_PARSE

#include "file.h"
#include <windows.h>

/***
 *  typedef struct _IMAGE_DOS_HEADER
 *  {
 *      WORD e_magic;       // Magic number
 *      WORD e_cblp;
 *      WORD e_cp;
 *      WORD e_crlc;
 *      WORD e_cparhdr;
 *      WORD e_minalloc;
 *      WORD e_maxalloc;
 *      WORD e_ss;
 *      WORD e_sp;
 *      WORD e_csum;
 *      WORD e_ip;
 *      WORD e_cs;
 *      WORD e_lfarlc;
 *      WORD e_ovno;
 *      WORD e_res[4];
 *      WORD e_oemid;
 *      WORD e_oeminfo;
 *      WORD e_res2[10];
 *      LONG e_lfanew;      // File address of new exe header
 *  } IMAGE_DOS_HEADER, *PIMAGE_DOS_HEADER;
 */
typedef IMAGE_DOS_HEADER DosHeader;

/***
 *  typedef struct _IMAGE_NT_HEADERS {
 *      DWORD                 Signature; // "PE\0\0"
 *      IMAGE_FILE_HEADER     FileHeader;
 *      IMAGE_OPTIONAL_HEADER OptionalHeader;
 *  } IMAGE_NT_HEADERS, *PIMAGE_NT_HEADERS;
 */
typedef IMAGE_NT_HEADERS64 NTHeader64;

/***
 *  typedef struct _IMAGE_FILE_HEADER {
 *      WORD  Machine;          // Machine type
 *      WORD  NumberOfSections; // Number of segments in real
 *      DWORD TimeDateStamp;    // Low 32 bit represents birth date
 *      DWORD PointerToSymbolTable; // Raw offset to a symbol table
 *      DWORD NumberOfSymbols;
 *      WORD  SizeOfOptionalHeader; // Zero for Object file
 *      WORD  Characteristics;  // What is file exactly
 *  } IMAGE_FILE_HEADER, *PIMAGE_FILE_HEADER;
 */
typedef IMAGE_FILE_HEADER FileHeader;

/***
 *  typedef struct _IMAGE_OPTIONAL_HEADER64 {
 *      WORD                 Magic;
 *      BYTE                 MajorLinkerVersion;
 *      BYTE                 MinorLinkerVersion;
 *      DWORD                SizeOfCode;
 *      DWORD                SizeOfInitializedData;
 *      DWORD                SizeOfUninitializedData;
 *      DWORD                AddressOfEntryPoint;
 *      DWORD                BaseOfCode;
 *      ULONGLONG            ImageBase; // Program virtual base address
 *      DWORD                SectionAlignment;
 *      DWORD                FileAlignment;
 *      WORD                 MajorOperatingSystemVersion;
 *      WORD                 MinorOperatingSystemVersion;
 *      WORD                 MajorImageVersion;
 *      WORD                 MinorImageVersion;
 *      WORD                 MajorSubsystemVersion;
 *      WORD                 MinorSubsystemVersion;
 *      DWORD                Win32VersionValue;
 *      DWORD                SizeOfImage;
 *      DWORD                SizeOfHeaders;
 *      DWORD                CheckSum;
 *      WORD                 Subsystem;
 *      WORD                 DllCharacteristics;
 *      ULONGLONG            SizeOfStackReserve;
 *      ULONGLONG            SizeOfStackCommit;
 *      ULONGLONG            SizeOfHeapReserve;
 *      ULONGLONG            SizeOfHeapCommit;
 *      DWORD                LoaderFlags;
 *      DWORD                NumberOfRvaAndSizes;
 *      IMAGE_DATA_DIRECTORY DataDirectory[IMAGE_NUMBEROF_DIRECTORY_ENTRIES];
 *  } IMAGE_OPTIONAL_HEADER64, *PIMAGE_OPTIONAL_HEADER64;
 */
typedef IMAGE_OPTIONAL_HEADER64 OptHeader64;

/***
 *  typedef struct _IMAGE_SECTION_HEADER {
 *      BYTE  Name[IMAGE_SIZEOF_SHORT_NAME];
 *      union {
 *          DWORD PhysicalAddress;
 *          DWORD VirtualSize;
 *      } Misc;
 *      DWORD VirtualAddress;
 *      DWORD SizeOfRawData;
 *      DWORD PointerToRawData;
 *      DWORD PointerToRelocations;
 *      DWORD PointerToLinenumbers;
 *      WORD  NumberOfRelocations;
 *      WORD  NumberOfLinenumbers;
 *      DWORD Characteristics;
 *  } IMAGE_SECTION_HEADER, *PIMAGE_SECTION_HEADER;
 */
typedef IMAGE_SECTION_HEADER PESection;

/***
 *  typedef struct _IMAGE_SYMBOL {
 *      union {
 *          BYTE ShortName[8];
 *          struct {
 *              DWORD Short;
 *              DWORD Long;
 *          } Name;
 *          DWORD LongName[2];
 *      } N;
 *      DWORD Value;
 *      SHORT SectionNumber;
 *      WORD Type;
 *      BYTE StorageClass;
 *      BYTE NumberOfAuxSymbols;
 *  } IMAGE_SYMBOL;
 */
typedef IMAGE_SYMBOL PESymbol;

/***
 *  typedef union _IMAGE_AUX_SYMBOL {
 *      struct {
 *          DWORD    TagIndex;                      // struct, union, or enum tag index
 *          union {
 *              struct {
 *                  WORD    Linenumber;             // declaration line number
 *                  WORD    Size;                   // size of struct, union, or enum
 *              } LnSz;
 *             DWORD    TotalSize;
 *          } Misc;
 *          union {
 *              struct {                            // if ISFCN, tag, or .bb
 *                  DWORD    PointerToLinenumber;
 *                  DWORD    PointerToNextFunction;
 *              } Function;
 *              struct {                            // if ISARY, up to 4 dimen.
 *                  WORD     Dimension[4];
 *              } Array;
 *          } FcnAry;
 *          WORD    TvIndex;                        // tv index
 *      } Sym;
 *      struct {
 *          BYTE    Name[IMAGE_SIZEOF_SYMBOL];
 *      } File;
 *      struct {
 *          DWORD   Length;             // Section length
 *          WORD    NumberOfRelocations;// Number of relocation entries
 *          WORD    NumberOfLinenumbers;// number of line numbers
 *          DWORD   CheckSum;           // The checksum for communal data.
 *          SHORT   Number;             // Number of section in section table (start from 1)
 *          BYTE    Selection;          // COMDAT selection number. This is applicable
 *                                      // if the section is a COMDAT section.
 *      } Section;
 *  } IMAGE_AUX_SYMBOL;
 *  typedef IMAGE_AUX_SYMBOL UNALIGNED *PIMAGE_AUX_SYMBOL;
 */
typedef IMAGE_AUX_SYMBOL PEAuxSymbol;

#define TEXT_SECT_NAME ".text"

#define IS_PE64_FILE_EXEC(pe) \
    (pe->fileHeader->Characteristics && IMAGE_FILE_EXECUTABLE_IMAGE)

#define IS_PE64_FILE_SHARED(pe) \
    (pe->fileHeader->Characteristics && IMAGE_FILE_DLL)

#define IS_PE64_FILE_OBJ(pe) \
    (pe->fileHeader->SizeOfOptionalHeader == 0)

typedef enum {
    PE64_EXEC = 0,
    PE64_SHARED,
    PE64_OBJ
} PE64_FILE_TYPE;

typedef struct {
    FileD          fd;
    char           *fn;
    PE64_FILE_TYPE type;
    DosHeader      *dosHeader;  // Legacy stub
    NTHeader64     *ntHeader;   // Contain File and Opt header
    FileHeader     *fileHeader; // Description of file
    OptHeader64    *optHeader;  // Actual very necessary
    WORD           sectNum;     // Sections number
    PESection      *sections;   // Sections table
    PESymbol       *symtab;     // Symbol table
    PESymbol       *sortSymtab; // Sorted symbol table
    DWORD          symNum;      // Number of symbols in symtab
    char           *strtab;     // String table
} PE64File;

#ifdef __WIN__
typedef enum : uint64_t {
#else
typedef enum {
#endif /* __WIN__ */
    PE64_NO_RELOCATION = (uint64_t)-18,
    PE64_NO_OBJ,
    PE64_NO_SECTION,
    PE64_NO_SYMBOL,
    PE64_NO_RELADYN,
    PE64_NO_RELAPLT,
    PE64_NO_DYN_SYM_NAME_TAB,
    PE64_NO_STR_TAB,
    PE64_NO_DYNSYM,
    PE64_NO_SYMTAB,
    PE64_NO_SEGMENTS,
    PE64_NO_SECTIONS,
    PE64_NO_TYPE,
    PE64_INV_MACHINE_TYPE,
    PE64_NO_NT_HEADER,
    PE64_NO_DOS_HEADER,
    PE64_NO_MEM,
    PE64_INV_ARG,
    PE64_OK = 0
} PE64_ERROR;

static_assert(sizeof(PE64_ERROR) == 8, "PE64_ERROR must be 64 bit");
static_assert(((int64_t)PE64_INV_ARG) < 0, "ERRORS must be negative");

PE64File *pe64Parse(const char *fn);
void pe64Free(PE64File *pe);

PE64_ERROR pe64Check(const PE64File *pe);

/***
 * List of Machine_ID (not all):
 * IMAGE_FILE_MACHINE_UNKNOWN   0x0     To any machine type
 * IMAGE_FILE_MACHINE_AMD64     0x8664  x64
 * IMAGE_FILE_MACHINE_ARM       0x1c0   ARM little endian
 * IMAGE_FILE_MACHINE_ARM64     0xaa64  ARM64 little endian
 * IMAGE_FILE_MACHINE_ARMNT     0x1c4   ARM Thumb-2 little endian
 * IMAGE_FILE_MACHINE_I386      0x14c   Intel 386 or later processors
 * IMAGE_FILE_MACHINE_IA64      0x200   Intel Itanium processor family
 */
uint64_t pe64GetMachineID(const PE64File *pe);

PESection *pe64GetSectByAddr(const PE64File *pe);
PESection *pe64GetSectByNum(const PE64File *pe, uint64_t sectNum);

const char *pe64GetShortSectName(const PE64File *pe, const PESection *sect);
const char *pe64GetLongSectName(const PE64File *pe, const PESection *sect);
const char *pe64GetSectName(const PE64File *pe, const PESection *sect);
uint64_t pe64GetSectAddr(const PESection *sect);
uint64_t pe64GetSectFileoff(const PESection *sect);
uint64_t pe64GetSectEndFileoff(const PESection *sect);

PESymbol *pe64GetSSymTab(const PE64File *pe);
PESymbol *pe64GetSSymSortTab(const PE64File *pe);
PESymbol *pe64GetSymByName(const PE64File *pe, const char *name);

/***
 * Description:
 *  Function for work with qsort. Function compares addresses of symbols and
 * Output:
 *  1 - if (a->addr > b->addr)
 *  -1 - if (a->addr < b->addr)
 *  0 - if (a->addr == b->addr)
 */
int pe64CmpSym(const void *a, const void *b);

const char*pe64GetLongSymName(const PE64File *pe, const PESymbol *sym);
const char*pe64GetShortSymName(const PE64File *pe, const PESymbol *sym);
const char*pe64GetSymName(const PE64File *pe, const PESymbol *sym);

uint64_t pe64GetAmountSSym(const PE64File *pe);
uint64_t pe64GetSSymAddr(const PESymbol *sym);
uint64_t pe64GetAddrSymByName(const PE64File *pe, const char *name);

#endif /* __PE_64_PARSE */


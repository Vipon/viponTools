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

#ifndef __PE_64_PARSE
#define __PE_64_PARSE

// ViponTools headers
#include "file.h"
#include "arch.h"

// Windows standard headres
#include <Windows.h>
#include <delayimp.h>

// C standard headers
#include <assert.h>

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
 *      DWORD                SizeOfCode; // Size of all code sections
 *      DWORD                SizeOfInitializedData; // Size of all initialized data sections
 *      DWORD                SizeOfUninitializedData; // Size of all uninitialized data sections
 *      DWORD                AddressOfEntryPoint; // Ptr to the entry point func, relative to the image base addr
 *      DWORD                BaseOfCode; // Start ptr of the code sect, relative to the image base
 *      ULONGLONG            ImageBase; // Program virtual base address
 *      DWORD                SectionAlignment; // Sect alignment in memory
 *      DWORD                FileAlignment; // Raw data alignment of sect in the file
 *      WORD                 MajorOperatingSystemVersion;
 *      WORD                 MinorOperatingSystemVersion;
 *      WORD                 MajorImageVersion;
 *      WORD                 MinorImageVersion;
 *      WORD                 MajorSubsystemVersion;
 *      WORD                 MinorSubsystemVersion;
 *      DWORD                Win32VersionValue; // reserved and must be 0
 *      DWORD                SizeOfImage; // Image size - multiple of SectionAlignment
 *      DWORD                SizeOfHeaders;
 *      DWORD                CheckSum;
 *      WORD                 Subsystem; // Subsystem required to run this image
 *      WORD                 DllCharacteristics;
 *      ULONGLONG            SizeOfStackReserve;
 *      ULONGLONG            SizeOfStackCommit;
 *      ULONGLONG            SizeOfHeapReserve;
 *      ULONGLONG            SizeOfHeapCommit;
 *      DWORD                LoaderFlags; // This member is obsolete
 *      DWORD                NumberOfRvaAndSizes;
 *      IMAGE_DATA_DIRECTORY DataDirectory[IMAGE_NUMBEROF_DIRECTORY_ENTRIES];
 *  } IMAGE_OPTIONAL_HEADER64, *PIMAGE_OPTIONAL_HEADER64;
 */
typedef IMAGE_OPTIONAL_HEADER64 OptHeader64;

/***
 *  typedef struct _IMAGE_DATA_DIRECTORY {
 *      DWORD VirtualAddress;
 *      DWORD Size;
 *  } IMAGE_DATA_DIRECTORY, *PIMAGE_DATA_DIRECTORY;
 */
typedef IMAGE_DATA_DIRECTORY DataDir;

/***
 *  typedef struct _IMAGE_IMPORT_DESCRIPTOR {
 *      union {
 *          DWORD Characteristics;    // 0 for terminating null import descriptor
 *          DWORD OriginalFirstThunk; // RVA to original unbound IAT
 *      };
 *      DWORD TimeDateStamp;          // 0 if not bound,
 *                                    // -1 if bound, and real date\time stamp
 *                                    // in IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT (new)
 *                                    // O.W. date/time stamp of DLL bound to (old)
 *      DWORD ForwarderChain;         // -1 if no forwarders
 *      DWORD Name;
 *      DWORD FirstThunk;             // RVA to IAT
 *  } IMAGE_IMPORT_DESCRIPTOR;
 */
typedef IMAGE_IMPORT_DESCRIPTOR PEImport;

/***
 * WORD Hint
 * Name
 */
typedef IMAGE_IMPORT_BY_NAME ImportByName;

/***
 *  typedef struct _IMAGE_THUNK_DATA {
 *      union {
 *          ULONGLONG Function;        // address of imported function
 *          ULONGLONG Ordinal;         // ordinal value of function
 *          ULONGLONG AddressOfData;   // RVA of imported name
 *          ULONGLONG ForwarderStringl // RVA to forwarder string
 *      } u1;
 *  } IMAGE_THUNK_DATA64, *PIMAGE_THUNK_DATA64;
 */
typedef IMAGE_THUNK_DATA64 ThunkData64;

/***
 *  typedef struct ImgDelayDescr {
 *      DWORD           grAttrs;        // attributes
 *      RVA             rvaDLLName;     // RVA to dll name
 *      RVA             rvaHmod;        // RVA of module handle
 *      RVA             rvaIAT;         // RVA of the IAT
 *      RVA             rvaINT;         // RVA of the INT
 *      RVA             rvaBoundIAT;    // RVA of the optional bound IAT
 *      RVA             rvaUnloadIAT;   // RVA of optional copy of original IAT
 *      DWORD           dwTimeStamp;    // 0 if not bound,
 *                                      // O.W. date/time stamp of DLL bound to (Old BIND)
 *  } ImgDelayDescr, * PImgDelayDescr;
 */
typedef ImgDelayDescr PEDelimp;

/***
 *  typedef struct _IMAGE_EXPORT_DIRECTORY {
 *      DWORD   Characteristics;
 *      DWORD   TimeDateStamp;
 *      WORD    MajorVersion;
 *      WORD    MinorVersion;
 *      DWORD   Name;
 *      DWORD   Base;
 *      DWORD   NumberOfFunctions;
 *      DWORD   NumberOfNames;
 *      DWORD   AddressOfFunctions;
 *      DWORD   AddressOfNames;
 *      DWORD   AddressOfNameOrdinals;
 *  } IMAGE_EXPORT_DIRECTORY,*PIMAGE_EXPORT_DIRECTORY;
*/
typedef IMAGE_EXPORT_DIRECTORY PEExport;

/***
 *  typedef struct _IMAGE_SECTION_HEADER {
 *      BYTE  Name[IMAGE_SIZEOF_SHORT_NAME];
 *      union {
 *          DWORD PhysicalAddress;
 *          DWORD VirtualSize;
 *      } Misc;
 *      DWORD VirtualAddress;               // vaddr for exec, 0 for obj
 *      DWORD SizeOfRawData;                // sect size in file with alignment
 *      DWORD PointerToRawData;             // file offset
 *      DWORD PointerToRelocations;
 *      DWORD PointerToLinenumbers;
 *      WORD  NumberOfRelocations;
 *      WORD  NumberOfLinenumbers;
 *      DWORD Characteristics;
 *  } IMAGE_SECTION_HEADER, *PIMAGE_SECTION_HEADER;
 */
typedef IMAGE_SECTION_HEADER PESection;
typedef PESection PESegment;

/***
 *  typedef struct _IMAGE_SYMBOL {
 *      union {
 *          BYTE ShortName[8];  // Short name
 *          struct {
 *              DWORD Short;    // if Short == 0
 *              DWORD Long;     // Long - offset in strtab
 *          } Name;
 *          DWORD LongName[2];
 *      } N;
 *      DWORD Value;
 *      SHORT SectionNumber;    // Section index
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

typedef IMAGE_RELOCATION COFFReloc;
typedef IMAGE_BASE_RELOCATION PEBaseReloc;

#define TEXT_SECT_NAME ".text"

#define IS_PE64_FILE_EXEC(pe) \
    (pe->fileHeader->Characteristics && IMAGE_FILE_EXECUTABLE_IMAGE)

#define IS_PE64_FILE_SHARED(pe) \
    (pe->fileHeader->Characteristics && IMAGE_FILE_DLL)

#define IS_PE64_FILE_OBJ(pe) \
    (pe->fileHeader->SizeOfOptionalHeader == 0)

#define IS_PE64_SECT_LOADABLE(sect) \
    (  (sect->Characteristics & IMAGE_SCN_MEM_EXECUTE) \
    || (sect->Characteristics & IMAGE_SCN_MEM_READ)    \
    || (sect->Characteristics & IMAGE_SCN_MEM_WRITE)   \
    )

typedef enum {
    PE64_EXEC = 0,
    PE64_SHARED,
    PE64_OBJ
} PE64_FILE_TYPE;

typedef struct {
    FileD          fd;
    char           *fn;
    size_t         fs;
    uint8_t        *faddr;
    Arch           arch;
    PE64_FILE_TYPE type;
    DosHeader      *dosHeader;  // Legacy stub
    NTHeader64     *ntHeader;   // Contain File and Opt header
    FileHeader     *fileHeader; // Description of file
    OptHeader64    *optHeader;  // Actual very necessary
    WORD           sectNum;     // Sections number
    PESection      *sections;   // Sections table
    PEImport       *import;     // Import tab
    uint64_t       importNum;   // Number of imports
    PEDelimp       *delimp;     // Delay import tab
    uint64_t       delimpNum;   // Number of delay imports
    PEExport       *exp;        // Export table
    uint64_t       expNum;      // Number of exports
    PESymbol       *symtab;     // Symbol table
    uint64_t       symNum;      // Number of symbols in symtab
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
    PE64_NO_STR_TAB,
    PE64_NO_IMPORTS,
    PE64_NO_DELIMP,
    PE64_NO_EXPORTS,
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

/***
 * @brief Parse PE64File binary file
 *
 * @param[in] fn Binary file name. C string with terminal null
 *
 * @return Pointer to PE64File structer or NULL if fail
 */
EXPORT_FUNC
PE64File *pe64Parse(const char *fn);

/***
 * @warning You must complete all jobs with this pe, otherwise you will free
 *        all information about this file including sections, symbols etc
 * @param[in] pe pointer to PE64File structer
 */
EXPORT_FUNC
void pe64Free(PE64File *pe);

/***
 * @brief Check if pe points to a valid structure
 *
 * @param[in] pe pointer to PE64File structure to check
 *
 * @return PE64_OK or number of error (see PE64_ERROR)
 */
EXPORT_FUNC
PE64_ERROR pe64Check(const PE64File *pe);

/***
 * @brief find out file position with specified virtual addr
 *
 * @param[in] pe PE64File pointer
 * @param[in] addr interested addr
 *
 * @return file offset or -1
*/
EXPORT_FUNC
uint64_t pe64AddrToFileOff(const PE64File *pe, uint64_t addr);

/***
 * @brief get target machine specifier
 *        List of Machine_ID (not all):
 *        IMAGE_FILE_MACHINE_UNKNOWN   0x0     To any machine type
 *        IMAGE_FILE_MACHINE_AMD64     0x8664  x64
 *        IMAGE_FILE_MACHINE_ARM       0x1c0   ARM little endian
 *        IMAGE_FILE_MACHINE_ARM64     0xaa64  ARM64 little endian
 *        IMAGE_FILE_MACHINE_ARMNT     0x1c4   ARM Thumb-2 little endian
 *        IMAGE_FILE_MACHINE_I386      0x14c   Intel 386 or later processors
 *        IMAGE_FILE_MACHINE_IA64      0x200   Intel Itanium processor family
 *
 * @param[in] pe PE64File pointer
 */
EXPORT_FUNC
uint64_t pe64GetMachineID(const PE64File *pe);

/***
 * @brief returns a descriptor of a section contains addr
 *
 * @param[in] pe pointer to the target PE64File
 * @param[in] addr address without ASLR
 *
 * @return pointer to a section of NULL
 */
EXPORT_FUNC
PESection *pe64GetSectByAddr(const PE64File *pe, uint64_t addr);

/***
 * @brief returns a descriptor with index
 *
 * @param[in] pe pointer to the target PE64File
 * @param[in] sectNum section number
 *            section enumeration starts with 1
 *
 * @return pointer to a section of NULL
 */
EXPORT_FUNC
PESection *pe64GetSectByIndx(const PE64File *pe, uint64_t sectNum);

/***
 * @brief returns a descriptor of a section with name
 *
 * @param[in] pe pointer to the target PE64File
 * @param[in] name section name
 *
 * @return pointer to a section of NULL
 */
EXPORT_FUNC
PESection *pe64GetSectByName(const PE64File *pe, const char *name);

/***
 * @brief returns last loadable section in binary file
 *
 * @param[in] pe pointer to the target PE64File
 *
 * @return pointer to a section of NULL
 */
EXPORT_FUNC
PESection *pe64GetLastLoadableSect(const PE64File *pe);

/***
 * @brief Reads section contents
 *
 * @param[in] pe pointer to the target PE64File
 * @param[in] sect section descriptor
 *
 * @return pointer to a content or NULL
 * @warning need to free memory
 */
EXPORT_FUNC
void *pe64ReadSect(const PE64File *pe, const PESection *sect);

/***
 * @brief returns an amount of sections
 *
 * @param[in] pe pointer to the target PE64File
 *
 * @return amount of sections
 */
EXPORT_FUNC
uint64_t pe64GetAmountSect(const PE64File *pe);

/***
 * @brief returns name of the section
 *
 * @param[in] pe pointer to the target PE64File
 * @param[in] sect section descriptor
 *
 * @return pointer of NULL
 */
EXPORT_FUNC
const char *pe64GetSectName(const PE64File *pe, const PESection *sect);
EXPORT_FUNC
const char *pe64GetShortSectName(const PE64File *pe, const PESection *sect);
EXPORT_FUNC
const char *pe64GetLongSectName(const PE64File *pe, const PESection *sect);

/***
 * @brief returns a virtual address of a section start
 *
 * @param[in] sect pointer to section descriptor
 *
 * @return addr or -1
 */
EXPORT_FUNC
uint64_t pe64GetSectAddr(const PESection *sect);

/***
 * @brief returns a virtual address of a section end
 *
 * @param[in] sect pointer to section descriptor
 *
 * @return addr or -1
 */
EXPORT_FUNC
uint64_t pe64GetSectVend(const PE64File *pe, const PESection *sect);

/***
 * @brief returns a size of a section
 *
 * @param[in] sect pointer to section descriptor
 *
 * @return section size or -1
 */
EXPORT_FUNC
uint64_t pe64GetSectSize(const PESection *sect);

/***
 * @brief returns a file offset of a section.
 *
 * @param[in] sect pointer to section descriptor.
 *
 * @return file offset to a section start
 */
EXPORT_FUNC
uint64_t pe64GetSectFileoff(const PESection *sect);

/***
 * @brief returns a file offset of the end of section.
 *
 * @param[in] sect pointer to section descriptor.
 *
 * @return file offset to a section start
 */
EXPORT_FUNC
uint64_t pe64GetSectEndFileoff(const PESection *sect);

/***
 * @brief Function returns pointer to the static symbols table
 *
 * @param[in] pe pointer to the target PE64File
 *
 * @return pointer to static symbol table or NULL if fail
 */
EXPORT_FUNC
PESymbol *pe64GetSSymTab(const PE64File *pe);

/***
 * @brief Function returns pointer to the symbol with name
 *
 * @param[in] pe pointer to PE64File structer
 * @param[in] name symbol name needed to find
 *
 * @return pointer to PESymbol or NULL if fail
 */
EXPORT_FUNC
PESymbol *pe64GetSymByName(const PE64File *pe, const char *name);

/***
 * @brief Function for work with qsort. Functions compare addresses of symbols
 *        and returns 1/-1/0 if a->addr >/</== b->addr.
 *
 * @param[in] a pointer to a fist symbol
 * @param[in] b pointer to a second symbol
 */
EXPORT_FUNC
int pe64CmpSym(const void *a, const void *b);

/***
 * @brief Function returns name of the symbol
 *
 * @param[in] pe pointer to the target PE64File
 * @param[in] sym pointer to symbol structure
 *
 * @return pointer to name of symbol or NULL if fail
 */
EXPORT_FUNC
const char*pe64GetSymName(const PE64File *pe, const PESymbol *sym);
EXPORT_FUNC
const char*pe64GetLongSymName(const PE64File *pe, const PESymbol *sym);
EXPORT_FUNC
const char*pe64GetShortSymName(const PE64File *pe, const PESymbol *sym);

/***
 * @brief Function returns amount of static symbols
 *
 * @param[in] pe pointer to the target PE64File
 *
 * @return amount of static symbols or -1
 */
EXPORT_FUNC
uint64_t pe64GetAmountSSym(const PE64File *pe);

/***
 * @brief returns addr of static symbol without ASLR
 *
 * @param[in] sym pointer to the target static PESymbol
 *
 * @return address of -1
 */
EXPORT_FUNC
uint64_t pe64GetSSymAddr(const PESymbol *sym);

/***
 * @brief return section number where symbol placed
 *
 * @param[in] sym pointer to the target static PESymbol
 *
 * @return section number of -1
 */
EXPORT_FUNC
uint64_t pe64GetSSymSectIndx(const PESymbol *sym);

/***
 * @brief returns addr of static symbol with name without ASLR
 *
 * @param[in] pe PE file descriptor
 * @param[in] name mane of symbol
 *
 * @return address of -1
 */
EXPORT_FUNC
uint64_t pe64GetAddrSymByName(const PE64File *pe, const char *name);

/***
 * @brief returns file position for the static symbol
 *
 * @param[in] pe pointer to the target PE64File
 * @param[in] sym pointer to symbol
 *
 * @return file offset to start of symbol or -1
 */
EXPORT_FUNC
uint64_t pe64GetSSymFileoff(const PE64File *pe, const PESymbol *sym);

/***
 * @brief returns size of static symbol
 *
 * @param[in] pe pointer to PE64File structer
 * @param[in] sym pointer to the target static PESymbol
 *
 * @return size of static symbol or -1
 */
EXPORT_FUNC
uint64_t pe64GetSSymSize(const PE64File *pe, const PESymbol *sym);

/***
 * @brief returns an amount of segments
 *
 * @param[in] pe pointer to the target PE64File
 *
 * @return amount of segments
 */
EXPORT_FUNC
uint64_t pe64GetAmountSeg(const PE64File *pe);

#endif /* __PE_64_PARSE */


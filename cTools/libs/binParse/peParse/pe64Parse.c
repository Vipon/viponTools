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

#include "mem.h"
#include "bits.h"
#include "file.h"
#include "comdef.h"
#include "pe64Parse.h"

static PE64_ERROR pe64ParseDosHeader(PE64File *pe)
{
    if (pe == NULL || IS_INV_FD(pe->fd))
        return PE64_INV_ARG;

    FileD fd = pe->fd;
    uint64_t off = 0;
    uint64_t hSize = sizeof(DosHeader);
    DosHeader *header = (DosHeader*) readFromFile(fd, &off, hSize);
    if (header == NULL)
        return PE64_NO_MEM;

    unsigned char *e_magic = (unsigned char *)&header->e_magic;
    if (e_magic[0] == 'M' && e_magic[1] == 'Z') {
        pe->dosHeader = header;
        return PE64_OK;
    } else {
        Free(header);
        return PE64_NO_DOS_HEADER;
    }
}

static PE64_ERROR pe64ParseType(PE64File *pe)
{
    if (pe == NULL || pe->ntHeader == NULL)
        return PE64_INV_ARG;

    if (IS_PE64_FILE_EXEC(pe)) {
        pe->type = PE64_EXEC;
    } else if (IS_PE64_FILE_SHARED(pe)) {
        pe->type = PE64_SHARED;
    } else if (IS_PE64_FILE_OBJ(pe)) {
        pe->type = PE64_OBJ;
    } else {
        return PE64_NO_TYPE;
    }

    return PE64_OK;
}

static PE64_ERROR pe64ParseFileHeader(PE64File *pe)
{
    if (pe == NULL)
        return PE64_INV_ARG;

    FileD fd = pe->fd;
    uint64_t off = 0;
    uint64_t size = sizeof(FileHeader);
    FileHeader *fileHeader = (FileHeader*) readFromFile(fd, &off, size);
    if (fileHeader == NULL)
        return PE64_NO_MEM;

    pe->fileHeader = fileHeader;
    return PE64_OK;
}

static PE64_ERROR pe64ParseNTHeader(PE64File *pe)
{
    if (pe == NULL || pe->dosHeader == NULL)
        return PE64_INV_ARG;

    FileD fd = pe->fd;
    uint64_t off = (uint64_t)pe->dosHeader->e_lfanew;
    uint64_t hSize = sizeof(NTHeader64);
    NTHeader64 *header = (NTHeader64*) readFromFile(fd, &off, hSize);
    if (header == NULL)
        return PE64_NO_MEM;

    char *sig = (char*)&header->Signature;
    if (sig[0] == 'P' && sig[1] == 'E' && sig[2] == '\0' && sig[3] == '\0') {
        pe->ntHeader = header;
        pe->fileHeader = &header->FileHeader;
        pe->optHeader = &header->OptionalHeader;
        pe64ParseType(pe);
        return PE64_OK;
    } else {
        Free(header);
        return PE64_NO_NT_HEADER;
    }
}

static PE64_ERROR pe64ParseSections(PE64File *pe)
{
    if (pe == NULL || pe->fileHeader == NULL)
        return PE64_INV_ARG;

    FileD fd = pe->fd;

    uint64_t off = 0;
    uint64_t size = pe->fileHeader->NumberOfSections * sizeof(PESection);
    if (IS_PE64_FILE_EXEC(pe) || IS_PE64_FILE_SHARED(pe))
        off = (uint64_t)pe->dosHeader->e_lfanew + sizeof(NTHeader64);
    else if (IS_PE64_FILE_OBJ(pe))
        off = sizeof(FileHeader);
    else
        return PE64_NO_SECTIONS;

    PESection *sections = readFromFile(fd, &off, size);
    if (sections == NULL)
        return PE64_NO_SECTIONS;

    pe->sections = sections;
    pe->sectNum = pe->fileHeader->NumberOfSections;

    return PE64_OK;
}

static PE64_ERROR pe64ParseImport(PE64File *pe)
{
    if (pe == NULL || pe->optHeader== NULL)
        return PE64_INV_ARG;


    DataDir *importDir = pe->optHeader->DataDirectory + IMAGE_DIRECTORY_ENTRY_IMPORT;
    uint64_t importAddr = importDir->VirtualAddress;

    FileD fd = pe->fd;
    uint64_t size = importDir->Size;
    if (size == 0)
        return PE64_NO_IMPORTS;

    uint64_t off = pe64AddrToFileOff(pe, importAddr);
    PEImport *import = readFromFile(fd, &off, size);
    if (import == NULL)
        return PE64_NO_MEM;

    pe->import = import;
    pe->importNum = size / sizeof(PEImport);
    return PE64_OK;
}

static PE64_ERROR pe64ParseSymtab(PE64File *pe)
{
    if (pe == NULL || pe->fileHeader == NULL)
        return PE64_INV_ARG;

    if (IS_PE64_FILE_EXEC(pe) || IS_PE64_FILE_SHARED(pe)) {
        pe->symtab = NULL;
        pe->symNum = 0;
        return PE64_NO_SYMTAB;
    }

    FileD fd = pe->fd;
    uint64_t off = pe->fileHeader->PointerToSymbolTable;
    if (off == 0)
        return PE64_NO_SYMTAB;

    uint64_t num = pe->fileHeader->NumberOfSymbols;
    uint64_t size = num * sizeof(PESymbol);
    PESymbol *symtab = readFromFile(fd, &off, size);
    if (symtab == NULL)
        return PE64_NO_SYMTAB;

    pe->symtab = symtab;
    pe->symNum = num;

    return PE64_OK;
}

static PE64_ERROR pe64ParseSortSymTab(PE64File *pe)
{
    if (pe == NULL)
        return PE64_INV_ARG;

    PESymbol *symtab = pe64GetSSymTab(pe);
    uint64_t num = pe64GetAmountSSym(pe);
    uint64_t size = num * sizeof(PESymbol);
    PESymbol *sortTab = malloc(size);
    if (sortTab == NULL)
        return PE64_NO_MEM;

    memcpy(sortTab, symtab, size);
    qsort(sortTab, num, sizeof(PESymbol), pe64CmpSym);

    pe->sortSymtab = sortTab;
    return PE64_OK;
}

static PE64_ERROR pe64ParseStrtab(PE64File *pe)
{
    if (pe == NULL || pe->fileHeader == NULL)
        return PE64_INV_ARG;

    if (IS_PE64_FILE_EXEC(pe) || IS_PE64_FILE_SHARED(pe)) {
        pe->strtab = NULL;
        return PE64_NO_SYMTAB;
    }

    // COFF String Table
    // Immediately following the COFF symbol table is the COFF string table.

    // At the beginning of the COFF string table are 4 bytes that contain the
    // total size (in bytes) of the rest of the string table. This size includes
    // the size field itself, so that the value in this location would be 4
    // if no strings were present.
    FileD fd = pe->fd;
    uint64_t symOff = pe->fileHeader->PointerToSymbolTable;
    uint64_t symNum = pe->fileHeader->NumberOfSymbols;
    uint64_t strtabOff = symOff + symNum * sizeof(PESymbol);
    uint32_t *strtabSize = readFromFile(fd, &strtabOff, sizeof(uint32_t));
    if (strtabSize == NULL)
        return PE64_NO_MEM;

    char *strtab = readFromFile(fd, &strtabOff, *strtabSize);
    if (strtab == NULL)
        return PE64_NO_MEM;

    pe->strtab = strtab;

    Free(strtabSize);
    return PE64_OK;
}

static PE64_ERROR pe64ParseMaybeObj(PE64File *pe)
{
    if (pe == NULL)
        return PE64_INV_ARG;

    PE64File peCopy = *pe;
    PE64_ERROR res = pe64ParseFileHeader(&peCopy);
    if (res != PE64_OK)
        return res;

    switch (pe64GetMachineID(&peCopy)) {
    case IMAGE_FILE_MACHINE_UNKNOWN:
    case IMAGE_FILE_MACHINE_AMD64:
    case IMAGE_FILE_MACHINE_ARM:
    case IMAGE_FILE_MACHINE_ARM64:
    case IMAGE_FILE_MACHINE_ARMNT:
    case IMAGE_FILE_MACHINE_I386:
    case IMAGE_FILE_MACHINE_IA64:
    case IMAGE_FILE_MACHINE_R3000:
    case IMAGE_FILE_MACHINE_R4000:
    case IMAGE_FILE_MACHINE_R10000:
    case IMAGE_FILE_MACHINE_WCEMIPSV2:
    case IMAGE_FILE_MACHINE_ALPHA:
    case IMAGE_FILE_MACHINE_SH3:
    case IMAGE_FILE_MACHINE_SH3DSP:
    case IMAGE_FILE_MACHINE_SH3E:
    case IMAGE_FILE_MACHINE_SH4:
    case IMAGE_FILE_MACHINE_SH5:
    case IMAGE_FILE_MACHINE_THUMB:
    case IMAGE_FILE_MACHINE_AM33:
    case IMAGE_FILE_MACHINE_POWERPC:
    case IMAGE_FILE_MACHINE_POWERPCFP:
    case IMAGE_FILE_MACHINE_MIPS16:
    case IMAGE_FILE_MACHINE_ALPHA64:
    case IMAGE_FILE_MACHINE_MIPSFPU:
    case IMAGE_FILE_MACHINE_MIPSFPU16:
    case IMAGE_FILE_MACHINE_TRICORE:
    case IMAGE_FILE_MACHINE_CEF:
    case IMAGE_FILE_MACHINE_EBC:
    case IMAGE_FILE_MACHINE_M32R:
    case IMAGE_FILE_MACHINE_CEE:
        break;
    default:
        return PE64_INV_MACHINE_TYPE;
    }

    uint64_t off = sizeof (FileHeader);
    char *firstSectName = readFromFile(peCopy.fd, &off, sizeof(TEXT_SECT_NAME) + 1);
    if (firstSectName == NULL)
        return PE64_NO_MEM;

    if (strcmp(firstSectName, TEXT_SECT_NAME)) {
        Free(firstSectName);
        return PE64_NO_OBJ;
    }

    Free(firstSectName);
    pe->fileHeader = peCopy.fileHeader;
    pe->type = PE64_OBJ;
    return PE64_OK;
}

PE64File *pe64Parse(const char *fn)
{
    if (fn == NULL) {
        ERROR("Invalid arguments");
        return NULL;
    }

    FileD fd = open(fn, O_RDONLY);
    if (IS_INV_FD(fd)) {
        PERROR("open()");
        return NULL;
    }

    PE64File *pe = (PE64File*) Calloc(1, sizeof(PE64File));
    if (pe == NULL) {
        ERROR("Cannot allocate %zu bytes", sizeof(PE64File));
        goto eexit_0;
    }

    pe->fd = fd;
    uint64_t nameSize = strlen(fn) * sizeof(char);
    if ((pe->fn = (char*) Calloc(nameSize, sizeof(char))) == NULL) {
        ERROR("Cannot allocate %zu bytes", nameSize);
        goto eexit_1;
    }

    strncpy(pe->fn, fn, nameSize);

    if (pe64ParseMaybeObj(pe)) {
        if (pe64ParseDosHeader(pe)) {
            ERROR("Cannot parse dos header");
            goto eexit_1;
        }

        if (pe64ParseNTHeader(pe)) {
            ERROR("Cannot parse nt header");
            goto eexit_1;
        }
    }

    if (pe64ParseSections(pe)) {
        ERROR("Cannot parse sections");
        goto eexit_1;
    }

    // File can be without imports
    pe64ParseImport(pe);

    if (pe64ParseSymtab(pe)) {
        if (IS_PE64_FILE_OBJ(pe)) {
            ERROR("Cannot parse symbol table");
            goto eexit_1;
        }
    } else if (pe64ParseSortSymTab(pe)) {
        ERROR("Cannot parse sorted symbol table");
        goto eexit_1;
    }

    if (pe64ParseStrtab(pe)) {
        if (IS_PE64_FILE_OBJ(pe)) {
            ERROR("Cannot parse string table");
            goto eexit_1;
        }
    }

    return pe;

eexit_1:
    pe64Free(pe);
    return NULL;
eexit_0:
    close(fd);
    return NULL;
}

void pe64Free(PE64File *pe)
{
    if (pe == NULL)
        return;

    Free(pe->fn);
    close(pe->fd);

    if (IS_PE64_FILE_EXEC(pe) || IS_PE64_FILE_SHARED(pe)) {
        Free(pe->dosHeader);
        Free(pe->ntHeader);
    } else if (IS_PE64_FILE_OBJ(pe)) {
        Free(pe->fileHeader);
    }

    Free(pe->sections);
    Free(pe->symtab);
    Free(pe->strtab);
}

uint64_t pe64AddrToFileOff(const PE64File *pe, uint64_t addr)
{
    if (pe == NULL || pe->sections == NULL)
        return PE64_INV_ARG;

    PESection *sect = pe64GetSectByAddr(pe, addr);
    uint64_t sectAddr = pe64GetSectAddr(sect);
    uint64_t sectFileOff = pe64GetSectFileoff(sect);

    return (addr - sectAddr + sectFileOff);
}

uint64_t pe64GetMachineID(const PE64File *pe)
{
    if (pe == NULL)
        return PE64_INV_ARG;

    return pe->fileHeader->Machine;
}

PESection *pe64GetSectByAddr(const PE64File *pe, uint64_t addr)
{
    if (pe == NULL)
        return NULL;

    WORD i = 0;
    for (i = 0; i < pe->sectNum; ++i) {
        PESection *sect = pe->sections + i;
        uint64_t start = pe64GetSectAddr(sect);
        uint64_t end = start + pe64GetSectSize(sect);
        if (addr >= start && addr <= end)
            return sect;
    }

    return NULL;
}

PESection *pe64GetSectByIndx(const PE64File *pe, uint64_t sectNum)
{
    if (pe == NULL)
        return NULL;

    if (sectNum > 0 && sectNum <= pe->sectNum)
        return &pe->sections[sectNum - 1]; // sections enumerate from 1, not 0

    return NULL;
}

PESection *pe64GetSectByName(const PE64File *pe, const char *name)
{
    if (pe == NULL || name == NULL)
        return NULL;

    uint64_t i = 0;
    uint64_t num = pe64GetAmountSect(pe);
    for (i = 0; i < num; ++i) {
        PESection *sect = pe->sections + i;
        const char *curName = pe64GetSectName(pe, sect);
        if (strcmp(curName, name) == 0)
            return sect;
    }

    return NULL;
}

PESection *pe64GetLastLoadableSect(const PE64File *pe)
{
    if (pe == NULL)
        return NULL;

    int64_t i = (int64_t)pe64GetAmountSect(pe);
    for (; i >= 0; --i) {
        PESection *sect = pe->sections + i;
        if (IS_PE64_SECT_LOADABLE(sect))
            return sect;
    }

    return NULL;
}

void *pe64ReadSect(const PE64File *pe, const PESection *sect)
{
    if (pe == NULL || sect == NULL)
        return NULL;

    FileD fd = pe->fd;
    uint64_t size = pe64GetSectSize(sect);
    uint64_t fileOff = pe64GetSectFileoff(sect);
    return readFromFile(fd, (size_t*)&fileOff, size);
}

const char *pe64GetShortSectName(const PE64File *pe, const PESection *sect)
{
    if (pe == NULL || sect == NULL)
        return NULL;

    UNUSED(pe);
    return (const char *)sect->Name;
}

const char *pe64GetLongSectName(const PE64File *pe, const PESection *sect)
{
    if (pe == NULL || sect == NULL)
        return NULL;

    // If the string is exactly 8 characters long, there is no terminating null.
    // For longer names, this field contains a slash (/) that is followed by an
    // ASCII representation of a decimal number that is an offset into the
    // string table.
    const char *name = (const char*)&sect->Name[1];
    int64_t off = strntoi64(name, 7, NULL, 10);
    return (const char *)&pe->strtab[off];
}

const char *pe64GetSectName(const PE64File *pe, const PESection *sect)
{
    if (pe == NULL || sect == NULL)
        return NULL;

    switch (pe->type) {
    case PE64_EXEC:
    case PE64_SHARED:
        // For exec and shared all long name are truncated to 8 characters
        return pe64GetShortSectName(pe, sect);
    case PE64_OBJ:
        if (sect->Name[0] == '/')
            return pe64GetLongSectName(pe, sect);
        else
            return pe64GetShortSectName(pe, sect);
    default:
        ERROR("Unknown file type");
        return NULL;
    }
}

uint64_t pe64GetAmountSect(const PE64File *pe)
{
    if (pe == NULL)
        return PE64_INV_ARG;

    return pe->sectNum;
}

uint64_t pe64GetSectAddr(const PESection *sect)
{
    if (sect == NULL)
        return PE64_INV_ARG;

    return sect->VirtualAddress;
}

uint64_t pe64GetSectSize(const PESection *sect)
{
    if (sect == NULL)
        return PE64_INV_ARG;

    return sect->SizeOfRawData;
}

uint64_t pe64GetSectFileoff(const PESection *sect)
{
    if (sect == NULL)
        return PE64_INV_ARG;

    return sect->PointerToRawData;
}

uint64_t pe64GetSectEndFileoff(const PESection *sect)
{
    if (sect == NULL)
        return PE64_INV_ARG;

    return 0;
}

uint64_t pe64GetSectVend(const PE64File *pe, const PESection *sect)
{
    if (pe == NULL || sect == NULL)
        return PE64_INV_ARG;

    return 0;
}

PESymbol *pe64GetSSymTab(const PE64File *pe)
{
    if (pe == NULL)
        return NULL;

    return pe->symtab;
}

PESymbol *pe64GetSSymSortTab(const PE64File *pe)
{
    if (pe == NULL)
        return NULL;

    return pe->sortSymtab;
}

PESymbol *pe64GetSymByName(const PE64File *pe, const char *name)
{
    if (pe == NULL || name == NULL)
        return NULL;

    uint64_t i = 0;
    uint64_t num = pe64GetAmountSSym(pe);
    for (i = 0; i < num; ++i) {
        PESymbol *sym = pe->symtab + i;
        const char *curName = pe64GetSymName(pe, sym);
        if (strcmp(curName, name) == 0)
            return sym;
    }

    ERROR("There is no symbol %s.", name);
    return NULL;
}

int pe64CmpSym(const void *a, const void *b)
{
    int64_t d = (int64_t)(((const PESymbol*)a)->Value - ((const PESymbol*)b)->Value);
    if (d > 0)
        return 1;
    else if (d < 0)
        return -1;
    else
        return 0;
}

const char*pe64GetLongSymName(const PE64File *pe, const PESymbol *sym)
{
    if (pe == NULL || sym == NULL)
        return NULL;

    DWORD off = sym->N.Name.Long;
    return (const char *)&pe->strtab[off];
}

const char*pe64GetShortSymName(const PE64File *pe, const PESymbol *sym)
{
    if (pe == NULL || sym == NULL)
        return NULL;

    return (const char *)sym->N.ShortName;
}

const char*pe64GetSymName(const PE64File *pe, const PESymbol *sym)
{
    if (pe == NULL || sym == NULL)
        return NULL;

    if (sym->N.Name.Short)
        return pe64GetShortSymName(pe, sym);
    else
        return pe64GetLongSymName(pe,sym);
}

uint64_t pe64GetAmountSSym(const PE64File *pe)
{
    if (pe == NULL)
        return PE64_INV_ARG;

    return pe->symNum;
}

uint64_t pe64GetSSymAddr(const PESymbol *sym)
{
    if (sym == NULL)
        return PE64_INV_ARG;

    return sym->Value;
}

uint64_t pe64GetSSymSectIndx(const PESymbol *sym)
{
    if (sym == NULL)
        return PE64_INV_ARG;

    return (uint64_t)sym->SectionNumber;
}

uint64_t pe64GetAddrSymByName(const PE64File *pe, const char *name)
{
    if (pe == NULL || name == NULL)
        return PE64_INV_ARG;

    PESymbol *sym = pe64GetSymByName(pe, name);
    return pe64GetSSymAddr(sym);
}

uint64_t pe64GetSSymFileoff(const PE64File *pe, const PESymbol *sym)
{
    if (pe == NULL || sym == NULL)
        return PE64_INV_ARG;

    // for object it's offset from start of section
    uint64_t symAddr = pe64GetSSymAddr(sym);
    uint64_t sectIndx = pe64GetSSymSectIndx(sym);
    PESection *sect = pe64GetSectByIndx(pe, sectIndx);
    uint64_t sectFileoff = pe64GetSectFileoff(sect);

    return sectFileoff + symAddr;
}

uint64_t pe64GetSSymSize(const PE64File *pe, const PESymbol *sym)
{
    if (pe == NULL || sym == NULL)
        return PE64_INV_ARG;

    uint64_t i = 0;
    uint64_t vend = 0;
    uint64_t start = pe64GetSSymAddr(sym);
    PESymbol *symtab = pe64GetSSymSortTab(pe);
    for (i = 0; symtab[i].Value < start; ++i);

    if (symtab[i].SectionNumber != sym->SectionNumber) {
        PESection *sect = pe64GetSectByIndx(pe, (uint64_t)sym->SectionNumber);
        vend = pe64GetSectSize(sect);
    } else {
        vend = pe64GetSSymAddr(symtab + i);
    }

    return vend - start;
}

uint64_t pe64GetAmountSeg(const PE64File *pe)
{
    return pe64GetAmountSect(pe);
}

PE64_ERROR pe64Check(const PE64File *pe)
{
    if (pe == NULL)
        return PE64_INV_ARG;

    return PE64_OK;
}

void *pe64Hook(const PE64File *pe, const char *func, const void *hand)
{
    if (pe == NULL || func == NULL || hand == NULL)
        return NULL;

    size_t base = 0;
    if (pe->optHeader->DllCharacteristics & IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE)
        base = (size_t)GetModuleHandle(NULL);
    else
        base = pe->optHeader->ImageBase;

    uint64_t i = 0;
    uint64_t impNum = pe->importNum - 1;
    for (i = 0; i < impNum; ++i) {
        PEImport *imp = pe->import + i;
        ThunkData64 *IAT = (ThunkData64*)(base + imp->FirstThunk);
        ThunkData64 *INT = (ThunkData64*)(base + imp->OriginalFirstThunk);

        uint64_t j = 0;
        for (;;++j) {
            ImportByName *importByName = (ImportByName*)(base + INT[j].u1.AddressOfData);
            if (INT[j].u1.AddressOfData == 0)
                break;

            char *name = importByName->Name;
            if (strcmp(name, func) == 0) {
                void *oldVal = (void*)IAT[j].u1.Function;

                DWORD oldProtect = 0;
                void *addr = (void*)alignToPageSize((size_t)(IAT + j));
                size_t pageSize = (size_t)getPageSize();

                VirtualProtect(addr, pageSize, PAGE_READWRITE, &oldProtect);
                IAT[j].u1.Function = (size_t)hand;
                VirtualProtect(addr, pageSize, oldProtect, &oldProtect);
                return oldVal;
            }
        }
    }

    return NULL;
}


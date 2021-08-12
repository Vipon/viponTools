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
    if (pe == NULL || pe->fd < 0)
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

    FileD fd = 0;
    if ((fd = open(fn, O_RDONLY)) < 0) {
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
        return

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

uint64_t pe64GetMachineID(const PE64File *pe)
{
    if (pe == NULL)
        return PE64_INV_ARG;

    return pe->fileHeader->Machine;
}

PESection *pe64GetSectByAddr(const PE64File *pe)
{
    if (pe == NULL)
        return NULL;

    WORD i = 0;
    for (i = 0; i < pe->sectNum; ++i)
        return &pe->sections[i];

    return NULL;
}

PESection *pe64GetSectByNum(const PE64File *pe, uint64_t sectNum)
{
    if (pe == NULL)
        return NULL;

    if (sectNum > 0 && sectNum <= pe->sectNum)
        return &pe->sections[sectNum - 1]; // sections enumerate from 1, not 0

    return NULL;
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

uint64_t pe64GetSectAddr(const PESection *sect)
{
    if (sect == NULL)
        return PE64_INV_ARG;

    return sect->VirtualAddress;
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
        curName = pe64GetSymName(pe->symtab[i]);
        if (strcmp(curName, name) == 0)
            return pe->symtab + i;
    }

    ERROR("There is no symbol %s.", name);
    return NULL;
}

int pe64CmpSym(const void *a, const void *b)
{
    int64_t d = (int64_t)(((const PESymbol*)a)->value - ((const PESymbol*)b)->value);
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


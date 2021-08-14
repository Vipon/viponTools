#include "mem.h"
#include "comdef.h"
#include "pe64Printer.h"

#include <time.h>
#include <stdio.h>
#include <inttypes.h>

static void pe64PrintImportName(const PE64File *pe, const PEImport *import)
{
    if (pe == NULL || import == NULL)
        return;

    FileD fd = pe->fd;
    uint64_t off = pe64AddrToFileOff(pe, import->Name);
    char *name = readFromFile(fd, &off, 256);
    printf("%s", name);
    Free(name);
}

static void pe64PrintImportTimeStamp(const PE64File *pe, const PEImport *import)
{
    if (pe == NULL || import == NULL)
        return;

    printf("%ld", import->TimeDateStamp);
}

static void pe64PrintImportForwardIndex(const PE64File *pe, const PEImport *import)
{
    if (pe == NULL || import == NULL)
        return;

    printf("%ld", import->ForwarderChain);
}

static void pe64PrintImporAddrTable(const PE64File *pe, const PEImport *import)
{
    if (pe == NULL || import == NULL)
        return;

    printf("%lx", import->FirstThunk);
}

static void pe64PrintImporNameTable(const PE64File *pe, const PEImport *import)
{
    if (pe == NULL || import == NULL)
        return;

    printf("%lx", import->OriginalFirstThunk);
}

void pe64PrintImport(const PE64File *pe, const PEImport *import)
{
    if (pe == NULL || import == NULL)
        return;

    pe64PrintImportName(pe, import);
    NEW_LINE;
    TAB;
    printf("addr table:\t");
    pe64PrintImporAddrTable(pe, import);
    NEW_LINE;
    TAB;
    printf("name table:\t");
    pe64PrintImporNameTable(pe, import);
    NEW_LINE;
    TAB;
    printf("time stamp:\t");
    pe64PrintImportTimeStamp(pe, import);
    NEW_LINE;
    TAB;
    printf("forward index:\t");
    pe64PrintImportForwardIndex(pe, import);
    NEW_LINE;

    printf("\t\tIndx Name\n");
    printf("\t\t---- --------\n");
    FileD fd = pe->fd;
    uint64_t off = pe64AddrToFileOff(pe, import->OriginalFirstThunk);

    for(;;) {
        ThunkData64 *thunkData64 = readFromFile(fd, &off, sizeof(ThunkData64));
        uint64_t AddressOfData = thunkData64->u1.AddressOfData;
        Free(thunkData64);
        off += sizeof(ThunkData64);

        if (AddressOfData) {
            uint64_t importByNameOff = pe64AddrToFileOff(pe, AddressOfData);
            ImportByName *importByName = readFromFile(fd, &importByNameOff, 256);
            printf("\t\t%.4hx %s", importByName->Hint, importByName->Name);
            NEW_LINE;
            Free(importByName);
        } else {
            break;
        }
    }

    NEW_LINE;
}

void pe64PrintImports(const PE64File *pe)
{
    if (pe == NULL)
        return;

    printf("Imports Table:\n");
    printf("--------------------------------\n");

    uint64_t i = 0;
    // Last entry is NULL padded
    for (i = 0; i < pe->importNum - 1; ++i) {
        pe64PrintImport(pe, pe->import + i);
        NEW_LINE;
    }
}


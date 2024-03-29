/***
 * MIT License
 *
 * Copyright (c) 2021-2023 Konychev Valerii
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
#include "comdef.h"
#include "pe64Printer.h"

#include <time.h>
#include <stdio.h>

static void pe64PrintDelayImportName(const PE64File *pe, const PEDelimp *delimp)
{
    if (pe == NULL || delimp == NULL)
        return;

    FileD fd = pe->fd;
    uint64_t off = 0;
    if (delimp->grAttrs) {
        // New it's RVA
        off = pe64AddrToFileOff(pe, delimp->rvaDLLName);
    } else {
        // Old it's virtual addr
        off = pe64AddrToFileOff(pe, delimp->rvaDLLName - pe->optHeader->ImageBase);
    }

    char *name = readFromFile(fd, (size_t*)&off, 256);
    printf("%s", name);
    Free(name);
}

static void pe64PrintDelayImportAttr(const PEDelimp *delimp)
{
    if (delimp == NULL)
        return;

    if (delimp->grAttrs)
        printf("New");
    else
        printf("Old");
}

static void pe64PrintDelayImportHmod(const PEDelimp *delimp)
{
    if (delimp == NULL)
        return;

    printf("%x", (uint32_t)delimp->rvaDLLName);
}

static void pe64PrintDelayImportIAT(const PEDelimp *delimp)
{
    if (delimp == NULL)
        return;

    printf("%x", (uint32_t)delimp->rvaIAT);
}

static void pe64PrintDelayImportINT(const PEDelimp *delimp)
{
    if (delimp == NULL)
        return;

    printf("%x", (uint32_t)delimp->rvaINT);
}

static void pe64PrintDelayImportBoundIAT(const PEDelimp *delimp)
{
    if (delimp == NULL)
        return;

    printf("%x", (uint32_t)delimp->rvaBoundIAT);
}

static void pe64PrintDelayImportUnloadIAT(const PEDelimp *delimp)
{
    if (delimp == NULL)
        return;

    printf("%x", (uint32_t)delimp->rvaUnloadIAT);
}

static void pe64PrintDelayImportTimeStamp(const PEDelimp *delimp)
{
    if (delimp == NULL)
        return;

    if (delimp->dwTimeStamp) {
        time_t time = (time_t)delimp->dwTimeStamp;
        printf("%s", asctime(localtime(&time)));
    } else
        printf("%u", 0);
}

void pe64PrintDelayImport(const PE64File *pe, const PEDelimp *delimp)
{
    if (pe == NULL || delimp == NULL)
        return;

    pe64PrintDelayImportName(pe, delimp);
    NEW_LINE;
    printf("%13s: ", "type");
    pe64PrintDelayImportAttr(delimp);
    NEW_LINE;
    printf("%13s: ", "handle");
    pe64PrintDelayImportHmod(delimp);
    NEW_LINE;
    printf("%13s: ", "IAT");
    pe64PrintDelayImportIAT(delimp);
    NEW_LINE;
    printf("%13s: ", "INT");
    pe64PrintDelayImportINT(delimp);
    NEW_LINE;
    printf("%13s: ", "BoundIAT");
    pe64PrintDelayImportBoundIAT(delimp);
    NEW_LINE;
    printf("%13s: ", "UnloadIAT");
    pe64PrintDelayImportUnloadIAT(delimp);
    NEW_LINE;
    printf("%13s: ", "TimeStamp");
    pe64PrintDelayImportTimeStamp(delimp);
    NEW_LINE;

    printf("%8sIndx Name\n", "");
    printf("%8s---- --------\n", "");

    FileD fd = pe->fd;
    uint64_t off = pe64AddrToFileOff(pe, delimp->rvaINT);
    for(;;) {
        ThunkData64 *INT = readFromFile(fd, (size_t*)&off, sizeof(ThunkData64));
        uint64_t AddressOfData = INT->u1.AddressOfData;

        if (AddressOfData) {
            pe64PrintINT(pe, INT);
            NEW_LINE;
        } else {
            Free(INT);
            break;
        }

        Free(INT);
        off += sizeof(ThunkData64);
    }

    NEW_LINE;
}

void pe64PrintDelayImports(const PE64File *pe)
{
    if (pe == NULL || !pe->delimpNum)
        return;

    printf("Delay Imports Table:\n");
    printf("--------------------------------\n");

    uint64_t i = 0;
    // Last entry is NULL padded
    for (i = 0; i < pe->delimpNum - 1; ++i) {
        pe64PrintDelayImport(pe, pe->delimp + i);
        NEW_LINE;
    }
}


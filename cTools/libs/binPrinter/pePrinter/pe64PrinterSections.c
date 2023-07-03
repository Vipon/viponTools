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

#include "comdef.h"
#include "pe64Printer.h"

#include <stdio.h>
#include <inttypes.h>

void pe64PrintSectName(const PE64File *pe, const PESection *sect)
{
    if (pe == NULL || sect == NULL)
        return;

    switch (pe->type) {
    case PE64_EXEC:
    case PE64_SHARED:
        // For exec and shared all long name are truncated to 8 characters
        printf("%.*s ", IMAGE_SIZEOF_SHORT_NAME, sect->Name);
        break;
    case PE64_OBJ:
        if (sect->Name[0] == '/')
            printf("%s ", pe64GetLongSectName(pe, sect));
        else
            printf("%.*s ", IMAGE_SIZEOF_SHORT_NAME, sect->Name);

        break;
    default:
        //ERROR("Unknown file type");
        break;
    }
}

static void pe64PrintSectMisc(const PE64File *pe, const PESection *sect)
{
    if (pe == NULL || sect == NULL)
        return;

    switch (pe->type) {
    case PE64_OBJ:
        printf("%13s: %.8x ", "PhysAddr", (uint32_t)sect->Misc.PhysicalAddress);
        break;
    case PE64_EXEC:
    case PE64_SHARED:
    default:
        printf("%13s: %.8x ", "VirtSize", (uint32_t)sect->Misc.VirtualSize);
        break;
    }
}

static void pe64PrintSectVirtAddr(const PESection *sect)
{
    if (sect == NULL)
        return;

    printf("%13s: %.8x ", "VirtAddr", (uint32_t)sect->VirtualAddress);
}

static void pe6464PrintSectSizeOfRawData(const PESection *sect)
{
    if (sect == NULL)
        return;

    printf("%13s: %.8x ", "raw data size", (uint32_t)sect->SizeOfRawData);
}

static void pe6464PrintPointerToRawData(const PESection *sect)
{
    if (sect == NULL)
        return;

    printf("%13s: %.8x ", "raw data offs", (uint32_t)sect->PointerToRawData);
}

static void pe6464PrintPointerToRelocations(const PESection *sect)
{
    if (sect == NULL)
        return;

    printf("%13s: %.8x ", "reloc offs", (uint32_t)sect->PointerToRelocations);
}

static void pe6464PrintPointerToLinenumbers(const PESection *sect)
{
    if (sect == NULL)
        return;

    printf("%13s: %.8x ", "line # offs", (uint32_t)sect->PointerToLinenumbers);
}

static void pe6464PrintNumberOfRelocations(const PESection *sect)
{
    if (sect == NULL)
        return;

    printf("%13s: %.8hx ", "relocations", sect->NumberOfRelocations);
}

static void pe6464PrintNumberOfLinenumbers(const PESection *sect)
{
    if (sect == NULL)
        return;

    printf("%13s: %.8hx ", "line #'s", sect->NumberOfLinenumbers);
}

static void pe6464PrintCharacteristics(const PESection *sect)
{
    if (sect == NULL)
        return;

    printf("%13s: %.8x ", "flags", (uint32_t)sect->Characteristics);
}

void pe64PrintSection(const PE64File *pe, const PESection *sect)
{
    if (pe == NULL || sect == NULL)
        return;

    pe64PrintSectName(pe, sect);
    NEW_LINE;

    pe64PrintSectMisc(pe, sect);
    pe64PrintSectVirtAddr(sect);
    NEW_LINE;

    pe6464PrintPointerToRawData(sect);
    pe6464PrintSectSizeOfRawData(sect);
    NEW_LINE;

    pe6464PrintPointerToRelocations(sect);
    pe6464PrintNumberOfRelocations(sect);
    NEW_LINE;

    pe6464PrintPointerToLinenumbers(sect);
    pe6464PrintNumberOfLinenumbers(sect);
    NEW_LINE;

    pe6464PrintCharacteristics(sect);
    NEW_LINE;
}

void pe64PrintSections(const PE64File *pe)
{
    if (pe == NULL || pe->sections == NULL)
        return;

    uint8_t i = 0;
    for (i = 0; i < pe->sectNum; ++i) {
        printf("%.2"PRIu8" ", (uint8_t)(i + 1));
        pe64PrintSection(pe, &pe->sections[i]);
        NEW_LINE;
    }
}


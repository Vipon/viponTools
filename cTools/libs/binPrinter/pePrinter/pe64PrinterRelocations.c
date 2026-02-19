/***
 * MIT License
 *
 * Copyright (c) 2026 Konychev Valerii
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
#include "inttypes.h"
#include "pe64Printer.h"

static const
char *pe64GetRelBaseTypeStr(uint16_t relBase)
{
    uint16_t type = (relBase & 0xF000) >> 12;
    switch (type) {
    case IMAGE_REL_BASED_ABSOLUTE:
        return "ABS";
    case IMAGE_REL_BASED_HIGH:
        return "HIGH";
    case IMAGE_REL_BASED_LOW:
        return "LOW";
    case IMAGE_REL_BASED_HIGHLOW:
        return "HIGHLOW";
    case IMAGE_REL_BASED_HIGHADJ:
        return "HIGHADJ";\
    /*
    case IMAGE_REL_BASED_MACHINE_SPECIFIC_5:
        return "MACHINE_SPECIFIC_5";
    case IMAGE_REL_BASED_RESERVED:
        return "RESERVED";
    case IMAGE_REL_BASED_MACHINE_SPECIFIC_7:
        return "MACHINE_SPECIFIC_7";
    case IMAGE_REL_BASED_MACHINE_SPECIFIC_8:
        return "MACHINE_SPECIFIC_8";
    case IMAGE_REL_BASED_MACHINE_SPECIFIC_9:
        return "MACHINE_SPECIFIC_9";
    */
    case IMAGE_REL_BASED_DIR64:
        return "DIR64";
    default:
        return "UNKNOWN";
    }
}

static
uint64_t pe64GetRelVal(const PE64File *pe, uint32_t vaddr,
    uint16_t relBase)
{
    uint64_t relVal = 0;
    uint16_t voff = relBase & 0xFFF;
    uint16_t type = (relBase & 0xF000) >> 12;
    switch (type) {
    case IMAGE_REL_BASED_ABSOLUTE:
        break;
    case IMAGE_REL_BASED_HIGH:
        break;
    case IMAGE_REL_BASED_LOW:
        break;
    case IMAGE_REL_BASED_HIGHLOW:
        break;
    case IMAGE_REL_BASED_HIGHADJ:
        break;
    /*
    case IMAGE_REL_BASED_MACHINE_SPECIFIC_5:
        break;
    case IMAGE_REL_BASED_RESERVED:
        break;
    case IMAGE_REL_BASED_MACHINE_SPECIFIC_7:
        break;
    case IMAGE_REL_BASED_MACHINE_SPECIFIC_8:
        break;
    case IMAGE_REL_BASED_MACHINE_SPECIFIC_9:
        break;
    */
    case IMAGE_REL_BASED_DIR64:
    {
        size_t fileOff = pe64AddrToFileOff(pe, vaddr + voff);
        uint64_t *val = readFromFile(pe->fd, &fileOff, sizeof(uint64_t));
        relVal = *val;
        Free(val);
        break;
    }
    default:
        break;
    }

    return relVal;
}

static
void pe64PrintBaseRelocation(const PE64File *pe)
{
    PESection *relSect = pe64GetSectByName(pe, ".reloc");
    if (relSect == NULL)
        return;

    uint64_t sectSize = pe64GetSectSize(relSect);
    uint8_t *reloc = pe64ReadSect(pe, relSect);
    if (reloc == NULL)
        return;

    printf("Base relocations:\n");
    uint8_t *p = reloc;
    while (p < reloc + sectSize) {
        PEBaseReloc *base = (PEBaseReloc*)p;
        uint32_t vaddr = base->VirtualAddress;
        uint32_t bsize = base->SizeOfBlock;
        if (bsize == 0)
            break;

        printf("%8"PRIx32" RVA,%8"PRIx32" SizeOfBlock\n", vaddr, bsize);
        p += sizeof(vaddr) + sizeof(bsize);
        while (p < (uint8_t*)base + bsize) {
            uint16_t relBase = *(uint16_t*)p;
            uint16_t voff = relBase & 0xFFF;
            uint64_t relVal = pe64GetRelVal(pe, vaddr, relBase);
            printf("%8"PRIx16" %6s      %.16"PRIx64"\n", voff,
                pe64GetRelBaseTypeStr(relBase), relVal);

            p += sizeof(uint16_t);
        }
    }

    Free(reloc);
    NEW_LINE;
}

static const
char *coffX86GetRelTypeStr(uint16_t type)
{
    switch (type) {
    case IMAGE_REL_I386_ABSOLUTE:
        return "ABSOLUTE";
    case IMAGE_REL_I386_DIR16:
        return "DIR16";
    case IMAGE_REL_I386_REL16:
        return "REL16";
    case IMAGE_REL_I386_DIR32:
        return "DIR32";
    case IMAGE_REL_I386_DIR32NB:
        return "DIR32NB";
    case IMAGE_REL_I386_SEG12:
        return "SEG12";
    case IMAGE_REL_I386_SECTION:
        return "SECTION";
    case IMAGE_REL_I386_SECREL:
        return "SECREL";
    case IMAGE_REL_I386_TOKEN:
        return "TOKEN";
    case IMAGE_REL_I386_SECREL7:
        return "SECREL7";
    case IMAGE_REL_I386_REL32:
        return "REL32";
    default:
        return "UNKNOWN";
    }
}

static const
char *coffX86_64GetRelTypeStr(uint16_t type)
{
    switch (type) {
    case IMAGE_REL_AMD64_ABSOLUTE:
        return "ABSOLUTE";
    case IMAGE_REL_AMD64_ADDR64:
        return "ADDR64";
    case IMAGE_REL_AMD64_ADDR32:
        return "ADDR32";
    case IMAGE_REL_AMD64_ADDR32NB:
        return "ADDR32NB";
    case IMAGE_REL_AMD64_REL32:
        return "REL32";
    case IMAGE_REL_AMD64_REL32_1:
        return "REL32_1";
    case IMAGE_REL_AMD64_REL32_2:
        return "REL32_2";
    case IMAGE_REL_AMD64_REL32_3:
        return "REL32_3";
    case IMAGE_REL_AMD64_REL32_4:
        return "REL32_4";
    case IMAGE_REL_AMD64_REL32_5:
        return "REL32_5";
    case IMAGE_REL_AMD64_SECTION:
        return "SECTION";
    case IMAGE_REL_AMD64_SECREL:
        return "SECREL";
    case IMAGE_REL_AMD64_SECREL7:
        return "SECREL7";
    case IMAGE_REL_AMD64_TOKEN:
        return "TOKEN";
    case IMAGE_REL_AMD64_SREL32:
        return "SREL32";
    case IMAGE_REL_AMD64_PAIR:
        return "PAIR";
    case IMAGE_REL_AMD64_SSPAN32:
        return "SSPAN32";
    default:
        return "UNKNOWN";
    }
}

static const
char *coffARMGetRelTypeStr(uint16_t type)
{
    switch (type) {
    case IMAGE_REL_ARM_ABSOLUTE:
        return "ABSOLUTE";
    case IMAGE_REL_ARM_ADDR32:
        return "ADDR32";
    case IMAGE_REL_ARM_ADDR32NB:
        return "ADDR32NB";
    case IMAGE_REL_ARM_BRANCH24:
        return "BRANCH24";
    case IMAGE_REL_ARM_BRANCH11:
        return "BRANCH11";
    case IMAGE_REL_ARM_TOKEN:
        return "TOKEN";
    case IMAGE_REL_ARM_GPREL12:
        return "GPREL12";
    case IMAGE_REL_ARM_GPREL7:
        return "GPREL7";
    case IMAGE_REL_ARM_BLX24:
        return "BLX24";
    case IMAGE_REL_ARM_BLX11:
        return "BLX11";
    case IMAGE_REL_ARM_SECTION:
        return "SECTION";
    case IMAGE_REL_ARM_SECREL:
        return "SECREL";
    case IMAGE_REL_ARM_MOV32A:
        return "MOV32A";
    case IMAGE_REL_ARM_MOV32T:
        return "MOV32T";
    case IMAGE_REL_ARM_BRANCH20T:
        return "BRANCH20T";
    case IMAGE_REL_ARM_BRANCH24T:
        return "BRANCH24T";
    case IMAGE_REL_ARM_BLX23T:
        return "BLX23T";
    default:
        return "UNKNOWN";
    }
}

static const
char *coffAARCH64GetRelTypeStr(uint16_t type)
{
    switch (type) {
    case IMAGE_REL_ARM64_ABSOLUTE:
        return "ABSOLUTE";
    case IMAGE_REL_ARM64_ADDR32:
        return "ADDR32";
    case IMAGE_REL_ARM64_ADDR32NB:
        return "ADDR32NB";
    case IMAGE_REL_ARM64_BRANCH26:
        return "BRANCH26";
    case IMAGE_REL_ARM64_PAGEBASE_REL21:
        return "PAGEBASE_REL21";
    case IMAGE_REL_ARM64_REL21:
        return "REL21";
    case IMAGE_REL_ARM64_PAGEOFFSET_12A:
        return "PAGEOFFSET_12A";
    case IMAGE_REL_ARM64_PAGEOFFSET_12L:
        return "PAGEOFFSET_12L";
    case IMAGE_REL_ARM64_SECREL:
        return "SECREL";
    case IMAGE_REL_ARM64_SECREL_LOW12A:
        return "SECREL_LOW12A";
    case IMAGE_REL_ARM64_SECREL_HIGH12A:
        return "SECREL_HIGH12A";
    case IMAGE_REL_ARM64_SECREL_LOW12L:
        return "SECREL_LOW12L";
    case IMAGE_REL_ARM64_TOKEN:
        return "TOKEN";
    case IMAGE_REL_ARM64_SECTION:
        return "SECTION";
    case IMAGE_REL_ARM64_ADDR64:
        return "ADDR64";
    case IMAGE_REL_ARM64_BRANCH19:
        return "BRANCH19";
    default:
        return "UNKNOWN";
    }
}

static const
char *coffGetRelTypeStr(Arch arch, uint16_t type)
{
    switch (arch) {
    case X86:
        return coffX86GetRelTypeStr(type);
    case X86_64:
        return coffX86_64GetRelTypeStr(type);
    case ARM:
        return coffARMGetRelTypeStr(type);
    case AARCH64:
        return coffAARCH64GetRelTypeStr(type);
    default:
        return "UNKNOWN";
    }
}

static
uint64_t coffX86GetRelAddr(const PE64File *pe, uint16_t type, uint64_t vaddr)
{
    size_t addrSize = 0;
    UNUSED(addrSize);
    size_t addrFileOff = pe64AddrToFileOff(pe, vaddr);
    UNUSED(addrFileOff);
    switch (type) {
    default:
        return 0;
    }
}

static
uint64_t coffX86_64GetRelAddr(const PE64File *pe, uint16_t type, uint64_t vaddr)
{
    size_t addrSize = 0;
    UNUSED(addrSize);
    size_t addrFileOff = pe64AddrToFileOff(pe, vaddr);
    UNUSED(addrFileOff);
    switch (type) {
    case IMAGE_REL_AMD64_ADDR64:
        addrSize = 8;
        break;
    case IMAGE_REL_AMD64_ADDR32:
        addrSize = 4;
        break;
    case IMAGE_REL_AMD64_ADDR32NB:
        addrSize = 4;
        break;
    case IMAGE_REL_AMD64_REL32:
        addrSize = 4;
        break;
    case IMAGE_REL_AMD64_REL32_1:
        addrSize = 4;
        addrFileOff += 1;
        break;
    case IMAGE_REL_AMD64_REL32_2:
        addrSize = 4;
        addrFileOff += 2;
        break;
    case IMAGE_REL_AMD64_REL32_3:
        addrSize = 4;
        addrFileOff += 3;
        break;
    case IMAGE_REL_AMD64_REL32_4:
        addrSize = 4;
        addrFileOff += 4;
        break;
    case IMAGE_REL_AMD64_REL32_5:
        addrSize = 4;
        addrFileOff += 5;
        break;
    case IMAGE_REL_AMD64_SECTION:
        break;
    case IMAGE_REL_AMD64_SECREL:
        break;
    case IMAGE_REL_AMD64_SECREL7:
        break;
    case IMAGE_REL_AMD64_TOKEN:
        break;
    case IMAGE_REL_AMD64_SREL32:
        break;
    case IMAGE_REL_AMD64_PAIR:
        break;
    case IMAGE_REL_AMD64_SSPAN32:
        break;
    case IMAGE_REL_AMD64_ABSOLUTE:
    default:
        break;
    }

    return 0;
}

static
uint64_t coffARMGetRelAddr(const PE64File *pe, uint16_t type, uint64_t vaddr)
{
    size_t addrSize = 0;
    UNUSED(addrSize);
    size_t addrFileOff = pe64AddrToFileOff(pe, vaddr);
    UNUSED(addrFileOff);
    switch (type) {
    default:
        return 0;
    }
}

static
uint64_t coffAARCH64GetRelAddr(const PE64File *pe, uint16_t type, uint64_t vaddr)
{
    size_t addrSize = 0;
    UNUSED(addrSize);
    size_t addrFileOff = pe64AddrToFileOff(pe, vaddr);
    UNUSED(addrFileOff);
    switch (type) {
    default:
        return 0;
    }
}

static
uint64_t coffGetRelAddr(const PE64File *pe, uint16_t type, uint64_t vaddr)
{
    // !TODO: finish getting relocatable addr
    switch (pe->arch) {
    case X86:
        return coffX86GetRelAddr(pe, type, vaddr);
    case X86_64:
        return coffX86_64GetRelAddr(pe, type, vaddr);
    case ARM:
        return coffARMGetRelAddr(pe, type, vaddr);
    case AARCH64:
        return coffAARCH64GetRelAddr(pe, type, vaddr);
    default:
        return 0;
    }
}

static
void pe64PrintSectRelocations(const PE64File *pe)
{
    uint8_t i = 0;
    for (i = 0; i < pe->sectNum; ++i) {
        uint16_t relNum = pe->sections[i].NumberOfRelocations;
        size_t relFileOff = pe->sections[i].PointerToRelocations;
        if (relFileOff == 0)
            continue;

        printf("Relocations #%"PRIu8"\n", (uint8_t)(i + 1));
        printf(" %-8s %-16s %-17s %-8s %-8s\n", "Offset"
                                              , "Type"
                                              , "Applied To"
                                              , "Sym Indx"
                                              , "Sym Name");
        printf(" %-8s %-16s %-17s %-8s %-8s\n", "--------"
                                              , "----------------"
                                              , "-----------------"
                                              , "--------"
                                              , "--------");
        COFFReloc *rel = readFromFile(pe->fd, &relFileOff,
                            sizeof(COFFReloc) * relNum);

        uint16_t j = 0;
        for (j = 0; j < relNum; ++j) {
            uint64_t vaddr = pe->sections[i].VirtualAddress
                           + rel[j].VirtualAddress;
            uint32_t symIndx = (uint32_t)rel[j].SymbolTableIndex;

            printf(" %.8"PRIx32" %-16s %.16"PRIx64"  %8"PRIu32" %-8s\n"
                , (uint32_t)rel[j].VirtualAddress
                , coffGetRelTypeStr(pe->arch, rel[j].Type)
                , coffGetRelAddr(pe, rel[j].Type, vaddr)
                , symIndx
                , pe64GetSymName(pe, pe->symtab + symIndx));
        }

        Free(rel);
        NEW_LINE;
    }
}

void pe64PrintRelocations(const PE64File *pe)
{
    if (pe == NULL)
        return;

    if (IS_PE64_FILE_OBJ(pe)) {
        pe64PrintSectRelocations(pe);
    } else if (IS_PE64_FILE_EXEC(pe)) {
        pe64PrintBaseRelocation(pe);
    }
}


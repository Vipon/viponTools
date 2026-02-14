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
#include "macho64Printer.h"

#include <inttypes.h>

const Macho64RelType MACHO64_ARM_REL_TYPE[] = {
    { ARM_RELOC_VANILLA, "VANILLA" },
    { ARM_RELOC_PAIR, "PAIR" },
    { ARM_RELOC_SECTDIFF, "SECTDIFF" },
    { ARM_RELOC_LOCAL_SECTDIFF, "LOCAL_SECTDIFF" },
    { ARM_RELOC_PB_LA_PTR, "PB_LA_PTR" },
    { ARM_RELOC_BR24, "BR24" },
    { ARM_THUMB_RELOC_BR22, "THUMB_BR22" },
    { ARM_THUMB_32BIT_BRANCH, "THUMB_32BIT_BR" },
    { ARM_RELOC_HALF, "HALF" },
    { ARM_RELOC_HALF_SECTDIFF, "HALF_SECTDIFF" },
};

const Macho64RelType MACHO64_ARM64_REL_TYPE[] = {
    { ARM64_RELOC_UNSIGNED, "UNSIGNED" },
    { ARM64_RELOC_SUBTRACTOR, "SUBTRACTOR" },
    { ARM64_RELOC_BRANCH26, "BRANCH26" },
    { ARM64_RELOC_PAGE21, "PAGE21" },
    { ARM64_RELOC_PAGEOFF12, "PAGEOFF12" },
    { ARM64_RELOC_GOT_LOAD_PAGE21, "GOT_LOAD_PAGE21" },
    { ARM64_RELOC_GOT_LOAD_PAGEOFF12, "GOT_LOAD_PAGEOFF12" },
    { ARM64_RELOC_POINTER_TO_GOT, "POINTER_TO_GOT" },
    { ARM64_RELOC_TLVP_LOAD_PAGE21, "TLVP_LOAD_PAGE21" },
    { ARM64_RELOC_TLVP_LOAD_PAGEOFF12, "TLVP_LOAD_PAGEOFF12" },
    { ARM64_RELOC_ADDEND, "ADDEND" },
    { ARM64_RELOC_AUTHENTICATED_POINTER, "AUTHENTICATED_POINTER" },
};

const Macho64RelType MACHO64_X86_64_REL_TYPE[] = {
    { X86_64_RELOC_UNSIGNED, "UNSIGNED" },
    { X86_64_RELOC_SIGNED, "SIGNED" },
    { X86_64_RELOC_BRANCH, "BRANCH" },
    { X86_64_RELOC_GOT_LOAD, "GOT_LOAD" },
    { X86_64_RELOC_GOT, "GOT" },
    { X86_64_RELOC_SUBTRACTOR, "SUBTRACTOR" },
    { X86_64_RELOC_SIGNED_1, "SIGNED_1" },
    { X86_64_RELOC_SIGNED_2, "SIGNED_2" },
    { X86_64_RELOC_SIGNED_4, "SIGNED_4" },
    { X86_64_RELOC_TLV, "TLV" },
};

static const
char *macho64GetRelTypeStr(Arch arch, uint8_t type)
{
    switch (arch) {
    case X86_64:
        if (sizeof(MACHO64_X86_64_REL_TYPE) / sizeof (Macho64RelType) > type)
            return MACHO64_X86_64_REL_TYPE[type].str;
        break;
    case ARM:
        if (sizeof(MACHO64_ARM_REL_TYPE) / sizeof (Macho64RelType) > type)
            return MACHO64_ARM_REL_TYPE[type].str;
        break;
    case AARCH64:
        if (sizeof(MACHO64_ARM64_REL_TYPE) / sizeof (Macho64RelType) > type)
            return MACHO64_ARM64_REL_TYPE[type].str;
        break;
    default:
        break;
    }

    return "UKNOWN";
}

static const
char *macho64GetRelLengthStr(uint8_t len)
{
    switch (len) {
    case 0:
        return "1 byte";
    case 1:
        return "2 bytes";
    case 2:
        return "4 bytes";
    case 3:
        return "8 bytes";
    default:
        return "? bytes";
    }
}


static
void macho64PrintRelocVal(const Macho64File *mf, uint32_t extrn,
    uint32_t symbolnum, uint32_t type)
{
    if (mf->arch == AARCH64) {
        if (type == ARM64_RELOC_ADDEND) {
            printf("addend = %"PRIu32, symbolnum);
            return;
        }
    }

    if (extrn) {
        // relative to symbol
        printf("%s", macho64GetSymName(mf, mf->symtab + symbolnum));
    } else {
        // relative to section
        Macho64Sect *sect = macho64GetSectByIndx(mf, symbolnum);
        printf("%u (%s,%s)", symbolnum, sect->segname, sect->sectname);
    }
}

static
void macho64PrintRelocInfo(const Macho64File *mf, const MachoRelocInfo *relInfo)
{
    printf("%08"PRIx32" %-5s %-7s %-6s %-9s %-15s",
        (uint32_t)relInfo->r_address,
        relInfo->r_pcrel ? "True" : "False",
        macho64GetRelLengthStr(relInfo->r_length),
        relInfo->r_extern? "True" : "False",
        "False",
        macho64GetRelTypeStr(mf->arch, relInfo->r_type)
    );

    macho64PrintRelocVal(mf, relInfo->r_extern, relInfo->r_symbolnum, relInfo->r_type);

    NEW_LINE;
}

/*static
void macho64PrintScatRelocInfo(const Macho64File *mf, const MachoScatRelocInfo *scatRelInfo)
{
    printf("%08"PRIx32" %-5s %-7s %-6s %-9s %-15s",
        scatRelInfo->r_address,
        scatRelInfo->r_pcrel ? "True" : "False",
        macho64GetRelLengthStr(scatRelInfo->r_length),
        scatRelInfo->r_extern? "True" : "False",
        "True",
        macho64GetRelTypeStr(mf->arch, scatRelInfo->r_type)
    );

    macho64PrintRelocVal(mf, scatRelInfo->r_extern, scatRelInfo->r_symbolnum, scatRelInfo->r_type);

    NEW_LINE;
}*/

void macho64PrintRelocations(const Macho64File *mf)
{
    FOREACH_LOAD_COMMAND(mf,
        if (lcom->cmd == LC_SEGMENT_64) {
            Macho64Seg *seg = (Macho64Seg*)lcom;
            Macho64Sect *sect = (Macho64Sect*)((size_t)seg + sizeof(Macho64Seg));

            uint32_t j = 0;
            for (j = 0; j < seg->nsects; ++j, ++sect) {
                uint64_t relocOff = sect->reloff;
                uint64_t numReloc = sect->nreloc;
                if (numReloc == 0)
                    continue;

                uint64_t relocSize = sizeof(MachoRelocInfo) * numReloc;
                MachoRelocInfo *relInfo = (MachoRelocInfo*)readFromFile(mf->fd,
                                                (size_t*)&relocOff, relocSize);
                if (relInfo == NULL)
                    continue;

                printf("Relocation information (%s,%s) %"PRIu64" entries\n",
                    sect->segname, sect->sectname, numReloc);
                printf("address  pcrel length  extern scattered type           symbolnum/value\n");
                uint64_t k = 0;
                for (k = 0; k < numReloc; ++k)
                    if ((uint32_t)relInfo->r_address & R_SCATTERED) {
                        //!TODO: need to implement scater info printing
                        //macho64PrintScatRelocInfo(mf, (MachoScatRelocInfo*)(relInfo + k));
                    } else {
                        macho64PrintRelocInfo(mf, relInfo + k);
                    }

                Free(relInfo);
            }
        }
    )
}


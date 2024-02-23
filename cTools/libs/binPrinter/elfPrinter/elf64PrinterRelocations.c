/***
 * MIT License
 *
 * Copyright (c) 2024 Konychev Valerii
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
#include "elf64Printer.h"

#include <inttypes.h>

const Elf64RelType ELF64_X86_REL_TYPES[] = {
    { R_386_NONE, "R_386_NONE" },
    { R_386_32, "R_386_32" },
    { R_386_PC32, "R_386_PC32" },
    { R_386_GOT32, "R_386_GOT32" },
    { R_386_PLT32, "R_386_PLT32" },
    { R_386_COPY, "R_386_COPY" },
    { R_386_GLOB_DAT, "R_386_GLOB_DAT" },
    { R_386_JMP_SLOT, "R_386_JMP_SLOT" },
    { R_386_RELATIVE, "R_386_RELATIVE" },
    { R_386_GOTOFF, "R_386_GOTOFF" },
    { R_386_GOTPC, "R_386_GOTPC" },
    { R_386_32PLT, "R_386_32PLT" },
    { R_386_TLS_TPOFF, "R_386_TLS_TPOFF" },
    { R_386_TLS_IE, "R_386_TLS_IE" },
    { R_386_TLS_GOTIE, "R_386_TLS_GOTIE" },
    { R_386_TLS_LE, "R_386_TLS_LE" },
    { R_386_TLS_GD, "R_386_TLS_GD" },
    { R_386_TLS_LDM, "R_386_TLS_LDM" },
    { R_386_16, "R_386_16" },
    { R_386_PC16, "R_386_PC16" },
    { R_386_8, "R_386_8" },
    { R_386_PC8, "R_386_PC8" },
    { R_386_TLS_GD_32, "R_386_TLS_GD_32" },
    { R_386_TLS_GD_PUSH, "R_386_TLS_GD_PUSH" },
    { R_386_TLS_GD_CALL, "R_386_TLS_GD_CALL" },
    { R_386_TLS_GD_POP, "R_386_TLS_GD_POP" },
    { R_386_TLS_LDM_32, "R_386_TLS_LDM_32" },
    { R_386_TLS_LDM_PUSH, "R_386_TLS_LDM_PUSH" },
    { R_386_TLS_LDM_CALL, "R_386_TLS_LDM_CALL" },
    { R_386_TLS_LDM_POP, "R_386_TLS_LDM_POP" },
    { R_386_TLS_LDO_32, "R_386_TLS_LDO_32" },
    { R_386_TLS_IE_32, "R_386_TLS_IE_32" },
    { R_386_TLS_LE_32, "R_386_TLS_LE_32" },
    { R_386_TLS_DTPMOD32, "R_386_TLS_DTPMOD32" },
    { R_386_TLS_DTPOFF32, "R_386_TLS_DTPOFF32" },
    { R_386_TLS_TPOFF32, "R_386_TLS_TPOFF32" },
    { R_386_SIZE32, "R_386_SIZE32" },
    { R_386_TLS_GOTDESC, "R_386_TLS_GOTDESC" },
    { R_386_TLS_DESC_CALL, "R_386_TLS_DESC_CALL" },
    { R_386_TLS_DESC, "R_386_TLS_DESC" },
    { R_386_IRELATIVE, "R_386_IRELATIVE" },
    { R_386_GOT32X, "R_386_GOT32X" },
};

const Elf64RelType ELF64_X86_64_REL_TYPES[] = {
    { R_X86_64_NONE, "R_X86_64_NONE" },
    { R_X86_64_64, "R_X86_64_64" },
    { R_X86_64_PC32, "R_X86_64_PC32" },
    { R_X86_64_GOT32, "R_X86_64_GOT32" },
    { R_X86_64_PLT32, "R_X86_64_PLT32" },
    { R_X86_64_COPY, "R_X86_64_COPY" },
    { R_X86_64_GLOB_DAT, "R_X86_64_GLOB_DAT" },
    { R_X86_64_JMP_SLOT, "R_X86_64_JUMP_SLO" },
    { R_X86_64_RELATIVE, "R_X86_64_RELATIVE" },
    { R_X86_64_GOTPCREL, "R_X86_64_GOTPCREL" },
    { R_X86_64_32, "R_X86_64_32" },
    { R_X86_64_32S, "R_X86_64_32S" },
    { R_X86_64_16, "R_X86_64_16" },
    { R_X86_64_PC16, "R_X86_64_PC16" },
    { R_X86_64_8, "R_X86_64_8" },
    { R_X86_64_PC8, "R_X86_64_PC8" },
    { R_X86_64_DTPMOD64, "R_X86_64_DTPMOD64" },
    { R_X86_64_DTPOFF64, "R_X86_64_DTPOFF64" },
    { R_X86_64_TPOFF64, "R_X86_64_TPOFF64" },
    { R_X86_64_TLSGD, "R_X86_64_TLSGD" },
    { R_X86_64_TLSLD, "R_X86_64_TLSLD" },
    { R_X86_64_DTPOFF32, "R_X86_64_DTPOFF32" },
    { R_X86_64_GOTTPOFF, "R_X86_64_GOTTPOFF" },
    { R_X86_64_TPOFF32, "R_X86_64_TPOFF32" },
    { R_X86_64_PC64, "R_X86_64_PC64" },
    { R_X86_64_GOTOFF64, "R_X86_64_GOTOFF64" },
    { R_X86_64_GOTPC32, "R_X86_64_GOTPC32" },
    { R_X86_64_GOT64, "R_X86_64_GOT64" },
    { R_X86_64_GOTPCREL64, "R_X86_64_GOTPCREL64" },
    { R_X86_64_GOTPC64, "R_X86_64_GOTPC64" },
    { R_X86_64_GOTPLT64, "R_X86_64_GOTPLT64" },
    { R_X86_64_PLTOFF64, "R_X86_64_PLTOFF64" },
    { R_X86_64_SIZE32, "R_X86_64_SIZE32" },
    { R_X86_64_SIZE64, "R_X86_64_SIZE64" },
    { R_X86_64_GOTPC32_TLSDESC, "R_X86_64_GOTPC32_TLSDESC" },
    { R_X86_64_TLSDESC_CALL, "R_X86_64_TLSDESC_CALL" },
    { R_X86_64_TLSDESC, "R_X86_64_TLSDESC" },
    { R_X86_64_IRELATIVE, "R_X86_64_IRELATIVE" },
    { R_X86_64_RELATIVE64, "R_X86_64_RELATIVE64" },
    { R_X86_64_GOTPCRELX, "R_X86_64_GOTPCRELX" },
    { R_X86_64_REX_GOTPCRELX, "R_X86_64_REX_GOTPCRELX" },
};

const Elf64RelType ELF64_ARM_REL_TYPES[] = {
    { R_ARM_NONE, "R_ARM_NONE" },
    { R_ARM_PC24, "R_ARM_PC24" },
    { R_ARM_ABS32, "R_ARM_ABS32" },
    { R_ARM_REL32, "R_ARM_REL32" },
    { R_ARM_PC13, "R_ARM_PC13" },
    { R_ARM_ABS16, "R_ARM_ABS16" },
    { R_ARM_ABS12, "R_ARM_ABS12" },
    { R_ARM_THM_ABS5, "R_ARM_THM_ABS5" },
    { R_ARM_ABS8, "R_ARM_ABS8" },
    { R_ARM_SBREL32, "R_ARM_SBREL32" },
    { R_ARM_THM_PC22, "R_ARM_THM_PC22" },
    { R_ARM_THM_PC8, "R_ARM_THM_PC8" },
    { R_ARM_AMP_VCALL9, "R_ARM_AMP_VCALL9" },
    { R_ARM_SWI24, "R_ARM_SWI24" },
    { R_ARM_THM_SWI8, "R_ARM_THM_SWI8" },
    { R_ARM_XPC25, "R_ARM_XPC25" },
    { R_ARM_THM_XPC22, "R_ARM_THM_XPC22" },
    { R_ARM_TLS_DTPMOD32, "R_ARM_TLS_DTPMOD32" },
    { R_ARM_TLS_DTPOFF32, "R_ARM_TLS_DTPOFF32" },
    { R_ARM_TLS_TPOFF32, "R_ARM_TLS_TPOFF32" },
    { R_ARM_COPY, "R_ARM_COPY" },
    { R_ARM_GLOB_DAT, "R_ARM_GLOB_DAT" },
    { R_ARM_JUMP_SLOT, "R_ARM_JUMP_SLOT" },
    { R_ARM_RELATIVE, "R_ARM_RELATIVE" },
    { R_ARM_GOTOFF, "R_ARM_GOTOFF" },
    { R_ARM_GOTPC, "R_ARM_GOTPC" },
    { R_ARM_GOT32, "R_ARM_GOT32" },
    { R_ARM_PLT32, "R_ARM_PLT32" },
    { R_ARM_GNU_VTENTRY, "R_ARM_GNU_VTENTRY" },
    { R_ARM_GNU_VTINHERIT, "R_ARM_GNU_VTINHERIT" },
    { R_ARM_RSBREL32, "R_ARM_RSBREL32" },
    { R_ARM_THM_RPC22, "R_ARM_THM_RPC22" },
    { R_ARM_RREL32, "R_ARM_RREL32" },
    { R_ARM_RABS32, "R_ARM_RABS32" },
    { R_ARM_RPC24, "R_ARM_RPC24" },
    { R_ARM_RBASE, "R_ARM_RBASE" },
};

const Elf64RelType ELF64_AARCH64_REL_TYPES[] = {
    { R_AARCH64_NONE, "R_AARCH64_NONE" },
    { R_AARCH64_ABS64, "R_AARCH64_ABS64" },
    { R_AARCH64_ABS32, "R_AARCH64_ABS32" },
    { R_AARCH64_ABS16, "R_AARCH64_ABS16" },
    { R_AARCH64_PREL64, "R_AARCH64_PREL64" },
    { R_AARCH64_PREL32, "R_AARCH64_PREL32" },
    { R_AARCH64_PREL16, "R_AARCH64_PREL16" },
    { R_AARCH64_TSTBR14, "R_AARCH64_TSTBR14" },
    { R_AARCH64_CONDBR19, "R_AARCH64_CONDBR19" },
    { R_AARCH64_JUMP26, "R_AARCH64_JUMP26" },
    { R_AARCH64_CALL26, "R_AARCH64_CALL26" },
    { R_AARCH64_COPY, "R_AARCH64_COPY" },
    { R_AARCH64_GLOB_DAT, "R_AARCH64_GLOB_DAT" },
    { R_AARCH64_JUMP_SLOT, "R_AARCH64_JUMP_SLOT" },
    { R_AARCH64_RELATIVE, "R_AARCH64_RELATIVE" },
    { R_AARCH64_TLS_DTPREL64, "R_AARCH64_TLS_DTPREL64" },
    { R_AARCH64_TLS_DTPMOD64, "R_AARCH64_TLS_DTPMOD64" },
    { R_AARCH64_TLS_TPREL64, "R_AARCH64_TLS_TPREL64" },
    { R_AARCH64_TLSDESC, "R_AARCH64_TLSDESC" },
    { R_AARCH64_IRELATIVE, "R_AARCH64_IRELATIVE" },
};

const Elf64RelType *ELF64_RELA_TYPES[] = {
    ELF64_X86_REL_TYPES,
    ELF64_X86_64_REL_TYPES,
    ELF64_ARM_REL_TYPES,
    ELF64_AARCH64_REL_TYPES,
};

static const
char *elf64GetRelTypeStr(Arch arch, uint64_t type)
{
    switch (arch) {
    case X86:
    case X86_64:
        return ELF64_RELA_TYPES[arch][type].str;
    case ARM:
    case AARCH64:
        {
            int64_t i = -1;
            while (ELF64_RELA_TYPES[arch][++i].flag)
                if (ELF64_RELA_TYPES[arch][i].flag == type)
                    return ELF64_RELA_TYPES[arch][i].str;
        }
        FALLTHROUGH;
    default:
        return "UNKNOWN";
    }
}

void elf64PrintRela(const Elf64File *elf, const Elf64Rel *rel)
{
    uint64_t relType = ELF64_R_TYPE(rel->r_info);
    uint64_t dsymNum = ELF64_R_SYM(rel->r_info);
    Elf64Sym *sym = elf->dynsym + dsymNum;
    printf("%.12"PRIx64"  %.12"PRIx64" %s %.16"PRIx64" %s", rel->r_offset
                                          , rel->r_info
                                          , elf64GetRelTypeStr(elf->arch, relType)
                                          , sym->st_value
                                          , elf64GetDSymName(elf, sym)
                                          );

    Elf64Shdr *version = elf64GetSectByType(elf, SHT_GNU_versym);
    if (version == NULL)
        return;

    uint16_t *ver = elf64ReadSect(elf, version);
    if (ver == NULL)
        return;

    const char *verName = elf64GetVerNameBySymVersion(elf, ver[dsymNum]);
    if (verName)
        printf("@%s", verName);

    printf(" + %"PRIu64, rel->r_addend);
    NEW_LINE;

    Free(ver);
}

void elf64PrintRelaPlt(const Elf64File *elf)
{
    Elf64Shdr *relplt = elf64GetSectByName(elf, RELAPLT);
    if (relplt == NULL)
        return;

    uint64_t relpltAmount = relplt->sh_size / sizeof(Elf64Rel);
    if (relpltAmount == 0)
        return;

    printf("Relocation section '"RELAPLT"' at offset 0x%"PRIx64" contains %"PRIu64" entry:\n",
        relplt->sh_offset, relpltAmount);
    printf("  Offset%10sInfo%11sType%11sSym. Value%4sSym. Name + Addend\n", "", "", "", "");

    uint64_t i = 0;
    for (i = 0; i < relpltAmount; ++i)
        elf64PrintRela(elf, elf->relaplt + i);

    NEW_LINE;
}

void elf64PrintRelaDyn(const Elf64File *elf)
{
    Elf64Shdr *reldyn = elf64GetSectByName(elf, RELADYN);
    if (reldyn == NULL)
        return;

    uint64_t reldynAmount = reldyn->sh_size / sizeof(Elf64Rel);
    if (reldynAmount == 0)
        return;

    printf("Relocation section '"RELADYN"' at offset 0x%"PRIx64" contains %"PRIu64" entries:\n",
        reldyn->sh_offset, reldynAmount);
    printf("  Offset%10sInfo%11sType%11sSym. Value%4sSym. Name + Addend\n", "", "", "", "");

    uint64_t i = 0;
    for (i = 0; i < reldynAmount; ++i)
        elf64PrintRela(elf, elf->reladyn + i);

    NEW_LINE;
}

void elf64PrintRelocations(const Elf64File *elf)
{
    elf64PrintRelaDyn(elf);
    elf64PrintRelaPlt(elf);
}


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

const Elf64SegType ELF64_SEG_TYPES[] = {
    { PT_NULL, "NULL" },
    { PT_LOAD, "LOAD" },
    { PT_DYNAMIC, "DYNAMIC" },
    { PT_INTERP, "INTERP" },
    { PT_NOTE, "NOTE" },
    { PT_SHLIB, "SHLIB" },
    { PT_PHDR, "PHDR" },
    { PT_TLS, "TLS" },
    { PT_LOOS, "LOOS" },
    { PT_SUNW_UNWIND, "SUNW_UNWIND" },
    { PT_GNU_EH_FRAME, "GNU_EH_FRAME" },
    { PT_GNU_STACK, "GNU_STACK" },
    { PT_GNU_RELRO, "GNU_RELRO" },
    { PT_DUMP_DELTA, "DUMP_DELTA" },
    { PT_LOSUNW, "LOSUNW" },
    { PT_SUNWBSS, "SUNWBSS" },
    { PT_SUNWSTACK, "SUNWSTACK" },
    { PT_SUNWDTRACE, "SUNWDTRACE" },
    { PT_SUNWCAP, "SUNWCAP" },
    { PT_HISUNW, "HISUNW" },
    { PT_HIOS, "HIOS" },
    { PT_LOPROC, "LOPROC" },
    { PT_ARM_ARCHEXT, "ARM_ARCHEXT" },
    { PT_ARM_EXIDX, "ARM_EXIDX" },
    { PT_MIPS_REGINFO, "MIPS_REGINFO" },
    { PT_MIPS_RTPROC, "MIPS_RTPROC" },
    { PT_MIPS_OPTIONS, "MIPS_OPTIONS" },
    { PT_MIPS_ABIFLAGS, "MIPS_ABIFLAGS" },
    { PT_HIPROC, "HIPROC" },
    { PT_OPENBSD_RANDOMIZE, "OPENBSD_RANDOMIZE" },
    { PT_OPENBSD_WXNEEDED, "OPENBSD_WXNEEDED" },
    { PT_OPENBSD_BOOTDATA, "OPENBSD_BOOTDATA" },
};

static const
char *elf64GetSegTypeStr(uint32_t type)
{
    uint32_t segTypeNum = sizeof(ELF64_SEG_TYPES) / sizeof(Elf64SegType);
    uint32_t i = 0;
    for (i = 0; i < segTypeNum; ++i)
        if (ELF64_SEG_TYPES[i].flag == type)
            return ELF64_SEG_TYPES[i].str;

    return "UNKNOWN";
}

const Elf64SegFlag ELF64_SEG_FLAGS[] = {
    { PF_R, "R" },
    { PF_W, "W" },
    { PF_X, "E" },
    { PF_MASKOS, "MASKOS" },
    { PF_MASKPROC, "MASKPROC" },
};

static
void elf64PrintSegFlags(const Elf64Phdr *seg)
{
    uint32_t segFlagsNum = sizeof(ELF64_SEG_FLAGS) / sizeof(Elf64SegFlag);
    uint32_t i = 0;
    for (i = 0; i < segFlagsNum; ++i)
        if (ELF64_SEG_FLAGS[i].flag & seg->p_flags)
            printf("%s", ELF64_SEG_FLAGS[i].str);
        else
            SPACE;
}

static
char *elf64GetInterpreter(const Elf64File *elf, const Elf64Phdr *seg)
{
    return readFromFile(elf->fd, (const size_t*)&(seg->p_offset), seg->p_filesz);
}

void elf64PrintSegment(const Elf64File *elf, const Elf64Phdr *seg)
{
    UNUSED(elf);
    printf("  %-14s", elf64GetSegTypeStr(seg->p_type));
    printf(" 0x%.16"PRIx64, seg->p_offset);
    printf(" 0x%.16"PRIx64, seg->p_vaddr);
    printf(" 0x%.16"PRIx64"\n", seg->p_paddr);
    printf("%17s0x%.16"PRIx64, "", seg->p_filesz);
    printf(" 0x%.16"PRIx64"  ", seg->p_memsz);
    elf64PrintSegFlags(seg);
    printf("  0x%"PRIx64"\n", seg->p_align);
    if (seg->p_type == PT_INTERP) {
        char *interpreter = elf64GetInterpreter(elf, seg);
        printf("%6s[Requesting program interpreter: %s]\n", "", interpreter);
        Free(interpreter);
    }
}

static
void elf64PrintSectInSeg(const Elf64File *elf, const Elf64Phdr *seg)
{
    uint64_t startSeg = seg->p_vaddr;
    uint64_t endSeg = startSeg + seg->p_memsz;

    uint16_t i = 0;
    for (i = 0; i < elf->header->e_shnum; ++i) {
        uint64_t startSect = elf->sections[i].sh_addr;
        if (startSect >= startSeg && startSect < endSeg)
            printf("%s ", elf64GetSectName(elf, elf->sections + i));
    }
}

void elf64PrintSegments(const Elf64File *elf)
{
    printf("Program Headers:\n");
    printf("  Type%11sOffset%13sVirtAddr%11sPhysAddr\n", "", "", "");
    printf("%17sFileSiz%12sMemSiz%14sFlags  Align\n", "", "", "");

    uint16_t segNum = elf->header->e_phnum;
    uint16_t i = 0;
    for (i = 0; i < segNum; ++i) {
        elf64PrintSegment(elf, elf->segments + i);
    }

    printf("\n Section to Segment mapping:\n");
    printf("  Segment Sections...\n");
    for (i = 0; i < segNum; ++i) {
        printf("   %.2u     ", i);
        elf64PrintSectInSeg(elf, elf->segments + i);
        NEW_LINE;
    }

    NEW_LINE;
}


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

const Elf64DynamicTag ELF64_DYNAMIC_TAGS[] = {
    { DT_NULL, "(NULL)" },
    { DT_NEEDED, "(NEEDED)" },
    { DT_PLTRELSZ, "(PLTRELSZ)" },
    { DT_PLTGOT, "(PLTGOT)" },
    { DT_HASH, "(HASH)" },
    { DT_STRTAB, "(STRTAB)" },
    { DT_SYMTAB, "(SYMTAB)" },
    { DT_RELA, "(RELA)" },
    { DT_RELASZ, "(RELASZ)" },
    { DT_RELAENT, "(RELAENT)" },
    { DT_STRSZ, "(STRSZ)" },
    { DT_SYMENT, "(SYMENT)" },
    { DT_INIT, "(INIT)" },
    { DT_FINI, "(FINI)" },
    { DT_SONAME, "(SONAME)" },
    { DT_RPATH, "(RPATH)" },
    { DT_SYMBOLIC, "(SYMBOLIC)" },
    { DT_REL, "(REL)" },
    { DT_RELSZ, "(RELSZ)" },
    { DT_RELENT, "(RELENT)" },
    { DT_PLTREL, "(PLTREL)" },
    { DT_DEBUG, "(DEBUG)" },
    { DT_TEXTREL, "(TEXTREL)" },
    { DT_JMPREL, "(JMPREL)" },
    { DT_BIND_NOW, "(BIND_NOW)" },
    { DT_INIT_ARRAY, "(INIT_ARRAY)" },
    { DT_FINI_ARRAY, "(FINI_ARRAY)" },
    { DT_INIT_ARRAYSZ, "(INIT_ARRAYSZ)" },
    { DT_FINI_ARRAYSZ, "(FINI_ARRAYSZ)" },
    { DT_RUNPATH, "(RUNPATH)" },
    { DT_FLAGS, "(FLAGS)" },
    //{ DT_ENCODING, "(ENCODING)" },
    { DT_PREINIT_ARRAY, "(PREINIT_ARRAY)" },
    { DT_PREINIT_ARRAYSZ, "(PREINIT_ARRAYSZ)" },
    { DT_MAXPOSTAGS, "(MAXPOSTAGS)" },
    { DT_LOOS, "(LOOS)" },
    { DT_SUNW_AUXILIARY, "(SUNW_AUXILIARY)" },
    { DT_SUNW_RTLDINF, "(SUNW_RTLDINF)" },
    { DT_SUNW_FILTER, "(SUNW_FILTER)" },
    { DT_SUNW_CAP, "(SUNW_CAP)" },
    { DT_SUNW_ASLR, "(SUNW_ASLR)" },
    { DT_HIOS, "(HIOS)" },
    { DT_VALRNGLO, "(VALRNGLO)" },
    { DT_GNU_PRELINKED, "(GNU_PRELINKED)" },
    { DT_GNU_CONFLICTSZ, "(GNU_CONFLICTSZ)" },
    { DT_GNU_LIBLISTSZ, "(GNU_LIBLISTSZ)" },
    { DT_CHECKSUM, "(CHECKSUM)" },
    { DT_PLTPADSZ, "(PLTPADSZ)" },
    { DT_MOVEENT, "(MOVEENT)" },
    { DT_MOVESZ, "(MOVESZ)" },
    { DT_FEATURE, "(FEATURE)" },
    { DT_FEATURE_1, "(FEATURE_1)" },
    { DT_POSFLAG_1, "(POSFLAG_1)" },
    { DT_SYMINSZ, "(SYMINSZ)" },
    { DT_SYMINENT, "(SYMINENT)" },
    { DT_VALRNGHI, "(VALRNGHI)" },
    { DT_ADDRRNGLO, "(ADDRRNGLO)" },
    { DT_GNU_HASH, "(GNU_HASH)" },
    { DT_TLSDESC_PLT, "(TLSDESC_PLT)" },
    { DT_TLSDESC_GOT, "(TLSDESC_GOT)" },
    { DT_GNU_CONFLICT, "(GNU_CONFLICT)" },
    { DT_GNU_LIBLIST, "(GNU_LIBLIST)" },
    { DT_CONFIG, "(CONFIG)" },
    { DT_DEPAUDIT, "(DEPAUDIT)" },
    { DT_AUDIT, "(AUDIT)" },
    { DT_PLTPAD, "(PLTPAD)" },
    { DT_MOVETAB, "(MOVETAB)" },
    { DT_SYMINFO, "(SYMINFO)" },
    { DT_ADDRRNGHI, "(ADDRRNGHI)" },
    { DT_VERSYM, "(VERSYM)" },
    { DT_RELACOUNT, "(RELACOUNT)" },
    { DT_RELCOUNT, "(RELCOUNT)" },
    { DT_FLAGS_1, "(FLAGS_1)" },
    { DT_VERDEF, "(VERDEF)" },
    { DT_VERDEFNUM, "(VERDEFNUM)" },
    { DT_VERNEED, "(VERNEED)" },
    { DT_VERNEEDNUM, "(VERNEEDNUM)" },
    { DT_LOPROC, "(LOPROC)" },
    { DT_ARM_SYMTABSZ, "(ARM_SYMTABSZ)" },
    { DT_ARM_PREEMPTMAP, "(ARM_PREEMPTMAP)" },
    { DT_SPARC_REGISTER, "(SPARC_REGISTER)" },
    { DT_DEPRECATED_SPARC_REGISTER, "(DEPRECATED_SPARC_REGISTER)" },
    { DT_MIPS_RLD_VERSION, "(MIPS_RLD_VERSION)" },
    { DT_MIPS_TIME_STAMP, "(MIPS_TIME_STAMP)" },
    { DT_MIPS_ICHECKSUM, "(MIPS_ICHECKSUM)" },
    { DT_MIPS_IVERSION, "(MIPS_IVERSION)" },
    { DT_MIPS_FLAGS, "(MIPS_FLAGS)" },
    { DT_MIPS_BASE_ADDRESS, "(MIPS_BASE_ADDRESS)" },
    { DT_MIPS_CONFLICT, "(MIPS_CONFLICT)" },
    { DT_MIPS_LIBLIST, "(MIPS_LIBLIST)" },
    { DT_MIPS_LOCAL_GOTNO, "(MIPS_LOCAL_GOTNO)" },
    { DT_MIPS_CONFLICTNO, "(MIPS_CONFLICTNO)" },
    { DT_MIPS_LIBLISTNO, "(MIPS_LIBLISTNO)" },
    { DT_MIPS_SYMTABNO, "(MIPS_SYMTABNO)" },
    { DT_MIPS_UNREFEXTNO, "(MIPS_UNREFEXTNO)" },
    { DT_MIPS_GOTSYM, "(MIPS_GOTSYM)" },
    { DT_MIPS_HIPAGENO, "(MIPS_HIPAGENO)" },
    { DT_MIPS_RLD_MAP, "(MIPS_RLD_MAP)" },
    { DT_MIPS_DELTA_CLASS, "(MIPS_DELTA_CLASS)" },
    { DT_MIPS_DELTA_CLASS_NO, "(MIPS_DELTA_CLASS_NO)" },
    { DT_MIPS_DELTA_INSTANCE, "(MIPS_DELTA_INSTANCE)" },
    { DT_MIPS_DELTA_INSTANCE_NO, "(MIPS_DELTA_INSTANCE_NO)" },
    { DT_MIPS_DELTA_RELOC, "(MIPS_DELTA_RELOC)" },
    { DT_MIPS_DELTA_RELOC_NO, "(MIPS_DELTA_RELOC_NO)" },
    { DT_MIPS_DELTA_SYM, "(MIPS_DELTA_SYM)" },
    { DT_MIPS_DELTA_SYM_NO, "(MIPS_DELTA_SYM_NO)" },
    { DT_MIPS_DELTA_CLASSSYM, "(MIPS_DELTA_CLASSSYM)" },
    { DT_MIPS_DELTA_CLASSSYM_NO, "(MIPS_DELTA_CLASSSYM_NO)" },
    { DT_MIPS_CXX_FLAGS, "(MIPS_CXX_FLAGS)" },
    { DT_MIPS_PIXIE_INIT, "(MIPS_PIXIE_INIT)" },
    { DT_MIPS_SYMBOL_LIB, "(MIPS_SYMBOL_LIB)" },
    { DT_MIPS_LOCALPAGE_GOTIDX, "(MIPS_LOCALPAGE_GOTIDX)" },
    { DT_MIPS_LOCAL_GOTIDX, "(MIPS_LOCAL_GOTIDX)" },
    { DT_MIPS_HIDDEN_GOTIDX, "(MIPS_HIDDEN_GOTIDX)" },
    { DT_MIPS_PROTECTED_GOTIDX, "(MIPS_PROTECTED_GOTIDX)" },
    { DT_MIPS_OPTIONS, "(MIPS_OPTIONS)" },
    { DT_MIPS_INTERFACE, "(MIPS_INTERFACE)" },
    { DT_MIPS_DYNSTR_ALIGN, "(MIPS_DYNSTR_ALIGN)" },
    { DT_MIPS_INTERFACE_SIZE, "(MIPS_INTERFACE_SIZE)" },
    { DT_MIPS_RLD_TEXT_RESOLVE_ADDR, "(MIPS_RLD_TEXT_RESOLVE_ADDR)" },
    { DT_MIPS_PERF_SUFFIX, "(MIPS_PERF_SUFFIX)" },
    { DT_MIPS_COMPACT_SIZE, "(MIPS_COMPACT_SIZE)" },
    { DT_MIPS_GP_VALUE, "(MIPS_GP_VALUE)" },
    { DT_MIPS_AUX_DYNAMIC, "(MIPS_AUX_DYNAMIC)" },
    { DT_MIPS_PLTGOT, "(MIPS_PLTGOT)" },
    { DT_MIPS_RLD_OBJ_UPDATE, "(MIPS_RLD_OBJ_UPDATE)" },
    { DT_MIPS_RWPLT, "(MIPS_RWPLT)" },
    { DT_MIPS_RLD_MAP_REL, "(MIPS_RLD_MAP_REL)" },
    { DT_PPC_GOT, "(PPC_GOT)" },
    { DT_PPC_TLSOPT, "(PPC_TLSOPT)" },
    { DT_PPC64_GLINK, "(PPC64_GLINK)" },
    { DT_PPC64_OPD, "(PPC64_OPD)" },
    { DT_PPC64_OPDSZ, "(PPC64_OPDSZ)" },
    { DT_PPC64_TLSOPT, "(PPC64_TLSOPT)" },
    { DT_AUXILIARY, "(AUXILIARY)" },
    { DT_USED, "(USED)" },
    { DT_FILTER, "(FILTER)" },
    { DT_HIPROC, "(HIPROC)" },
};

static const
char *elf64GetDynamicTag(int64_t tag)
{
    size_t tagNum = sizeof(ELF64_DYNAMIC_TAGS) / sizeof(Elf64DynamicTag);
    size_t i = 0;
    for (i = 0; i < tagNum; ++i) {
        if (ELF64_DYNAMIC_TAGS[i].flag == tag)
            return ELF64_DYNAMIC_TAGS[i].str;
    }

    return "UNKNOWN";
}

static
void elf64PrintDynPtr(const Elf64Dyn *dyn)
{
    printf("0x%"PRIx64, dyn->d_un.d_ptr);
}

static
void elf64PrintDynVal(const Elf64File *elf, const Elf64Dyn *dyn)
{
    switch (dyn->d_tag) {
    case DT_NEEDED:
        printf("Shared library: [%s]", elf->dtStrTab + dyn->d_un.d_val);
        break;
    case DT_PLTRELSZ:
    case DT_RELASZ:
    case DT_RELAENT:
    case DT_STRSZ:
    case DT_SYMENT:
    case DT_INIT_ARRAYSZ:
    case DT_FINI_ARRAYSZ:
    case DT_RELRENT:
    case DT_RELSZ:
        printf("%"PRIu64" (bytes)", dyn->d_un.d_val);
        break;
    case DT_PLTREL:
        switch (dyn->d_un.d_val) {
        case DT_REL:
            printf("REL");
            break;
        case DT_RELA:
            printf("RELA");
            break;
        case DT_RELR:
            printf("RELR");
            break;
        default:
            printf("UNKNOWN");
        }
        break;
    default:
        printf("%"PRIu64, dyn->d_un.d_val);
        break;
    }
}

static
void elf64PrintDynUn(const Elf64File *elf, const Elf64Dyn *dyn)
{
    if (  dyn->d_tag >= DT_ENCODING
       && (dyn->d_tag <= DT_HIOS || dyn->d_tag >= DT_LOPROC)
    ) {
        if (dyn->d_tag % 2)
            elf64PrintDynPtr(dyn);
        else
            elf64PrintDynVal(elf, dyn);
    } else switch(dyn->d_tag) {
    case DT_NEEDED:
    case DT_PLTRELSZ:
    case DT_RELASZ:
    case DT_RELAENT:
    case DT_STRSZ:
    case DT_SYMENT:
    case DT_SONAME:
    case DT_RPATH:
    case DT_RELSZ:
    case DT_RELENT:
    case DT_PLTREL:
    case DT_INIT_ARRAYSZ:
    case DT_FINI_ARRAYSZ:
    case DT_RUNPATH:
    case DT_FLAGS:
    case DT_PREINIT_ARRAYSZ:
    case DT_RELRSZ:
    case DT_RELRENT:
    case DT_CHECKSUM:
    case DT_PLTPADSZ:
    case DT_MOVEENT:
    case DT_MOVESZ:
    case DT_FEATURE_1:
    case DT_POSFLAG_1:
    case DT_SYMINSZ:
    case DT_SYMINENT:
    case DT_RELACOUNT:
    case DT_RELCOUNT:
    case DT_FLAGS_1:
    case DT_VERDEFNUM:
    case DT_VERNEEDNUM:
    case DT_SPARC_REGISTER:
    case DT_AUXILIARY:
    case DT_USED:
    case DT_FILTER:
        elf64PrintDynVal(elf, dyn);
        break;

    case DT_NULL:
    case DT_PLTGOT:
    case DT_HASH:
    case DT_STRTAB:
    case DT_SYMTAB:
    case DT_RELA:
    case DT_INIT:
    case DT_FINI:
    case DT_REL:
    case DT_DEBUG:
    case DT_JMPREL:
    case DT_INIT_ARRAY:
    case DT_FINI_ARRAY:
    case DT_PREINIT_ARRAY:
    case DT_RELR:
    case DT_SUNW_RTLDINF:
//  from DT_ADDRRNGLO    0x6ffffe00
    case DT_GNU_HASH:
    case DT_TLSDESC_PLT:
    case DT_TLSDESC_GOT:
    case DT_GNU_CONFLICT:
    case DT_GNU_LIBLIST:
    case DT_CONFIG:
    case DT_DEPAUDIT:
    case DT_AUDIT:
    case DT_PLTPAD:
    case DT_MOVETAB:
    case DT_SYMINFO:
//  until DT_ADDRRNGHI    0x6ffffeff
    case DT_VERSYM:
    case DT_VERDEF:
    case DT_VERNEED:
        elf64PrintDynPtr(dyn);
        break;

    case DT_BIND_NOW:
    case DT_TEXTREL:
    case DT_SYMBOLIC:
    case DT_LOOS:
    case DT_HIOS:
    case DT_VALRNGLO:

    case DT_LOPROC:
        /* Ignoring */
    default:
        break;
    }
}

void elf64PrintDynamicSection(const Elf64File *elf)
{
    if (elf->dynamic == NULL)
        return;

    Elf64Shdr *dyn = elf64GetSectByType(elf, SHT_DYNAMIC);
    if (dyn == NULL)
        return;

    size_t num = 0;
    while (elf->dynamic[num++].d_tag != DT_NULL);

    printf("Dynamic section at offset 0x%"PRIx64" contains %zu entries:\n", dyn->sh_offset, num);
    printf("  Tag%8sType%25sName/Value\n", "", "");
    size_t i = 0;
    for (i = 0; i < num; ++i) {
        printf(" 0x%.16"PRIx64" %-20s ", elf->dynamic[i].d_tag
                                       , elf64GetDynamicTag(elf->dynamic[i].d_tag));
        elf64PrintDynUn(elf, elf->dynamic + i);
        NEW_LINE
    }

    NEW_LINE;
}


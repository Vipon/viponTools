#include "elf64Printer.h"

#include <inttypes.h>

static const
char *elf64GetSectTypeStr(uint32_t type)
{
    switch (type) {
    case SHT_NULL:
        return "NULL";
    case SHT_PROGBITS:
        return "PROGBITS";
    case SHT_SYMTAB:
        return "SYMTAB";
    case SHT_STRTAB:
        return "STRTAB";
    case SHT_RELA:
        return "RELA";
    case SHT_HASH:
        return "HASH";
    case SHT_DYNAMIC:
        return "DYNAMIC";
    case SHT_NOTE:
        return "NOTE";
    case SHT_NOBITS:
        return "NOBITS";
    case SHT_REL:
        return "REL";
    case SHT_SHLIB:
        return "SHLIB";
    case SHT_DYNSYM:
        return "DYNSYM";
    case SHT_INIT_ARRAY:
        return "INIT_ARRAY";
    case SHT_FINI_ARRAY:
        return "FINI_ARRAY";
    case SHT_PREINIT_ARRAY:
        return "PREINIT_ARRAY";
    case SHT_GROUP:
        return "GROUP";
    case SHT_SYMTAB_SHNDX:
        return "SYMTAB_SHNDX";
    case SHT_LOOS:
        return "LOOS";
    case SHT_LOSUNW:
        return "LOSUNW";
    case SHT_GNU_ATTRIBUTES:
        return "GNU_ATTRIBUTES";
    case SHT_GNU_HASH:
        return "GNU_HASH";
    case SHT_GNU_LIBLIST:
        return "GNU_LIBLIST";
    case SHT_SUNW_DEBUGSTR:
        return "SUNW_DEBUGSTR";
    case SHT_SUNW_DEBUG:
        return "SUNW_DEBUG";
    case SHT_SUNW_move:
        return "SUNW_move";
    case SHT_SUNW_COMDAT:
        return "SUNW_COMDAT";
    case SHT_SUNW_syminfo:
        return "SUNW_syminfo";
    case SHT_GNU_verdef:
        return "VERDEF";
    case SHT_GNU_verneed:
        return "VERNEED";
    case SHT_GNU_versym:
        return "VERSYM";
    case SHT_LOPROC:
        return "LOPROC";
    case SHT_X86_64_UNWIND:
        return "X86_64_UNWIND";
    case SHT_ARM_PREEMPTMAP:
        return "ARM_PREEMPTMAP";
    case SHT_ARM_ATTRIBUTES:
        return "ARM_ATTRIBUTES";
    case SHT_ARM_DEBUGOVERLAY:
        return "ARM_DEBUGOVERLAY";
    case SHT_ARM_OVERLAYSECTION:
        return "ARM_OVERLAYSECTION";
    case SHT_MIPS_REGINFO:
        return "MIPS_REGINFO";
    case SHT_MIPS_PACKAGE:
        return "MIPS_PACKAGE";
    case SHT_MIPS_PACKSYM:
        return "MIPS_PACKSYM";
    case SHT_MIPS_RELD:
        return "MIPS_RELD";
    case SHT_MIPS_IFACE:
        return "MIPS_IFACE";
    case SHT_MIPS_CONTENT:
        return "MIPS_CONTENT";
    case SHT_MIPS_OPTIONS:
        return "MIPS_OPTIONS";
    case SHT_MIPS_DELTASYM:
        return "MIPS_DELTASYM";
    case SHT_MIPS_DELTAINST:
        return "MIPS_DELTAINST";
    case SHT_MIPS_DELTACLASS:
        return "MIPS_DELTACLASS";
    case SHT_MIPS_DWARF:
        return "MIPS_DWARF";
    case SHT_MIPS_DELTADECL:
        return "MIPS_DELTADECL";
    case SHT_MIPS_SYMBOL_LIB:
        return "MIPS_SYMBOL_LIB";
    case SHT_MIPS_EVENTS:
        return "MIPS_EVENTS";
    case SHT_MIPS_TRANSLATE:
        return "MIPS_TRANSLATE";
    case SHT_MIPS_PIXIE:
        return "MIPS_PIXIE";
    case SHT_MIPS_XLATE:
        return "MIPS_XLATE";
    case SHT_MIPS_XLATE_DEBUG:
        return "MIPS_XLATE_DEBUG";
    case SHT_MIPS_WHIRL:
        return "MIPS_WHIRL";
    case SHT_MIPS_EH_REGION:
        return "MIPS_EH_REGION";
    case SHT_MIPS_XLATE_OLD:
        return "MIPS_XLATE_OLD";
    case SHT_MIPS_PDR_EXCEPTION:
        return "MIPS_PDR_EXCEPTION";
    case SHT_MIPS_ABIFLAGS:
        return "MIPS_ABIFLAGS";
    case SHT_HIPROC:
        return "HIPROC";
    case SHT_LOUSER:
        return "LOUSER";
    case SHT_HIUSER:
        return "HIUSER";
    default:
        return "UNKNOWN";
    }
}

static
void elf64PrintSectionType(const Elf64Shdr *sect)
{
    printf("%-17s", elf64GetSectTypeStr(sect->sh_type));
}

const Elf64SectFlag ELF64_SECT_FLAGS[] = {
    { SHF_WRITE, "W" },
    { SHF_ALLOC, "A" },
    { SHF_EXECINSTR, "X" },
    { SHF_MERGE, "M" },
    { SHF_STRINGS, "S" },
    { SHF_INFO_LINK, "I" },
    { SHF_LINK_ORDER, "L" },
    { SHF_OS_NONCONFORMING, "O" },
    { SHF_GROUP, "G" },
    { SHF_TLS, "T" },
    { SHF_COMPRESSED, "C" },
    //{ , "x" },
    { SHF_MASKOS, "o" },
    { SHF_EXCLUDE, "E" },
    //{ , "l" },
    { SHF_MASKPROC, "p" },
};

static
void elf64PrintSectionFlags(const Elf64Shdr *sect)
{
    uint8_t bufP = 0;
    const size_t flagsNum = sizeof(ELF64_SECT_FLAGS) / sizeof(Elf64SectFlag);
    char buf[flagsNum + 1] = "";

    uint8_t i = 0;
    for (i = 0; i < flagsNum; ++i)
        if (sect->sh_flags & ELF64_SECT_FLAGS[i].flag)
            buf[bufP++] = ELF64_SECT_FLAGS[i].str[0];

    printf("%3s", buf);

    if (bufP > 3 && bufP < 8)
        SPACEs(9 - bufP);
    if (bufP <= 3)
        SPACEs(6);
}

void elf64PrintSection(const Elf64File *elf, const Elf64Shdr *sect)
{
    printf("%-18.17s", elf64GetSectName(elf, sect));
    elf64PrintSectionType(sect);
    printf("%16.16"PRIx64"  ", sect->sh_addr);
    printf("%8.8"PRIx64, sect->sh_offset);
    NEW_LINE;
    printf("%7s%16.16"PRIx64"  ", "", sect->sh_size);
    printf("%16.16"PRIx64" ", sect->sh_entsize);
    elf64PrintSectionFlags(sect);
    printf("%2"PRIu32"   ", sect->sh_link);
    printf("%3"PRIu32"     ", sect->sh_info);
    printf("%"PRIu64"\n", sect->sh_addralign);
}

void elf64PrintSections(const Elf64File *elf)
{
    printf("Section Headers:\n");
    printf("  [Nr] Name%14sType%13sAddress%11sOffset\n", "", "", "");
    printf("       Size%14sEntSize%10sFlags  Link  Info  Align\n", "", "");
    uint16_t i = 0;
    for (i = 0; i < elf->header->e_shnum; ++i) {
        printf("  [%2"PRIu16"] ", i);
        elf64PrintSection(elf, elf->sections + i);
    }
    printf("Key to Flags:\n"
           "  W (write), A (alloc), X (execute), M (merge), S (strings), I (info),\n"
           "  L (link order), O (extra OS processing required), G (group), T (TLS),\n"
           "  C (compressed), x (unknown), o (OS specific), E (exclude),\n"
           "  l (large), p (processor specific)\n\n");
}


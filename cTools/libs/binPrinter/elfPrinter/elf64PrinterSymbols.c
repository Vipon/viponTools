#include "elf64Printer.h"

#include <inttypes.h>

static const
char *elf64GetSymTypeStr(const Elf64Sym *sym)
{
    switch (ELF64_ST_TYPE(sym->st_info)) {
    case STT_NOTYPE:
        return "NOTYPE";
    case STT_OBJECT:
        return "OBJECT";
    case STT_FUNC:
        return "FUNC";
    case STT_SECTION:
        return "SECTION";
    case STT_FILE:
        return "FILE";
    case STT_COMMON:
        return "COMMON";
    case STT_TLS:
        return "TLS";
    case STT_NUM:
        return "NUM";
    case STT_LOOS:
        return "LOOS";
    case STT_HIOS:
        return "HIOS";
    case STT_LOPROC:
        return "LOPROC";
    case STT_HIPROC:
        return "HIPROC";
    default:
        return "UNKNOWN";
    }
}

static const
char *elf64GetSymBindStr(const Elf64Sym *sym)
{
    switch (ELF64_ST_BIND(sym->st_info)) {
    case STB_LOCAL:
        return "LOCAL";
    case STB_GLOBAL:
        return "GLOBAL";
    case STB_WEAK:
        return "WEAK";
    case STB_LOOS:
        return "LOOS";
    case STB_HIOS:
        return "HIOS";
    case STB_LOPROC:
        return "LOPROC";
    case STB_HIPROC:
        return "HIPROC";
    default:
        return "UNKNOWN";
    }
}

static const
char *elf64GetSymVisStr(const Elf64Sym *sym)
{
    switch (ELF64_ST_VISIBILITY(sym->st_other)) {
    case STV_DEFAULT:
        return "DEFAULT";
    case STV_INTERNAL:
        return "INTERNAL";
    case STV_HIDDEN:
        return "HIDDEN";
    case STV_PROTECTED:
        return "PROTECTED";
    case STV_EXPORTED:
        return "EXPORTED";
    case STV_SINGLETON:
        return "SINGLETON";
    case STV_ELIMINATE:
        return "ELIMINATE";
    default:
        return "UNKNOWN";
    }
}

static
void elf64PrintSymIndx(const Elf64Sym *sym)
{
    switch (sym->st_shndx) {
    case SHN_UNDEF:
        printf("UND");
        break;
        break;
    case SHN_LOPROC:
        printf("LOPROC");
        break;
    case SHN_HIPROC:
        printf("HIPROC");
        break;
    case SHN_LOOS:
        printf("LOOS");
        break;
    case SHN_HIOS:
        printf("HIOS");
        break;
    case SHN_ABS:
        printf("ABS");
        break;
    case SHN_COMMON:
        printf("COMMON");
        break;
    case SHN_XINDEX:
        printf("XINDEX");
        break;
    default:
        printf("%3"PRIu16, sym->st_shndx);
        break;
    }
}

void elf64PrintDSymbol(const Elf64File *elf, const Elf64Sym *sym, uint64_t indx)
{
    printf("%016"PRIx64"  %4"PRIu64 " %-7s %-6s %-8s ", sym->st_value
                                                      , sym->st_size
                                                      , elf64GetSymTypeStr(sym)
                                                      , elf64GetSymBindStr(sym)
                                                      , elf64GetSymVisStr(sym));
    elf64PrintSymIndx(sym);
    printf(" %s", elf64GetDSymName(elf, sym));
    if (indx) {
        uint16_t ver = elf64GetSymVersionByIndx(elf, indx);
        const char *verName = elf64GetVerNameBySymVersion(elf, ver);
        if (verName) {
            printf("@%s (%"PRIu16")", verName, ver);
        }
    }
}

void elf64PrintSymbol(const Elf64File *elf, const Elf64Sym *sym)
{
    printf("%016"PRIx64"  %4"PRIu64 " %-7s %-6s %-8s ", sym->st_value
                                                      , sym->st_size
                                                      , elf64GetSymTypeStr(sym)
                                                      , elf64GetSymBindStr(sym)
                                                      , elf64GetSymVisStr(sym));
    elf64PrintSymIndx(sym);
    printf(" %.25s", elf64GetSymName(elf, sym));
}

void elf64PrintSymbols(const Elf64File *elf)
{
    uint64_t i = 0;

    Elf64Sym *dymsym = elf64GetDSymTab(elf);
    if (dymsym) {
        uint64_t dsymnum = elf64GetAmountDSym(elf);

        printf("Symbol table '"DYNSYM"' contains %"PRIu64" entries:\n", dsymnum);
        printf("   Num:%4sValue%10sSize Type%4sBind   Vis%6sNdx Name\n", "", "", "", "");
        for (i = 0; i < dsymnum; ++i) {
            printf("%4s%2"PRIu64": ", "", i);
            elf64PrintDSymbol(elf, dymsym + i, i);
            NEW_LINE;
        }

        NEW_LINE;
    }

    Elf64Sym *ssym = elf64GetSSymTab(elf);
    if (ssym) {
        uint64_t ssymNum = elf64GetAmountSSym(elf);

        printf("Symbol table '"SYMTAB"' contains %"PRIu64" entries:\n", ssymNum);
        printf("   Num:%4sValue%10sSize Type%4sBind   Vis%6sNdx Name\n", "", "", "", "");
        for (i = 0; i < ssymNum; ++i) {
            printf("%4s%2"PRIu64": ", "", i);
            elf64PrintSymbol(elf, &ssym[i]);
            NEW_LINE;
        }

        NEW_LINE;
    }
}

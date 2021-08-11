#include "pe64Printer.h"

#include <stdio.h>
#include <inttypes.h>

static void pe64PrintSectName(const PE64File *pe, const PESection *sect)
{
    if (pe == NULL || sect == NULL)
        return;

    switch (pe->type) {
    case PE64_EXEC:
    case PE64_SHARED:
        // For exec and shared all long name are truncated to 8 characters
        printf("%.*s", IMAGE_SIZEOF_SHORT_NAME, sect->Name);
        break;
    case PE64_OBJ:
        if (sect->Name[0] == '/')
            printf("%16s ", pe64GetLongSectName(pe, sect));
        else
            printf("%16.*s ", IMAGE_SIZEOF_SHORT_NAME, sect->Name);

        break;
    default:
        //ERROR("Unknown file type");
        break;
    }
}

void pe64PrintSection(const PE64File *pe, const PESection *sect)
{
    if (pe == NULL || sect == NULL)
        return;

    printf("\tName:\t");
    pe64PrintSectName(pe, sect);
    putchar('\n');
}

void pe64PrintSections(const PE64File *pe)
{
    if (pe == NULL || pe->sections == NULL)
        return;

    WORD i = 0;
    for (i = 0; i < pe->sectNum; ++i)
        pe64PrintSection(pe, &pe->sections[i]);
}

static void pe64PrintSymName(const PE64File *pe, const PESymbol *sym)
{
    if (pe == NULL || sym == NULL)
        return;

    if (sym->N.Name.Short)
        printf("%16.*s ", IMAGE_SIZEOF_SHORT_NAME, pe64GetShortSymName(pe, sym));
    else
        printf("%16s ", pe64GetLongSymName(pe, sym));
}

static void pe64PrintSymValue(const PESymbol *sym)
{
    if (sym == NULL)
        return;

    printf("%.16lx ", sym->Value);
}

static void pe64PrintSymSection(const PE64File *pe, const PESymbol *sym)
{
    if (pe == NULL || sym == NULL)
        return;

    SHORT sectNum = sym->SectionNumber;
    PESection *sect = pe64GetSectByNum(pe, (uint64_t)sectNum);
    switch(sectNum) {
    case IMAGE_SYM_UNDEFINED:
        printf("%16s ", "N_UNDEF");
        break;
    case IMAGE_SYM_ABSOLUTE:
        printf("%16s ", "N_ABS");
        break;
    case IMAGE_SYM_DEBUG:
        printf("%16s ", "N_DEBUG");
        break;
    default:
        pe64PrintSectName(pe, sect);
        break;
    }
}

static void pe64PrintSymType(const PESymbol *sym)
{
    if (sym == NULL)
        return;

    switch (sym->Type >> 8) {
    case IMAGE_SYM_DTYPE_NULL:
        printf("%7s:", "DT_NON");
        break;
    case IMAGE_SYM_DTYPE_POINTER:
        printf("%7s:", "DT_PTR");
        break;
    case IMAGE_SYM_DTYPE_FUNCTION:
        printf("%7s:", "DT_FCN");
        break;
    case IMAGE_SYM_DTYPE_ARRAY:
        printf("%7s:", "DT_ARY");
        break;
    default:
        break;
    }

    switch (sym->Type & 0xF) {
    case IMAGE_SYM_TYPE_NULL:
        printf("%8s ", "T_NULL");
        break;
    case IMAGE_SYM_TYPE_VOID:
        printf("%8s ", "T_VOID");
        break;
    case IMAGE_SYM_TYPE_CHAR:
        printf("%8s ", "T_CHAR");
        break;
    case IMAGE_SYM_TYPE_SHORT:
        printf("%8s ", "T_SHORT");
        break;
    case IMAGE_SYM_TYPE_INT:
        printf("%8s ", "T_INT");
        break;
    case IMAGE_SYM_TYPE_LONG:
        printf("%8s ", "T_LONG");
        break;
    case IMAGE_SYM_TYPE_FLOAT:
        printf("%8s ", "T_FLOAT");
        break;
    case IMAGE_SYM_TYPE_DOUBLE:
        printf("%8s ", "T_DOUBLE");
        break;
    case IMAGE_SYM_TYPE_STRUCT:
        printf("%8s ", "T_STRUCT");
        break;
    case IMAGE_SYM_TYPE_UNION:
        printf("%8s ", "T_UNION");
        break;
    case IMAGE_SYM_TYPE_ENUM:
        printf("%8s ", "T_ENUM");
        break;
    case IMAGE_SYM_TYPE_MOE:
        printf("%8s ", "T_MOE");
        break;
    case IMAGE_SYM_TYPE_BYTE:
        printf("%8s ", "T_BYTE");
        break;
    case IMAGE_SYM_TYPE_WORD:
        printf("%8s ", "T_WORD");
        break;
    case IMAGE_SYM_TYPE_UINT:
        printf("%8s ", "T_UINT");
        break;
    case IMAGE_SYM_TYPE_DWORD:
        printf("%8s ", "T_DWORD");
        break;
    default:
        break;
    }
}

static void pe64PrintSymStorageClass(const PESymbol *sym)
{
    if (sym == NULL)
        return;

    switch (sym->StorageClass) {
    case IMAGE_SYM_CLASS_END_OF_FUNCTION:
        printf("%8s ", "C_EFCN");
        break;
    case IMAGE_SYM_CLASS_NULL:
        printf("%8s ", "C_NULL");
        break;
    case IMAGE_SYM_CLASS_AUTOMATIC:
        printf("%8s ", "C_AUTO");
        break;
    case IMAGE_SYM_CLASS_EXTERNAL:
        printf("%8s ", "C_EXT");
        break;
    case IMAGE_SYM_CLASS_STATIC:
        printf("%8s ", "C_STAT");
        break;
    case IMAGE_SYM_CLASS_REGISTER:
        printf("%8s ", "C_REG");
        break;
    case IMAGE_SYM_CLASS_EXTERNAL_DEF:
        printf("%8s ", "C_EXTDEF");
        break;
    case IMAGE_SYM_CLASS_LABEL:
        printf("%8s ", "C_LABEL");
        break;
    case IMAGE_SYM_CLASS_UNDEFINED_LABEL:
        printf("%8s ", "C_ULABEL");
        break;
    case IMAGE_SYM_CLASS_MEMBER_OF_STRUCT:
        printf("%8s ", "C_MOS");
        break;
    case IMAGE_SYM_CLASS_ARGUMENT:
        printf("%8s ", "C_ARG");
        break;
    case IMAGE_SYM_CLASS_STRUCT_TAG:
        printf("%8s ", "C_STRTAG");
        break;
    case IMAGE_SYM_CLASS_MEMBER_OF_UNION:
        printf("%8s ", "C_MOU");
        break;
    case IMAGE_SYM_CLASS_UNION_TAG:
        printf("%8s ", "C_UNTAG");
        break;
    case IMAGE_SYM_CLASS_TYPE_DEFINITION:
        printf("%8s ", "C_TPDEF");
        break;
    case IMAGE_SYM_CLASS_UNDEFINED_STATIC:
        printf("%8s ", "C_USTATIC");
        break;
    case IMAGE_SYM_CLASS_ENUM_TAG:
        printf("%8s ", "C_ENTAG");
        break;
    case IMAGE_SYM_CLASS_MEMBER_OF_ENUM:
        printf("%8s ", "C_MOE");
        break;
    case IMAGE_SYM_CLASS_REGISTER_PARAM:
        printf("%8s ", "C_REGPARM");
        break;
    case IMAGE_SYM_CLASS_BIT_FIELD:
        printf("%8s ", "C_FIELD");
        break;
    case IMAGE_SYM_CLASS_BLOCK:
        printf("%8s ", "C_BLOCK");
        break;
    case IMAGE_SYM_CLASS_FUNCTION:
        printf("%8s ", "C_FCN");
        break;
    case IMAGE_SYM_CLASS_END_OF_STRUCT:
        printf("%8s ", "C_EOS");
        break;
    case IMAGE_SYM_CLASS_FILE:
        printf("%8s ", "C_FILE");
        break;
    case IMAGE_SYM_CLASS_SECTION:
        printf("%8s ", "C_SECTION");
        break;
    case IMAGE_SYM_CLASS_WEAK_EXTERNAL:
        printf("%8s ", "C_WEAK_EXTERNAL");
        break;
    case IMAGE_SYM_CLASS_CLR_TOKEN:
        printf("%8s ", "C_CLR_TOKEN");
        break;
    default:
        break;
    }
}

static void pe64PrintSymAuxilary(const PESymbol *sym)
{
    if (sym == NULL)
        return;

    printf("%8"PRIu8, sym->NumberOfAuxSymbols);
}

void pe64PrintSymbol(const PE64File *pe, const PESymbol *sym)
{
    if (pe == NULL || sym == NULL)
        return;

    pe64PrintSymName(pe, sym);
    pe64PrintSymValue(sym);
    pe64PrintSymSection(pe, sym);
    pe64PrintSymType(sym);
    pe64PrintSymStorageClass(sym);
    pe64PrintSymAuxilary(sym);

    putchar('\n');
}

void pe64PrintAuxSymSect(const PE64File *pe, const PEAuxSymbol *auxSym)
{
    if (pe == NULL || auxSym == NULL)
        return;

    printf("\t Len: %.8lx", auxSym->Section.Length);
    printf("\t Relocs: %.4hx", auxSym->Section.NumberOfRelocations);
    printf("\t Lines: %.4hx", auxSym->Section.NumberOfLinenumbers);
    printf("\t CheckSum: %.8lx", auxSym->Section.CheckSum);
    printf("\t AssocNum: %.4hu", auxSym->Section.Number);
    printf("\t Selec: %.2hhx", auxSym->Section.Selection);
}

void pe64PrintAuxSymFile(const PE64File *pe, const PEAuxSymbol *auxSym)
{
    if (pe == NULL || auxSym == NULL)
        return;

    printf("\t %s", auxSym->File.Name);
}

void pe64PrintAuxSymbol(const PE64File *pe, const PESymbol *sym, const PEAuxSymbol *auxSym)
{
    if (pe == NULL || sym == NULL || auxSym == NULL)
        return;

    switch (sym->StorageClass) {
    case IMAGE_SYM_CLASS_STATIC:
        pe64PrintAuxSymSect(pe, auxSym);
        break;
    case IMAGE_SYM_CLASS_FILE:
        pe64PrintAuxSymFile(pe, auxSym);
        break;
    default:
        break;
    }

    putchar('\n');
}

void pe64PrintSymbols(const PE64File *pe)
{
    if (pe == NULL || pe->symtab == NULL)
        return;

    printf("%8s ", "Indx");
    printf("%16s ", "Name");
    printf("%16s ", "Value");
    printf("%16s ", "Section");
    printf("%16s ", "Type");
    printf("%8s ", "Storage");
    printf("%8s\n", "Aux");
    printf("-------- ");
    printf("---------------- ");
    printf("---------------- ");
    printf("---------------- ");
    printf("---------------- ");
    printf("-------- ");
    printf("--------\n");

    DWORD num = pe->symNum;
    DWORD i = 0;
    for (i = 0; i < num; ++i) {
        PESymbol *sym = pe->symtab + i;

        printf("%.8lu ", i);
        pe64PrintSymbol(pe, sym);
        DWORD j = sym->NumberOfAuxSymbols;
        for (; j; j--) {
            PEAuxSymbol *auxSym = (PEAuxSymbol *)(pe->symtab + i + j);
            pe64PrintAuxSymbol(pe, sym, auxSym);
        }

        i += sym->NumberOfAuxSymbols;
    }
}


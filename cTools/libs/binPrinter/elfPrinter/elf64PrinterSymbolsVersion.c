#include "mem.h"
#include "elf64Printer.h"

#include "stdbool.h"
#include <inttypes.h>

void elf64PrintSymbolsVersion(const Elf64File *elf)
{
    /***
     * The special section .gnu.version which has a section type of SHT_GNU_versym
     * shall contain the Symbol Version Table. This section shall have the same
     * number of entries as the Dynamic Symbol Table in the .dynsym section.
     */
    Elf64Shdr *verSect = elf64GetSectByType(elf, SHT_GNU_versym);
    if (verSect == NULL)
        return;

    uint16_t *ver = elf64ReadSect(elf, verSect);
    if (ver == NULL)
        return;

    printf("Version symbols section '.gnu.version' contains %"PRIu64" entries:\n", elf->dynsymnum);
    printf(" Addr: 0x%.16"PRIx64"  Offset: 0x%.6"PRIx64"  Link: %"PRIu32" (%s)"
        , verSect->sh_addr
        , verSect->sh_offset
        , verSect->sh_link
        , elf64GetSectName(elf, elf->sections + verSect->sh_link));

    uint64_t i = 0;
    for (i = 0; i < elf->dynsymnum; ++i) {
        if (i % 4 == 0)
            printf("\n  %.3"PRIx64":", i);

        switch (ver[i]) {
        case 0:
            printf("   0 (*local*)    ");
            break;
        case 1:
            printf("   1 (*global*)   ");
            break;
        default:
            {
            uint16_t version = (uint16_t)(ver[i] & VERSYM_VERSION);
            printf("%4"PRIx16"%c(%s)", version
                                      , ver[i] & VERSYM_HIDDEN ? 'h' : ' '
                                      , elf64GetVerNameBySymVersion(elf, version));
            break;
            }
        }
    }

    NEW_LINE;
    NEW_LINE;
}

static
void elf64PrintVerFlags(uint16_t flags)
{
    if (flags == 0)
        printf("none");

    bool needDelimeter = false;
    if (flags & VER_FLG_BASE) {
        needDelimeter = true;
        printf("BASE");
    }

    if (flags & VER_FLG_WEAK) {
        if (needDelimeter)
            printf(" | ");
        else
            needDelimeter = true;
        printf("WEAK");
    }

    if (flags & VER_FLG_INFO) {
        if (needDelimeter)
            printf(" | ");
        else
            needDelimeter = true;
        printf("INFO");
    }

    if (flags & ~(VER_FLG_BASE | VER_FLG_WEAK | VER_FLG_INFO)) {
        if (needDelimeter)
            printf(" | ");
        else
            needDelimeter = true;
        printf("<unknown>");
    }
}

void elf64PrintSymbolsVersionR(const Elf64File *elf)
{
    /***
     * The special section .gnu.version_r which has a section type of
     * SHT_GNU_verneed shall contain required symbol version definitions.
     * The number of entries in this section shall be contained in the
     * DT_VERNEEDNUM entry of the Dynamic Section .dynamic. The sh_link member
     * of the section header (see figure 4-8 in System V ABI) shall point to
     * the section that contains the strings referenced by this section.
    */
    uint64_t i = 0;
    while (elf->dynamic[i++].d_tag != DT_VERNEEDNUM);
    uint64_t num = elf->dynamic[--i].d_un.d_val;

    Elf64Shdr *verneedSect = elf64GetSectByType(elf, SHT_GNU_verneed);
    if (verneedSect == NULL)
        return;

    Elf64Verneed *verneed = elf64ReadSect(elf, verneedSect);
    if (verneed == NULL)
        return;

    printf("Version needs section '%s' contains %"PRIu64" entry:\n"
        , elf64GetSectName(elf, verneedSect)
        , num);
    printf(" Addr: 0x%.16"PRIx64"  Offset: 0x%.6"PRIx64"  Link: %"PRIu32" (%s)\n"
        , verneedSect->sh_addr
        , verneedSect->sh_offset
        , verneedSect->sh_link
        , elf64GetSectName(elf, elf->sections + verneedSect->sh_link));

    Elf64Verneed *p = verneed;
    char *verneedStr = elf->dynSymNameTab;
    for (i = 0; i < num; ++i) {
        printf("  %.6"PRIx64": Version: %"PRIu16"  File: %s  Cnt: %"PRIu16"\n"
            , i
            , p->vn_version
            , verneedStr + p->vn_file
            , p->vn_cnt);

        Elf64Vernaux *aux = (Elf64Vernaux*)((size_t)p + p->vn_aux);
        uint64_t j = 0;
        for (j = 0; j < p->vn_cnt; ++j) {
            printf("  %#06"PRIx64":   Name: %s  Flags: "
                , p->vn_aux + j*sizeof(Elf64Vernaux)
                , verneedStr + aux->vna_name);
            elf64PrintVerFlags(aux->vna_flags);
            printf("  Version: %"PRIu16"\n", aux->vna_other);
            aux = (Elf64Vernaux*)((size_t)aux + aux->vna_next);
        }
        p = (Elf64Verneed*)((size_t)p + p->vn_next);
    }

    NEW_LINE;

    Free(verneed);
}


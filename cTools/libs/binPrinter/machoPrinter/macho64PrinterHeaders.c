/***
 * MIT License
 *
 * Copyright (c) 2023 Konychev Valerii
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
#include "macho64Printer.h"

#include <stdio.h>
#include <stdbool.h>

static const char UNKNOWN_STR[] = "UNKNOWN";

const Macho64HeaderFlag MACHO64_HEADERS_FLAGS[] = {
    { MH_NOUNDEFS,                      "NOUNDEFS"                      },
    { MH_INCRLINK,                      "INCRLINK"                      },
    { MH_DYLDLINK,                      "DYLDLINK"                      },
    { MH_BINDATLOAD,                    "BINDATLOAD"                    },
    { MH_PREBOUND,                      "PREBOUND"                      },
    { MH_SPLIT_SEGS,                    "SPLIT_SEGS"                    },
    { MH_LAZY_INIT,                     "LAZY_INIT"                     },
    { MH_TWOLEVEL,                      "TWOLEVEL"                      },
    { MH_FORCE_FLAT,                    "FORCE_FLAT"                    },
    { MH_NOMULTIDEFS,                   "NOMULTIDEFS"                   },
    { MH_NOFIXPREBINDING,               "NOFIXPREBINDING"               },
    { MH_PREBINDABLE,                   "PREBINDABLE"                   },
    { MH_ALLMODSBOUND,                  "ALLMODSBOUND"                  },
    { MH_SUBSECTIONS_VIA_SYMBOLS,       "SUBSECTIONS_VIA_SYMBOLS"       },
    { MH_CANONICAL,                     "CANONICAL"                     },
    { MH_WEAK_DEFINES,                  "WEAK_DEFINES"                  },
    { MH_BINDS_TO_WEAK,                 "BINDS_TO_WEAK"                 },
    { MH_ALLOW_STACK_EXECUTION,         "ALLOW_STACK_EXECUTION"         },
    { MH_ROOT_SAFE,                     "ROOT_SAFE"                     },
    { MH_SETUID_SAFE,                   "SETUID_SAFE"                   },
    { MH_NO_REEXPORTED_DYLIBS,          "NO_REEXPORTED_DYLIBS"          },
    { MH_PIE,                           "PIE"                           },
    { MH_DEAD_STRIPPABLE_DYLIB,         "DEAD_STRIPPABLE_DYLIB"         },
    { MH_HAS_TLV_DESCRIPTORS,           "HAS_TLV_DESCRIPTORS"           },
    { MH_NO_HEAP_EXECUTION,             "NO_HEAP_EXECUTION"             },
    { MH_APP_EXTENSION_SAFE,            "APP_EXTENSION_SAFE"            },
    { MH_NLIST_OUTOFSYNC_WITH_DYLDINFO, "NLIST_OUTOFSYNC_WITH_DYLDINFO" },
    { MH_SIM_SUPPORT,                   "SIM_SUPPORT"                   },
    { MH_DYLIB_IN_CACHE,                "DYLIB_IN_CACHE"                },
};

static
void macho64PrintMagicNum(const Macho64Header *h)
{
    printf("0x%x", h->magic);
}

const static
char *macho64GetCpuTypeStr(const Macho64Header *h)
{
    switch (h->cputype) {
    case CPU_TYPE_ANY:
        return "ANY";
    case CPU_TYPE_VAX:
        return "VAX";
    case CPU_TYPE_MC680x0:
        return "MC680x0";
    case CPU_TYPE_X86:
        return "X86";
    case CPU_TYPE_X86_64:
        return "X86_64";
    case CPU_TYPE_MC98000:
        return "MC98000";
    case CPU_TYPE_HPPA:
        return "HPPA";
    case CPU_TYPE_ARM:
        return "ARM";
    case CPU_TYPE_ARM64:
        return "ARM64";
    case CPU_TYPE_MC88000:
        return "MC88000";
    case CPU_TYPE_SPARC:
        return "SPARC";
    case CPU_TYPE_I860:
        return "I860";
    case CPU_TYPE_POWERPC:
        return "POWERPC";
    case CPU_TYPE_POWERPC64:
        return "POWERPC64";
    default:
        return UNKNOWN_STR;
    }
}

const static
char *macho64GetCpuSubTypeStr(const Macho64Header *h)
{
    switch (h->cputype) {
    case CPU_TYPE_ANY:
        return UNKNOWN_STR;
    case CPU_TYPE_VAX:
        switch (h->cpusubtype) {
        case CPU_SUBTYPE_VAX_ALL:
            return "VAX_ALL";
        case CPU_SUBTYPE_VAX780:
            return "VAX780";
        case CPU_SUBTYPE_VAX785:
            return "VAX785";
        case CPU_SUBTYPE_VAX750:
            return "VAX750";
        case CPU_SUBTYPE_VAX730:
            return "VAX730";
        case CPU_SUBTYPE_UVAXI:
            return "UVAXI";
        case CPU_SUBTYPE_UVAXII:
            return "UVAXII";
        case CPU_SUBTYPE_VAX8200:
            return "VAX8200";
        case CPU_SUBTYPE_VAX8500:
            return "VAX8500";
        case CPU_SUBTYPE_VAX8600:
            return "VAX8600";
        case CPU_SUBTYPE_VAX8650:
            return "VAX8650";
        case CPU_SUBTYPE_VAX8800:
            return "VAX8800";
        case CPU_SUBTYPE_UVAXIII:
            return "UVAXIII";
        default:
            return UNKNOWN_STR;
        }
    case CPU_TYPE_MC680x0:
        switch (h->cpusubtype) {
        case CPU_SUBTYPE_MC680x0_ALL:
            return "MC680x0_ALL";
        case CPU_SUBTYPE_MC68040:
            return "MC68040";
        case CPU_SUBTYPE_MC68030_ONLY:
            return "MC68030_ONLY";
        default:
            return UNKNOWN_STR;
        }
    case CPU_TYPE_X86:
        switch (h->cpusubtype) {
        case CPU_SUBTYPE_386:
            return "386";
        case CPU_SUBTYPE_486:
            return "486";
        case CPU_SUBTYPE_486SX:
            return "486SX";
        case CPU_SUBTYPE_PENT:
            return "PENT";
        case CPU_SUBTYPE_PENTPRO:
            return "PENTPRO";
        case CPU_SUBTYPE_PENTII_M3:
            return "PENTII_M3";
        case CPU_SUBTYPE_PENTII_M5:
            return "PENTII_M5";
        case CPU_SUBTYPE_CELERON:
            return "CELERON";
        case CPU_SUBTYPE_CELERON_MOBILE:
            return "CELERON_MOBILE";
        case CPU_SUBTYPE_PENTIUM_3:
            return "PENTIUM_3";
        case CPU_SUBTYPE_PENTIUM_3_M:
            return "PENTIUM_3_M";
        case CPU_SUBTYPE_PENTIUM_3_XEON:
            return "PENTIUM_3_XEON";
        case CPU_SUBTYPE_PENTIUM_M:
            return "PENTIUM_M";
        case CPU_SUBTYPE_PENTIUM_4:
            return "PENTIUM_4";
        case CPU_SUBTYPE_PENTIUM_4_M:
            return "PENTIUM_4_M";
        case CPU_SUBTYPE_ITANIUM:
            return "ITANIUM";
        case CPU_SUBTYPE_ITANIUM_2:
            return "ITANIUM_2";
        case CPU_SUBTYPE_XEON:
            return "XEON";
        case CPU_SUBTYPE_XEON_MP:
            return "XEON_MP";
        default:
            return UNKNOWN_STR;
        }
    case CPU_TYPE_X86_64:
        switch (h->cpusubtype) {
        case CPU_SUBTYPE_X86_64_ALL:
            return "X86_64_ALL";
        case CPU_SUBTYPE_X86_64_H:
            /* Haswell feature subset */
            return "X86_64_H";
        default:
            return UNKNOWN_STR;
        }
    case CPU_TYPE_MC98000:
        switch (h->cpusubtype) {
        case CPU_SUBTYPE_MC98000_ALL:
            return "MC98000_ALL";
        case CPU_SUBTYPE_MC98601:
            return "MC98601";
        default:
            return UNKNOWN_STR;
        }
    case CPU_TYPE_HPPA:
        switch (h->cpusubtype) {
        case CPU_SUBTYPE_HPPA_ALL:
            return "HPPA_ALL";
        case CPU_SUBTYPE_HPPA_7100LC:
            return "HPPA_7100LC";
        default:
            return UNKNOWN_STR;
        }
    case CPU_TYPE_ARM:
        switch (h->cpusubtype) {
        case CPU_SUBTYPE_ARM_ALL:
            return "ARM_ALL";
        case CPU_SUBTYPE_ARM_V4T:
            return "ARM_V4T";
        case CPU_SUBTYPE_ARM_V6:
            return "ARM_V6";
        case CPU_SUBTYPE_ARM_V5TEJ:
            return "ARM_V5TEJ";
        case CPU_SUBTYPE_ARM_XSCALE:
            return "ARM_XSCALE";
        case CPU_SUBTYPE_ARM_V7:
            return "ARM_V7";
        case CPU_SUBTYPE_ARM_V7F:
            return "ARM_V7F";
        case CPU_SUBTYPE_ARM_V7S:
            return "ARM_V7S";
        case CPU_SUBTYPE_ARM_V7K:
            return "ARM_V7K";
        case CPU_SUBTYPE_ARM_V6M:
            return "ARM_V6M";
        case CPU_SUBTYPE_ARM_V7M:
            return "ARM_V7M";
        case CPU_SUBTYPE_ARM_V7EM:
            return "ARM_V7EM";
        case CPU_SUBTYPE_ARM_V8:
            return "ARM_V8";
        default:
            return UNKNOWN_STR;
        }
    case CPU_TYPE_ARM64:
        switch (h->cpusubtype) {
        case CPU_SUBTYPE_ARM64_ALL:
            return "ARM64_ALL";
        case CPU_SUBTYPE_ARM64_V8:
            return "ARM64_V8";
        default:
            return UNKNOWN_STR;
        }
    case CPU_TYPE_MC88000:
        switch (h->cpusubtype) {
        case CPU_SUBTYPE_MC88000_ALL:
            return "MC88000_ALL";
        case CPU_SUBTYPE_MC88100:
            return "MC88100";
        case CPU_SUBTYPE_MC88110:
            return "MC88110";
        default:
            return UNKNOWN_STR;
        }
    case CPU_TYPE_SPARC:
        return "SPARC_ALL";
    case CPU_TYPE_I860:
        switch (h->cpusubtype) {
        case CPU_SUBTYPE_I860_ALL:
            return "I860_ALL";
        case CPU_SUBTYPE_I860_860:
            return "I860_860";
        default:
            return UNKNOWN_STR;
        }
    case CPU_TYPE_POWERPC:
    case CPU_TYPE_POWERPC64:
        switch (h->cpusubtype) {
        case CPU_SUBTYPE_POWERPC_ALL:
            return "POWERPC_ALL";
        case CPU_SUBTYPE_POWERPC_601:
            return "POWERPC_601";
        case CPU_SUBTYPE_POWERPC_602:
            return "POWERPC_602";
        case CPU_SUBTYPE_POWERPC_603:
            return "POWERPC_603";
        case CPU_SUBTYPE_POWERPC_603e:
            return "POWERPC_603e";
        case CPU_SUBTYPE_POWERPC_603ev:
            return "POWERPC_603ev";
        case CPU_SUBTYPE_POWERPC_604:
            return "POWERPC_604";
        case CPU_SUBTYPE_POWERPC_604e:
            return "POWERPC_604e";
        case CPU_SUBTYPE_POWERPC_620:
            return "POWERPC_620";
        case CPU_SUBTYPE_POWERPC_750:
            return "POWERPC_750";
        case CPU_SUBTYPE_POWERPC_7400:
            return "POWERPC_7400";
        case CPU_SUBTYPE_POWERPC_7450:
            return "POWERPC_7450";
        case CPU_SUBTYPE_POWERPC_970:
            return "POWERPC_970";
        default:
            return UNKNOWN_STR;
        }
    default:
        return UNKNOWN_STR;
    }
}

const static
char *macho64GetFileTypeStr(const Macho64Header *h)
{
    switch (h->filetype) {
    case MH_OBJECT:
        return "OBJECT";
    case MH_EXECUTE:
        return "EXECUTE";
    case MH_FVMLIB:
        return "FVMLIB";
    case MH_CORE:
        return "CORE";
    case MH_PRELOAD:
        return "PRELOAD";
    case MH_DYLIB:
        return "DYLIB";
    case MH_DYLINKER:
        return "DYLINKER";
    case MH_BUNDLE:
        return "BUNDLE";
    case MH_DYLIB_STUB:
        return "DYLIB_STUB";
    case MH_DSYM:
        return "DSYM";
    case MH_KEXT_BUNDLE:
        return "KEXT_BUNDLE";
    default:
        return UNKNOWN_STR;
    }
}

static
void macho64PrintHeaderFlagsStr(const Macho64Header *h)
{
    int i = sizeof(MACHO64_HEADERS_FLAGS) / sizeof(Macho64HeaderFlag) - 1;
    bool notFirstFlags = false;
    for (;i >= 0; --i) {
        if (h->flags & MACHO64_HEADERS_FLAGS[i].flag) {
            if (notFirstFlags)
                printf(" | ");
            printf("%s", MACHO64_HEADERS_FLAGS[i].str);
            notFirstFlags = true;
        }
    }
}

void macho64PrintHeader(const Macho64File *mf)
{
    Macho64Header *h = mf->header;

    printf("macho64 header:\n");
    printf("%10s: ", "magic");
    macho64PrintMagicNum(h);
    NEW_LINE;

    printf("%10s: 0x%.8x (%s)\n", "cputype"   , h->cputype, macho64GetCpuTypeStr(h));
    printf("%10s: 0x%.8x (%s)\n", "cpusubtype", h->cpusubtype, macho64GetCpuSubTypeStr(h));
    printf("%10s: 0x%.8x (%s)\n", "filetype"  , h->filetype, macho64GetFileTypeStr(h));
    printf("%10s: 0x%.8x\n"     , "ncmds"     , h->ncmds);
    printf("%10s: 0x%.8x\n"     , "sizeofcmds", h->sizeofcmds);

    printf("%10s: 0x%.8x ("      , "flags"     , h->flags);
    macho64PrintHeaderFlagsStr(h);
    printf(")\n");

    NEW_LINE;
}


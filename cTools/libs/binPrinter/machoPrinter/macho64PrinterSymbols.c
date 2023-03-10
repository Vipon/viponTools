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

#include <mach-o/stab.h>
#include "macho64Printer.h"

#include <stdbool.h>
#include <inttypes.h>

const Macho64StubType MACHO64_STUB_TYPE[] = {
    { N_GSYM    , "N_GSYM"      },
    { N_FNAME   , "N_FNAME"     },
    { N_FUN     , "N_FUN"       },
    { N_STSYM   , "N_STSYM"     },
    { N_LCSYM   , "N_LCSYM"     },
    { N_BNSYM   , "N_BNSYM"     },
    { N_AST     , "N_AST"       },
    { N_OPT     , "N_OPT"       },
    { N_RSYM    , "N_RSYM"      },
    { N_SLINE   , "N_SLINE"     },
    { N_ENSYM   , "N_ENSYM"     },
    { N_SSYM    , "N_SSYM"      },
    { N_SO      , "N_SO"        },
    { N_OSO     , "N_OSO"       },
    { N_LSYM    , "N_LSYM"      },
    { N_BINCL   , "N_BINCL"     },
    { N_SOL     , "N_SOL"       },
    { N_PARAMS  , "N_PARAMS"    },
    { N_VERSION , "N_VERSION"   },
    { N_OLEVEL  , "N_OLEVEL"    },
    { N_PSYM    , "N_PSYM"      },
    { N_EINCL   , "N_EINCL"     },
    { N_ENTRY   , "N_ENTRY"     },
    { N_LBRAC   , "N_LBRAC"     },
    { N_EXCL    , "N_EXCL"      },
    { N_RBRAC   , "N_RBRAC"     },
    { N_BCOMM   , "N_BCOMM"     },
    { N_ECOMM   , "N_ECOMM"     },
    { N_ECOML   , "N_ECOML"     },
    { N_LENG    , "N_LENG"      },
    { N_PC      , "N_PC"        },
};

const Macho64SymType MACHO64_SYM_TYPE[] = {
    { N_UNDF,   "N_UNDF"    },
    { N_ABS,    "N_ABS"     },
    { N_SECT,   "N_SECT"    },
    { N_PBUD,   "N_PBUD"    },
    { N_INDR,   "N_INDR"    },
};

static
void macho64PrintSymbolType(const Macho64Sym *ms)
{
    bool needColomn = false;
    if (IS_MACHO64_SYM_DEBUG(*ms)) {
        // In this case n_type field interpreted as stab value <mach-o/stab.h>
        uint8_t i = 0;
        for (i = 0; i < sizeof(MACHO64_STUB_TYPE)/sizeof(Macho64StubType); ++i) {
            if (ms->n_type == MACHO64_STUB_TYPE[i].flag) {
                printf("%s", MACHO64_STUB_TYPE[i].str);
                return;
            }
        }

        printf("N_STUB");
        return;
    }

    if (ms->n_type & N_PEXT) {
        printf("N_PEXT");
        needColomn = true;
    }

    uint8_t i = 0;
    for (i = 0; i < sizeof(MACHO64_SYM_TYPE)/sizeof(Macho64SymType); ++i) {
        if (MACHO64_SYM_TYPE(*ms) == MACHO64_SYM_TYPE[i].flag) {
            if (needColomn) {
                printf(" | ");
            }
            needColomn = true;
            printf("%s", MACHO64_SYM_TYPE[i].str);
        }
    }

    if (IS_MACHO64_SYM_EXT(*ms)) {
        if (needColomn) {
            printf(" | ");
        }
        needColomn = true;
        printf("N_EXT");
    }
}

void macho64PrintSymbol(const Macho64File *mf, const Macho64Sym *ms)
{
    char *symname = mf->symNameTab;
    printf("%9s: %s\n", "name", &symname[ms->n_un.n_strx]);
    printf("%9s: 0x%.2"PRIx8" ", "type", ms->n_type);
    macho64PrintSymbolType(ms);
    NEW_LINE;

    printf("%9s: 0x%.2"PRIx8" ", "sect", ms->n_sect);
    if (ms->n_sect == NO_SECT) {
        printf("NO_SECT\n");
    } else {
        const Macho64Sect *sect = macho64GetSectByIndx(mf, ms->n_sect);
        printf("%s,%s\n", sect->segname, sect->sectname);
    }

    printf("%9s: 0x%"PRIx16, "desc", ms->n_desc);
    if (IS_MACHO64_DSYM_LAZY(*ms) || IS_MACHO64_DSYM_NON_LAZY(*ms)) {
        uint32_t dylibNum = GET_LIBRARY_ORDINAL(ms->n_desc) - 1;
        printf(" (%s)", macho64GetDylibName(mf->dylibCom[dylibNum]));
    }
    NEW_LINE;
    printf("%9s: 0x%"PRIx64"\n", "value", ms->n_value);
}

void macho64PrintSymbols(const Macho64File *mf)
{
    uint32_t i = 0;
    uint32_t nsyms = macho64GetAmountSSym(mf);
    printf("Symbol table:\n");
    for (i = 0; i < nsyms; ++i) {
        printf("%"PRIu32".\n", i);
        macho64PrintSymbol(mf, mf->symtab + i);
    }
}


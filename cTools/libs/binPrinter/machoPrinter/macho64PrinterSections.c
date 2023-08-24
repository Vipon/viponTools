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

#include "macho64Printer.h"

#include <stdio.h>
#include <stdbool.h>
#include <inttypes.h>

const Macho64SectionFlag MACHO64_SECTION_FLAGS[] = {
    // Section types
    { S_REGULAR                             , "REG"                         },
    { S_ZEROFILL                            , "ZERO"                        },
    { S_CSTRING_LITERALS                    , "CSTRING"                     },
    { S_4BYTE_LITERALS                      , "4BYTE"                       },
    { S_8BYTE_LITERALS                      , "8BYTE"                       },
    { S_LITERAL_POINTERS                    , "LITERAL_POINT"               },
    { S_NON_LAZY_SYMBOL_POINTERS            , "NON_LAZY_SYM"                },
    { S_LAZY_SYMBOL_POINTERS                , "LAZY_SYM"                    },
    { S_SYMBOL_STUBS                        , "SYM_STUBS"                   },
    { S_MOD_INIT_FUNC_POINTERS              , "MOD_INIT_FUNC_POINT"         },
    { S_MOD_TERM_FUNC_POINTERS              , "MOD_TERM_FUNC_POINT"         },
    { S_COALESCED                           , "COALESCED"                   },
    { S_GB_ZEROFILL                         , "GB_ZERO"                     },
    { S_INTERPOSING                         , "INTERPOSING"                 },
    { S_16BYTE_LITERALS                     , "16BYTE"                      },
    { S_DTRACE_DOF                          , "DTRACE_DOF"                  },
    { S_LAZY_DYLIB_SYMBOL_POINTERS          , "LAZY_DYLIB_SYM_POINT"        },
    { S_THREAD_LOCAL_REGULAR                , "THREAD_LOCAL_REG"            },
    { S_THREAD_LOCAL_ZEROFILL               , "THREAD_LOCAL_ZERO"           },
    { S_THREAD_LOCAL_VARIABLES              , "THREAD_LOCAL_VAR"            },
    { S_THREAD_LOCAL_VARIABLE_POINTERS      , "THREAD_LOCAL_VAR_POINT"      },
    { S_THREAD_LOCAL_INIT_FUNCTION_POINTERS , "THREAD_LOCAL_INIT_FUNC_POINT"},
    { S_INIT_FUNC_OFFSETS                   , "INIT_FUNC_OFFSETS"           },
    // Section attributes
    { S_ATTR_LOC_RELOC                      , "ATTR_LOC_RELOC"              },
    { S_ATTR_EXT_RELOC                      , "ATTR_EXT_RELOC"              },
    { S_ATTR_SOME_INSTRUCTIONS              , "ATTR_SOME_INSTR"             },
    { S_ATTR_DEBUG                          , "ATTR_DEBUG"                  },
    { S_ATTR_SELF_MODIFYING_CODE            , "ATTR_SELF_MODIFYING_CODE"    },
    { S_ATTR_LIVE_SUPPORT                   , "ATTR_LIVE_SUPPORT"           },
    { S_ATTR_NO_DEAD_STRIP                  , "ATTR_NO_DEAD_STRIP"          },
    { S_ATTR_STRIP_STATIC_SYMS              , "ATTR_STRIP_STATIC_SYMS"      },
    { S_ATTR_NO_TOC                         , "ATTR_NO_TOC"                 },
    { S_ATTR_PURE_INSTRUCTIONS              , "ATTR_PURE_INSTR"             },
};

static
void macho64PrintSectionFlags(const Macho64Sect *sect)
{
    printf("%s", MACHO64_SECTION_FLAGS[MACHO64_SECT_TYPE(sect)].str);

    int i = sizeof(MACHO64_SECTION_FLAGS) / sizeof(Macho64SectionFlag) - 1;
    for (;i > S_INIT_FUNC_OFFSETS; --i) {
        if (sect->flags & MACHO64_SECTION_FLAGS[i].flag) {
            printf(" | %s", MACHO64_SECTION_FLAGS[i].str);
        }
    }
}

static
void macho64PrintSectionRes1(const Macho64Sect *sect)
{
    printf("%10s: %"PRIu32, "reserved1" , sect->reserved1);
    if (IS_MACHO64_SECT_INDIRECT_SYMBOLS(sect)) {
        printf(" (index into indirect symbol table)");
    }
}

static
void macho64PrintSectionRes2(const Macho64Sect *sect)
{
    printf("%10s: %"PRIu32, "reserved2" , sect->reserved2);
    if (IS_MACHO64_SECT_STUBS(sect)) {
        printf(" (size of stubs)");
    }
}

void macho64PrintSection(const Macho64Sect *sect)
{
    printf("%10s: %s\n", "sectname", sect->sectname);
    printf("%10s: %s\n", "segname" , sect->segname);
    printf("%10s: 0x%"PRIx64"\n", "addr" , sect->addr);
    printf("%10s: 0x%"PRIx64"\n", "size" , sect->size);
    printf("%10s: %"PRIu32"\n", "offset" , sect->offset);
    printf("%10s: %"PRIu32"\n", "align" , 1 << sect->align);
    printf("%10s: %"PRIu32"\n", "reloff" , sect->reloff);
    printf("%10s: %"PRIu32"\n", "nreloc" , sect->nreloc);
    printf("%10s: 0x%.8"PRIx32, "flags" , sect->flags);
    if (sect->flags) {
        SPACE;
        macho64PrintSectionFlags(sect);
    }
    NEW_LINE;
    macho64PrintSectionRes1(sect);
    NEW_LINE;
    macho64PrintSectionRes2(sect);
    NEW_LINE;
}

void macho64PrintSectionsInSegment(const Macho64Seg *seg)
{
    Macho64Sect *sect = (Macho64Sect*)((size_t)seg + sizeof(Macho64Seg));

    uint32_t i = 0;
    for (i = 0; i < seg->nsects; ++i) {
        printf("Section:\n");
        macho64PrintSection(&sect[i]);
    }
}

void macho64PrintSections(const Macho64File *mf)
{
    uint32_t sectNum = 1; // Section numbering begins at 1
    FOREACH_LOAD_COMMAND(mf,
        if (lcom->cmd == LC_SEGMENT_64) {
            Macho64Seg *seg = (Macho64Seg*)lcom;
            Macho64Sect *sect = (Macho64Sect*)((size_t)seg + sizeof(Macho64Seg));

            uint32_t j = 0;
            for (j = 0; j < seg->nsects; ++j) {
                printf("Section %"PRIu32":\n", sectNum);
                ++sectNum;
                macho64PrintSection(&sect[j]);
            }
        }
    )

    NEW_LINE;
}


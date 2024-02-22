/***
 * MIT License
 *
 * Copyright (c) 2024 Konychev Valera
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
 { * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR AARCH64_INSTR_TYPE_CBR },
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include "comdef.h"

#include <stdint.h>

#if defined(__APPLE__) && defined(__MACH__)
# define MC_STRUCT_SEGSECT   "__DATA,__mc_struct"
# define MC_STRUCT_SECTION   "__mc_struct"
# define MOD_SECT_FLAGS      "regular"

# define MOD_CODE_SEGSECT    "__TEXT,__mod_code"
# define MOD_CODE_SECTION    "__mod_code"
# define MOD_CODE_SECT_FLAGS "regular,pure_instructions"

#elif defined(__ELF__)
# define MC_STRUCT_SEGSECT   ".mc_struct"
# define MC_STRUCT_SECTION   ".mc_struct"
# define MOD_SECT_FLAGS      "\"aw\""

# define MOD_CODE_SEGSECT    ".mod_code"
# define MOD_CODE_SECTION    ".mod_code"
# define MOD_CODE_SECT_FLAGS "\"axw\""

#elif defined(__WIN__)
# define MC_STRUCT_SEGSECT   ".mc_struct"
# define MC_STRUCT_SECTION   ".mc_struct"
# define MOD_SECT_FLAGS      "\"aw\""

# define MOD_CODE_SEGSECT    ".mod_code"
# define MOD_CODE_SECTION    ".mod_code"
# define MOD_CODE_SECT_FLAGS "\"axw\""

#else
# error "Uknonw binary format"

#endif

#if AARCH64_DEFINED == 1
# define MOD_CODE(code)                                             \
    DEF_GUARD(                                                      \
        asm volatile(                                               \
        "0:\n"                                                      \
        PUSHSECTION" "MC_STRUCT_SEGSECT", "MOD_SECT_FLAGS"\n"       \
            ".align 8\n"                                            \
            /* .insert_point */                                     \
            ".quad 0b\n"                                            \
            /* .start */                                            \
            ".quad 1f\n"                                            \
            /* .end */                                              \
            ".quad 2f\n"                                            \
        POPSECTION"\n"                                              \
        PUSHSECTION" "MOD_CODE_SEGSECT", "MOD_CODE_SECT_FLAGS "\n"  \
        "1:\n"                                                      \
        ".align 8\n"                                                \
        "stp x30, x29, [sp, #-16]!\n"                               \
        ::: "memory"                                                \
        );                                                          \
        DEF_GUARD(                                                  \
            code;                                                   \
        )                                                           \
        asm volatile(                                               \
        "2:\n"                                                      \
        "ldp x30, x29, [sp], #16\n"                                 \
        "ret\n"                                                     \
        POPSECTION"\n"                                              \
        ::: "memory"                                                \
        );                                                          \
    )
#elif X86_64_DEFINED == 1
# define MOD_CODE(code)                                             \
    DEF_GUARD(                                                      \
        asm volatile(                                               \
        "0:\n"                                                      \
        PUSHSECTION" "MC_STRUCT_SEGSECT", "MOD_SECT_FLAGS"\n"       \
            ".align 8\n"                                            \
            /* .insert_point */                                     \
            ".quad 0b\n"                                            \
            /* .start */                                            \
            ".quad 1f\n"                                            \
            /* .end */                                              \
            ".quad 2f\n"                                            \
        POPSECTION"\n"                                              \
        PUSHSECTION" "MOD_CODE_SEGSECT", "MOD_CODE_SECT_FLAGS "\n"  \
        "1:\n"                                                      \
        ".align 8\n"                                                \
        ::: "memory"                                                \
        );                                                          \
        DEF_GUARD(                                                  \
            code;                                                   \
        )                                                           \
        asm volatile(                                               \
        "2:\n"                                                      \
        "ret\n"                                                     \
        POPSECTION"\n"                                              \
        ::: "memory"                                                \
        );                                                          \
    )
#else
# error "Unknown machine type"
#endif

typedef struct {
    uint8_t  *insert_point;
    uint8_t  *start;
    uint8_t  *end;
} PACKED mod_code_t;

EXPORT_VAR
extern mod_code_t *mc;
EXPORT_VAR
extern uint64_t num_mc;

EXPORT_FUNC int
mod_code_init(const char *fn);

EXPORT_FUNC void
mod_code_print(mod_code_t *mc);

EXPORT_FUNC void
mod_code_dump(void);


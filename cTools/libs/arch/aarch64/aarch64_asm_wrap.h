/***
 * MIT License
 *
 * Copyright (c) 2023 Konychev Valera
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

#ifndef __AARCH64_ASM_WRAP_H
#define __AARCH64_ASM_WRAP_H

#include "comdef.h"

#include <assert.h>
#include <stdint.h>

#ifdef KEYSTONE_ASM
# include <keystone/keystone.h>
typedef ks_engine Aarch64_asm;
#else
# error "*** ERROR: Unknown assembler. ***"
#endif /* KEYSTONE_ASM */

#ifdef __WIN__
typedef enum : uint64_t {
#else /* __WIN__ */
typedef enum {
#endif /* __WIN__ */
    AARCH64_ASM_WRAP_BAD_ARG = (uint64_t)0xFFFFFFFFFFFFFFF0, /* (int64_t) -16 */
    AARCH64_ASM_WRAP_INIT_FAIL,
    AARCH64_ASM_WRAP_INVALID_INSTR,
    AARCH64_ASM_WRAP_ALLOC,
    AARCH64_ASM_WRAP_UNKNOWN,
    AARCH64_ASM_WRAP_OK = 0,
} AARCH64_ASM_WRAP_ERROR;

static_assert(sizeof(AARCH64_ASM_WRAP_ERROR) == 8, "AARCH64_ASM_WRAP_ERROR must be 64 bit");
static_assert(((int64_t)AARCH64_ASM_WRAP_UNKNOWN) < 0, "ERRORS must be negative");

/***
 * @brief Function inits global structure disasm.
 * @return
 *  Success:
 *      AARCH64_ASM_WRAP_OK
 *  Fail:
 *      Other AARCH64_ASM_WRAP_*. To understanding sence of error call function
 *      arrch64_get_asm_error_str().
 */
EXPORT_FUNC
AARCH64_ASM_WRAP_ERROR aarch64_init_asm(void);

/***
 * @brief Function finalizes global structure asm.
 *        Always success.
 */
EXPORT_FUNC
void aarch64_fini_asm(void);

/***
 * @brief Function takes aarch64 assembler code from @p code and turns it into
 *        aarch64 binary code like the first instructions has PC = @p pc.
 * @param[in] code Pointer to place with assembler code.
 * @param[in] pc Initial program counter of first instruction.
 * @param[out] encoding Place, where will save pointer to the assembled code.
 *                      Don't need to allocate memory before, but need to free
 *                      after.
 * @param[out] size Size in bytes of assembled code.
 * @param[out] count Number of assembled instructions.
 */
EXPORT_FUNC
AARCH64_ASM_WRAP_ERROR aarch64_do_asm( const char* code
                                     , uint64_t pc
                                     , uint8_t **encoding
                                     , size_t *size
                                     , size_t *count
                                     );

#endif /* __AARCH64_ASM_WRAP_H */


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

#ifndef __AARCH64_AARCH64_DISASM_WRAP_H
#define __AARCH64_AARCH64_DISASM_WRAP_H

#include "comdef.h"

#include <assert.h>
#include <stdint.h>

#ifdef CAPSTONE_DISASM
# include <capstone/capstone.h>
# define MAX_MNEMONIC_SIZE   193

typedef cs_insn Aarch64_instr;
typedef csh     Aarch64_disasm;
#else
# error "*** ERROR: Unknown disassembler. ***"
#endif /* CAPSTONE_DISASM */

#ifdef __WIN__
typedef enum : uint64_t {
#else /* __WIN__ */
typedef enum {
#endif /* __WIN__ */
    AARCH64_DISASM_WRAP_BAD_ARG = (uint64_t)0xFFFFFFFFFFFFFFF0, /* (int64_t) -16 */
    AARCH64_DISASM_WRAP_INIT_FAIL,
    AARCH64_DISASM_WRAP_INVALID_INSTR,
    AARCH64_DISASM_WRAP_ALLOC,
    AARCH64_DISASM_WRAP_UNKNOWN,
    AARCH64_DISASM_WRAP_OK = 0,
} AARCH64_DISASM_WRAP_ERROR;

static_assert(sizeof(AARCH64_DISASM_WRAP_ERROR) == 8, "AARCH64_DISASM_WRAP_ERROR must be 64 bit");
static_assert(((int64_t)AARCH64_DISASM_WRAP_UNKNOWN) < 0, "ERRORS must be negative");

/***
 * @brief Function inits global structure disasm.
 * @return
 *  Success:
 *      AARCH64_DISASM_WRAP_OK.
 *  Fail:
 *      Other AARCH64_DISASM_WRAP_*. To understanding sence of error call function
 *      arrch64_get_disasm_error_str().
 */
EXPORT_FUNC
AARCH64_DISASM_WRAP_ERROR aarch64_init_disasm(void);

/***
 * @brief Function finalizes global structure disasm.
 *        Always success.
 */
EXPORT_FUNC
void aarch64_fini_disasm(void);


/***
 * @brief Function allocates instruction descriptor.
 * @return
 *  Success:
 *      Pointer to allocated instruction descriptor.
 *  Fail:
 *      NULL.
 */
EXPORT_FUNC
Aarch64_instr *aarch64_alloc_instr(void);

/***
 * @brief Function frees instruction descriptor.
 * @param[in] instr Pointer to the buffer where placed pointer to instruction
 *                  descriptor.
 */
EXPORT_FUNC
void aarch64_free_instr(Aarch64_instr **instr);

/***
 * @brief Disassemble 1 instruction.
 * @param[out] instr pointer to allocated instruction descriptor.
 * @param[in] code pointer to code should be disassembled.
 * @param[in] pc program counter of instruction.
*/
EXPORT_FUNC
AARCH64_DISASM_WRAP_ERROR aarch64_do_disasm( Aarch64_instr *instr
                                           , const uint8_t *code
                                           , uint64_t pc
                                           );

/***
 * @brief Put mnemonic of instruction in the @p mnemonic array of chars. Size
 *        of array should equal MAX_MNEMONIC_SIZE.
 * @param[in] insn pointer to instruction descriptor.
 * @param[out] mnemonic pointer mnemonic should be placed.
 */
EXPORT_FUNC
void aarch64_get_instr_mnemonic(const Aarch64_instr *instr, char *mnemonic);

#endif /* __AARCH64_AARCH64_DISASM_WRAP_H */


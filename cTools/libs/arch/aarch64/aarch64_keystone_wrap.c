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

#include "aarch64_asm_wrap.h"

static Aarch64_asm *aarch64_asm = NULL;

#ifdef KEYSTONE_ASM
AARCH64_ASM_WRAP_ERROR aarch64_init_asm(void)
{
    if (aarch64_asm)
        return AARCH64_ASM_WRAP_INIT_FAIL;

    ks_err err = ks_open(KS_ARCH_ARM64, KS_MODE_LITTLE_ENDIAN, &aarch64_asm);
    if (err != KS_ERR_OK) {
        STDERROR_PRINT("ERROR: failed on ks_open(), quit\n");
        return AARCH64_ASM_WRAP_INIT_FAIL;
    }

    return AARCH64_ASM_WRAP_OK;
}

void aarch64_fini_asm(void)
{
    if (aarch64_asm)
        ks_close(aarch64_asm);
}

AARCH64_ASM_WRAP_ERROR aarch64_do_asm( const char* code
                                     , uint64_t pc
                                     , uint8_t **encoding
                                     , size_t *size
                                     , size_t *count
                                     )
{
    if (ks_asm(aarch64_asm, code, pc, encoding, size, count) != KS_ERR_OK) {
        return AARCH64_ASM_WRAP_INVALID_INSTR;
    }

    return AARCH64_ASM_WRAP_OK;
}
#else
# error "*** ERROR: Unknown assembler. ***"
#endif /* KEYSTONE_ASM */


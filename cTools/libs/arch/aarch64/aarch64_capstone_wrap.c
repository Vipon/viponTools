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

#include "string.h"
#include "aarch64_disasm_wrap.h"
#include <stdbool.h>

static bool init = false;
static Aarch64_disasm aarch64_disasm;

#ifdef CAPSTONE_DISASM
AARCH64_DISASM_WRAP_ERROR aarch64_init_disasm(void)
{
    if (init) {
        return AARCH64_DISASM_WRAP_INIT_FAIL;
    }

    /* csopen(hardware architecture, hardware mode, handler) */
    if (cs_open(CS_ARCH_ARM64, CS_MODE_ARM, &aarch64_disasm) != CS_ERR_OK) {
        return AARCH64_DISASM_WRAP_INIT_FAIL;
    }

    /* turn on detail information. */
    if (cs_option(aarch64_disasm, CS_OPT_DETAIL, CS_OPT_ON) != CS_ERR_OK) {
        cs_close(&aarch64_disasm);
        return AARCH64_DISASM_WRAP_INIT_FAIL;
    }

    init = true;
    return AARCH64_DISASM_WRAP_OK;
}

void aarch64_fini_disasm(void)
{
    if (init) {
        cs_close(&aarch64_disasm);
        init = false;
    }
}

Aarch64_instr *aarch64_alloc_instr(void)
{
    return cs_malloc(aarch64_disasm);
}

void aarch64_free_instr(Aarch64_instr **instr)
{
    cs_free(*instr, 1);
    *instr = NULL;
}

AARCH64_DISASM_WRAP_ERROR aarch64_do_disasm( Aarch64_instr *instr
                                           , const uint8_t *code
                                           , uint64_t pc
                                           )
{
    size_t code_size = 4; // All instructions is 32 bits length
    if (!cs_disasm_iter(aarch64_disasm, &code, &code_size, &pc, instr)) {
        return AARCH64_DISASM_WRAP_INVALID_INSTR;
    }

    return AARCH64_DISASM_WRAP_OK;
}

void aarch64_get_instr_mnemonic(const Aarch64_instr *instr, char *mnemonic)
{
    strcpy(mnemonic, instr->mnemonic);
    strcat(mnemonic, " ");
    strcat(mnemonic, instr->op_str);
}
#else
# error "*** ERROR: Unknown disassembler. ***"
#endif /* CAPSTONE_DISASM */


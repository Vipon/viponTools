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

#include "mem.h"
#include "test.h"
#include "aarch64_asm_wrap.h"
#include "aarch64_disasm_wrap.h"

#include <stdlib.h>

static
void test_aarch64_asm_disasm(void)
{
    size_t size = 0;
    size_t count = 0;
    uint64_t addr = 0x0;
    uint8_t *encoding = NULL;
    uint8_t encoding_ref[] = {0x41, 0x00, 0x03, 0x8b};
    char *code_ref = "add x1, x2, x3";
    if (aarch64_do_asm(code_ref, addr, &encoding, &size, &count) != AARCH64_ASM_WRAP_OK)
        exit(EXIT_FAILURE);

    size_t i = 0;
    for (i = 0; i < size; ++i) {
        EXPECT_BYTE_EQ(encoding[i], encoding_ref[i]);
    }

    Aarch64_instr *instr = aarch64_alloc_instr();
    aarch64_do_disasm(instr, encoding, addr);
    char code[MAX_MNEMONIC_SIZE] = "";
    aarch64_get_instr_mnemonic(instr, code);
    EXPECT_STR_EQ(code_ref, code);
    aarch64_free_instr(&instr);
    Free(encoding);
}

int main(int argc, const char *argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    aarch64_init_asm();
    aarch64_init_disasm();

    test_aarch64_asm_disasm();

    aarch64_fini_asm();
    aarch64_fini_disasm();
    return 0;
}


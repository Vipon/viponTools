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
 { * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR AARCH64_INSTR_TYPE_CBR },
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include "mem.h"
#include "test.h"
#include "aarch64.h"
#include "aarch64_asm_wrap.h"
#include "aarch64_disasm_wrap.h"

#include <stdlib.h>
#include <stdbool.h>

typedef struct {
    char               *mnemonic;
    Aarch64_instr_type type;
    Aarch64_instr_op   op;
} Aarch64_test_instr;

static
void test_detect_pc_rel(void)
{
    Aarch64_test_instr pc_rel_instr[] = {
        { "adr x0, 0x4", AARCH64_INSTR_TYPE_IMM_PC_REL, AARCH64_INSTR_OP_ADR },
        { "adrp x0, 0x4000", AARCH64_INSTR_TYPE_IMM_PC_REL, AARCH64_INSTR_OP_ADRP },
        { "b.eq 0x160", AARCH64_INSTR_TYPE_CBR, AARCH64_INSTR_OP_BEQ },
        { "b.ne 0x100", AARCH64_INSTR_TYPE_CBR, AARCH64_INSTR_OP_BNE },
        { "b.ge 0x100", AARCH64_INSTR_TYPE_CBR, AARCH64_INSTR_OP_BGE },
        { "b.gt 0x100", AARCH64_INSTR_TYPE_CBR, AARCH64_INSTR_OP_BGT },
        { "b.le 0x100", AARCH64_INSTR_TYPE_CBR, AARCH64_INSTR_OP_BLE },
        { "b.lt 0x100", AARCH64_INSTR_TYPE_CBR, AARCH64_INSTR_OP_BLT },
        { "b.cs 0x100", AARCH64_INSTR_TYPE_CBR, AARCH64_INSTR_OP_BCS },
        { "b.cc 0x100", AARCH64_INSTR_TYPE_CBR, AARCH64_INSTR_OP_BCC },
        { "b.mi 0x100", AARCH64_INSTR_TYPE_CBR, AARCH64_INSTR_OP_BMI },
        { "b.pl 0x100", AARCH64_INSTR_TYPE_CBR, AARCH64_INSTR_OP_BPL },
        { "b.vs 0x100", AARCH64_INSTR_TYPE_CBR, AARCH64_INSTR_OP_BVS },
        { "b.vc 0x100", AARCH64_INSTR_TYPE_CBR, AARCH64_INSTR_OP_BVC },
        { "b.hi 0x100", AARCH64_INSTR_TYPE_CBR, AARCH64_INSTR_OP_BHI },
        { "b.hs 0x100", AARCH64_INSTR_TYPE_CBR, AARCH64_INSTR_OP_BCS },
        { "b.lo 0x100", AARCH64_INSTR_TYPE_CBR, AARCH64_INSTR_OP_BCC },
        { "b.ls 0x100", AARCH64_INSTR_TYPE_CBR, AARCH64_INSTR_OP_BLS },
/* FEAT_HBC
        { "bc.eq 0x160", AARCH64_INSTR_TYPE_MBR },
        { "bc.ne 0x100", AARCH64_INSTR_TYPE_MBR },
        { "bc.ge 0x100", AARCH64_INSTR_TYPE_MBR },
        { "bc.gt 0x100", AARCH64_INSTR_TYPE_MBR },
        { "bc.le 0x100", AARCH64_INSTR_TYPE_MBR },
        { "bc.lt 0x100", AARCH64_INSTR_TYPE_MBR },
        { "bc.cs 0x100", AARCH64_INSTR_TYPE_MBR },
        { "bc.cc 0x100", AARCH64_INSTR_TYPE_MBR },
        { "bc.mi 0x100", AARCH64_INSTR_TYPE_MBR },
        { "bc.pl 0x100", AARCH64_INSTR_TYPE_MBR },
        { "bc.vs 0x100", AARCH64_INSTR_TYPE_MBR },
        { "bc.vc 0x100", AARCH64_INSTR_TYPE_MBR },
        { "bc.hi 0x100", AARCH64_INSTR_TYPE_MBR },
        { "bc.hs 0x100", AARCH64_INSTR_TYPE_MBR },
        { "bc.lo 0x100", AARCH64_INSTR_TYPE_MBR },
        { "bc.ls 0x100", AARCH64_INSTR_TYPE_MBR },
*/
/* FEAT_PAuth_LR
*/
        { "b 0x4000", AARCH64_INSTR_TYPE_UBR_IMM, AARCH64_INSTR_OP_B },
        { "bl 0x4000", AARCH64_INSTR_TYPE_UBR_IMM, AARCH64_INSTR_OP_BL },
        { "cbz w10, 0x100", AARCH64_INSTR_TYPE_CMP_BR, AARCH64_INSTR_OP_CBZ32 },
        { "cbnz w11, 0x100", AARCH64_INSTR_TYPE_CMP_BR, AARCH64_INSTR_OP_CBNZ32 },
        { "cbz x12, 0x100", AARCH64_INSTR_TYPE_CMP_BR, AARCH64_INSTR_OP_CBZ64 },
        { "cbnz x13, 0x100", AARCH64_INSTR_TYPE_CMP_BR, AARCH64_INSTR_OP_CBNZ64 },
        { "tbz w13, 23, 0x100", AARCH64_INSTR_TYPE_TEST_BR, AARCH64_INSTR_OP_TBZ32 },
        { "tbnz w14, 23, 0x100", AARCH64_INSTR_TYPE_TEST_BR, AARCH64_INSTR_OP_TBNZ32 },
        { "tbz x15, 33, 0x100", AARCH64_INSTR_TYPE_TEST_BR, AARCH64_INSTR_OP_TBZ64 },
        { "tbnz x16, 33, 0x100", AARCH64_INSTR_TYPE_TEST_BR, AARCH64_INSTR_OP_TBNZ64 },
        { "ldr w3, 0x10", AARCH64_INSTR_TYPE_LOAD_REG_L, AARCH64_INSTR_OP_LDR32 },
        { "ldr x1, 0x8", AARCH64_INSTR_TYPE_LOAD_REG_L, AARCH64_INSTR_OP_LDR64 },
        { "ldr s5, 0x10", AARCH64_INSTR_TYPE_LOAD_REG_L, AARCH64_INSTR_OP_LDRF32 },
        { "ldr d4, 0x14", AARCH64_INSTR_TYPE_LOAD_REG_L, AARCH64_INSTR_OP_LDRF64 },
        { "ldr q2, 0x30", AARCH64_INSTR_TYPE_LOAD_REG_L, AARCH64_INSTR_OP_LDRF128 },
        { "ldrsw x6, 0x1000", AARCH64_INSTR_TYPE_LOAD_REG_L, AARCH64_INSTR_OP_LDRSW },
        { "prfm 0x3, 0x1000", AARCH64_INSTR_TYPE_LOAD_REG_L, AARCH64_INSTR_OP_PRFM },
        { NULL, AARCH64_INSTR_TYPE_UNKNOWN, AARCH64_INSTR_OP_NUM}
    };

    size_t size = 0;
    size_t count = 0;
    uint64_t addr = 0x0;
    uint8_t *encoding = NULL;
    size_t i = 0;
    for (i = 0; pc_rel_instr[i].mnemonic; ++i) {
        if (aarch64_do_asm(pc_rel_instr[i].mnemonic, addr, &encoding, &size, &count)
                != AARCH64_ASM_WRAP_OK)
        {
            VT_ERROR("Cannot asm %s", pc_rel_instr[i].mnemonic);
            exit(EXIT_FAILURE);
        }

        EXPECT_BOOL_EQ(true, aarch64_is_instr_pc_rel(*(uint32_t*)encoding));
        EXPECT_INT_EQ(pc_rel_instr[i].type, aarch64_get_instr_type(*(uint32_t*)encoding));
        EXPECT_INT_EQ(pc_rel_instr[i].op, aarch64_get_instr_op(*(uint32_t*)encoding));
        Free(encoding);
    }
}

int main(int argc, const char *argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    aarch64_init_asm();
    aarch64_init_disasm();

    test_detect_pc_rel();

    aarch64_fini_asm();
    aarch64_fini_disasm();
    return 0;
}


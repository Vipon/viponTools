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

#include "aarch64.h"

typedef struct {
    Aarch64_instr_type type;
    uint32_t           set_bit_mask;
    uint32_t           clear_bit_mask;
} Aarch64_instr_type_mask;

static
Aarch64_instr_type_mask aarch64_instr_type_mask[AARCH64_INSTR_TYPE_NUM] = {
    { AARCH64_INSTR_TYPE_RESERVED,   0x00000000 , 0x00000000 },
    { AARCH64_INSTR_TYPE_SME,        0x80000000 , 0x1e000000 },
    { AARCH64_INSTR_TYPE_SVE,        0x04000000 , 0x1a000000 },
    { AARCH64_INSTR_TYPE_IMM_PC_REL, 0x10000000 , 0x0f000000 },
    { AARCH64_INSTR_TYPE_IMM,        0x10000000 , 0x0c000000 },
    { AARCH64_INSTR_TYPE_CONTROL,    0x14000000 , 0x80000000 },
    { AARCH64_INSTR_TYPE_LOAD_STORE, 0x08000000 , 0x02000000 },
    { AARCH64_INSTR_TYPE_REGISTER,   0x0a000000 , 0x04000000 },
    { AARCH64_INSTR_TYPE_F_SIMD,     0x0e000000 , 0x00000000 },
};

Aarch64_instr_type aarch64_get_instr_type(uint32_t instr)
{
    Aarch64_instr_type i = AARCH64_INSTR_TYPE_RESERVED;
    for (i = AARCH64_INSTR_TYPE_RESERVED; i < AARCH64_INSTR_TYPE_NUM; ++i) {
        uint32_t set_bit_mask = aarch64_instr_type_mask[i].set_bit_mask;
        uint32_t clear_bit_mask = aarch64_instr_type_mask[i].clear_bit_mask;
        if (((instr & set_bit_mask) == set_bit_mask) &&
            ((~instr & clear_bit_mask) == clear_bit_mask)) {
            return i;
        }
    }

    return AARCH64_INSTR_TYPE_UNKNOWN;
}

bool aarch64_is_instr_pc_rel(uint32_t instr)
{
    switch (aarch64_get_instr_type(instr)) {
    case AARCH64_INSTR_TYPE_IMM_PC_REL:
        return true;
    case AARCH64_INSTR_TYPE_CONTROL:
    case AARCH64_INSTR_TYPE_LOAD_STORE:
    default:
        return false;
    }
}


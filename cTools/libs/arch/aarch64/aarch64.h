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

#ifndef __AARCH64_H
#define __AARCH64_H

#include "comdef.h"

#include <stdint.h>
#include <stdbool.h>

typedef enum {
    AARCH64_INSTR_TYPE_RESERVED = 0,
    // SME
    AARCH64_INSTR_TYPE_SME,         // Scalable Matrix Extension
    // SVE
    AARCH64_INSTR_TYPE_SVE,         // Scalable Vector Extension
    // Immediate
    AARCH64_INSTR_TYPE_IMM_PC_REL,  // PC relative Immediate
    AARCH64_INSTR_TYPE_IMM,         // Immediate
    // Control, System
    AARCH64_INSTR_TYPE_CBR,         // Conditional Branch (Immediate)
    AARCH64_INSTR_TYPE_MBR,         // Miscellaneous Branch (Immediate)
    AARCH64_INSTR_TYPE_UBR_REG,     // Unconditional Branch (Register)
    AARCH64_INSTR_TYPE_UBR_IMM,     // Unconditional Branch (Immediate)
    AARCH64_INSTR_TYPE_CMP_BR,      // Compare and Branch (Immediate)
    AARCH64_INSTR_TYPE_TEST_BR,     // Test and Branch (Immediate)
    AARCH64_INSTR_TYPE_CONTROL,     // Branch, Exceptions, System instruction
    // Load Store
    AARCH64_INSTR_TYPE_LOAD_REG_L,  // Load Register (Literal)
    AARCH64_INSTR_TYPE_LOAD_STORE,
    // Register
    AARCH64_INSTR_TYPE_REGISTER,
    // Floating-Point SIMD
    AARCH64_INSTR_TYPE_F_SIMD,      // Floating-point and SIMD
    AARCH64_INSTR_TYPE_UNKNOWN,
    AARCH64_INSTR_TYPE_NUM = AARCH64_INSTR_TYPE_UNKNOWN
} Aarch64_instr_type;

typedef enum {
    AARCH64_INSTR_OP_RESERVED = 0,
    AARCH64_INSTR_OP_ADR,
    AARCH64_INSTR_OP_ADRP,
    AARCH64_INSTR_OP_BEQ,
    AARCH64_INSTR_OP_BNE,
    AARCH64_INSTR_OP_BCS,
    AARCH64_INSTR_OP_BCC,
    AARCH64_INSTR_OP_BMI,
    AARCH64_INSTR_OP_BPL,
    AARCH64_INSTR_OP_BVS,
    AARCH64_INSTR_OP_BVC,
    AARCH64_INSTR_OP_BHI,
    AARCH64_INSTR_OP_BLS,
    AARCH64_INSTR_OP_BGE,
    AARCH64_INSTR_OP_BLT,
    AARCH64_INSTR_OP_BGT,
    AARCH64_INSTR_OP_BLE,
    AARCH64_INSTR_OP_BCEQ,
    AARCH64_INSTR_OP_BCNE,
    AARCH64_INSTR_OP_BCCS,
    AARCH64_INSTR_OP_BCCC,
    AARCH64_INSTR_OP_BCMI,
    AARCH64_INSTR_OP_BCPL,
    AARCH64_INSTR_OP_BCVS,
    AARCH64_INSTR_OP_BCVC,
    AARCH64_INSTR_OP_BCHI,
    AARCH64_INSTR_OP_BCLS,
    AARCH64_INSTR_OP_BCGE,
    AARCH64_INSTR_OP_BCLT,
    AARCH64_INSTR_OP_BCGT,
    AARCH64_INSTR_OP_BCLE,
    AARCH64_INSTR_OP_RETAASPPC,
    AARCH64_INSTR_OP_RETABSPPC,
    AARCH64_INSTR_OP_B,
    AARCH64_INSTR_OP_BL,
    AARCH64_INSTR_OP_CBZ32,
    AARCH64_INSTR_OP_CBNZ32,
    AARCH64_INSTR_OP_CBZ64,
    AARCH64_INSTR_OP_CBNZ64,
    AARCH64_INSTR_OP_TBZ32,
    AARCH64_INSTR_OP_TBNZ32,
    AARCH64_INSTR_OP_TBZ64,
    AARCH64_INSTR_OP_TBNZ64,
    AARCH64_INSTR_OP_LDR32,
    AARCH64_INSTR_OP_LDR64,
    AARCH64_INSTR_OP_LDRF32,
    AARCH64_INSTR_OP_LDRF64,
    AARCH64_INSTR_OP_LDRF128,
    AARCH64_INSTR_OP_LDRSW,
    AARCH64_INSTR_OP_PRFM,
    AARCH64_INSTR_OP_UNKNOWN,
    AARCH64_INSTR_OP_NUM = AARCH64_INSTR_OP_UNKNOWN
} Aarch64_instr_op;

EXPORT_FUNC
Aarch64_instr_type aarch64_get_instr_type(uint32_t instr);

EXPORT_FUNC
bool aarch64_is_instr_pc_rel(uint32_t instr);

EXPORT_FUNC
Aarch64_instr_op aarch64_get_instr_op(uint32_t instr);

#endif /* __AARCH64_H */


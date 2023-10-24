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

#include "mem.h"
#include "bits.h"
#include "string.h"
#include "comdef.h"
#include "aarch64.h"
#include "aarch64_code_move.h"

#include <inttypes.h>

CODE_MOVE_ERROR aarch64_instr_move( const uint8_t *src
                                  , uint8_t  *dst
                                  , uint64_t old_pc
                                  , uint64_t new_pc
                                  , uint64_t dst_size
                                  )
{
    if (src == NULL || dst == NULL || dst_size == (uint64_t)(-1)) {
        return CODE_MOVE_ERROR_BAD_ARG;
    }

    uint32_t old_instr = *(const uint32_t*)src;
    switch (aarch64_get_instr_type(old_instr)) {
    case AARCH64_INSTR_TYPE_RESERVED:
    case AARCH64_INSTR_TYPE_UNKNOWN:
        return CODE_MOVE_ERROR_UNKNOWN_INSTR;
    default:
        break;
    }

    uint64_t new_instr_size = 4;
    uint32_t new_instr = old_instr;
    switch (aarch64_get_instr_op(old_instr)) {
    case AARCH64_INSTR_OP_BL:
    {
        int64_t imm26 = SIGN_EXTEND(old_instr, 25);
        uint64_t target_addr = old_pc + ((uint64_t)imm26 << 2);
        int64_t new_imm26 = SIGN_EXTEND(((int64_t)(target_addr - new_pc) >> 2), 25);
        uint64_t new_target_addr = new_pc + ((uint64_t)(new_imm26) << 2);
        if (target_addr == new_target_addr) {
            // We can fit offset into 26 bits.
            new_instr_size = aarch64_put_bl((uint32_t*)dst, new_pc, target_addr);
        } else {
            // We cannot fit offset into 26 bits. Need to place bl_stub.
            new_instr_size = aarch64_put_bl_stub((uint32_t*)dst, target_addr);
        }

        break;
    }
    case AARCH64_INSTR_OP_ADR:
    /*{
        // Number of destination register
        uint8_t reg_num = old_instr & 0x1F;
        UNUSED(reg_num);
        // PC + imm21 = address of a byte in the memory
        int32_t imm21 = (((old_instr >> 5) << 2) | (old_instr >> 29)) & 0x1FFFFF;
        uint64_t res_addr = old_pc + SIGN_EXTEND(imm21, 20);
        int64_t new_imm = (int64_t)(res_addr - new_pc);
        UNUSED(new_imm);
        UNUSED(res_addr);

    }*/
    case AARCH64_INSTR_OP_ADRP:
    case AARCH64_INSTR_OP_BEQ:
    case AARCH64_INSTR_OP_BNE:
    case AARCH64_INSTR_OP_BCS:
    case AARCH64_INSTR_OP_BCC:
    case AARCH64_INSTR_OP_BMI:
    case AARCH64_INSTR_OP_BPL:
    case AARCH64_INSTR_OP_BVS:
    case AARCH64_INSTR_OP_BVC:
    case AARCH64_INSTR_OP_BHI:
    case AARCH64_INSTR_OP_BLS:
    case AARCH64_INSTR_OP_BGE:
    case AARCH64_INSTR_OP_BLT:
    case AARCH64_INSTR_OP_BGT:
    case AARCH64_INSTR_OP_BLE:
    case AARCH64_INSTR_OP_BCEQ:
    case AARCH64_INSTR_OP_BCNE:
    case AARCH64_INSTR_OP_BCCS:
    case AARCH64_INSTR_OP_BCCC:
    case AARCH64_INSTR_OP_BCMI:
    case AARCH64_INSTR_OP_BCPL:
    case AARCH64_INSTR_OP_BCVS:
    case AARCH64_INSTR_OP_BCVC:
    case AARCH64_INSTR_OP_BCHI:
    case AARCH64_INSTR_OP_BCLS:
    case AARCH64_INSTR_OP_BCGE:
    case AARCH64_INSTR_OP_BCLT:
    case AARCH64_INSTR_OP_BCGT:
    case AARCH64_INSTR_OP_BCLE:
    case AARCH64_INSTR_OP_RETAASPPC:
    case AARCH64_INSTR_OP_RETABSPPC:
    case AARCH64_INSTR_OP_B:
    case AARCH64_INSTR_OP_CBZ32:
    case AARCH64_INSTR_OP_CBNZ32:
    case AARCH64_INSTR_OP_CBZ64:
    case AARCH64_INSTR_OP_CBNZ64:
    case AARCH64_INSTR_OP_TBZ32:
    case AARCH64_INSTR_OP_TBNZ32:
    case AARCH64_INSTR_OP_TBZ64:
    case AARCH64_INSTR_OP_TBNZ64:
    case AARCH64_INSTR_OP_LDR32:
    case AARCH64_INSTR_OP_LDR64:
    case AARCH64_INSTR_OP_LDRF32:
    case AARCH64_INSTR_OP_LDRF64:
    case AARCH64_INSTR_OP_LDRF128:
    case AARCH64_INSTR_OP_LDRSW:
    case AARCH64_INSTR_OP_PRFM:
        FALLTHROUGH;
    default:
        *(uint32_t*)dst = new_instr;
        break;
    }

    return (CODE_MOVE_ERROR)new_instr_size;
}

CODE_MOVE_ERROR aarch64_code_move( const uint8_t *src
                                 , uint8_t  *dst
                                 , uint64_t old_pc
                                 , uint64_t new_pc
                                 , uint64_t src_size
                                 , uint64_t dst_size
                                 )
{
    if (src == NULL                || dst == NULL ||
        src_size == (uint64_t)(-1) || dst_size == (uint64_t)(-1)) {
        return CODE_MOVE_ERROR_BAD_ARG;
    }

    uint64_t i = 0;
    uint64_t instr_num = src_size / 4;
    uint64_t code_size = 0;
    for (i = 0; i < instr_num; ++i) {
        uint64_t instr_size = (uint64_t)aarch64_instr_move(src, dst, old_pc, new_pc, dst_size);

        src += 4;
        dst += instr_size;
        old_pc += 4;
        new_pc += instr_size;
        code_size += instr_size;
    }

    return (CODE_MOVE_ERROR)code_size;
}


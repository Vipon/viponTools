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

    //STDERROR_PRINT("old_pc %"PRIx64"\n", old_pc);
    uint32_t instr = *(const uint32_t*)src;
    bool x64 = true;

    switch (aarch64_get_instr_type(instr)) {
    case AARCH64_INSTR_TYPE_RESERVED:
    case AARCH64_INSTR_TYPE_UNKNOWN:
        return CODE_MOVE_ERROR_UNKNOWN_INSTR;
    default:
        break;
    }

    uint64_t new_instr_size = 4;
    uint32_t new_instr = instr;
    switch (aarch64_get_instr_op(instr)) {
    case AARCH64_INSTR_OP_ADR:
    {
        STDERROR_PRINT("ADR\n");
        uint8_t reg_num = instr & 0x1F;
        int64_t imm21 = (((instr >> 5) << 2) | ((instr >> 29) & 0x3))
                      & 0x1fffff;
        STDERROR_PRINT("imm21 %"PRIx64"\n", imm21);
        imm21 = SIGN_EXTEND(imm21, 20);
        uint64_t target_addr = old_pc + (uint64_t)imm21;
        STDERROR_PRINT("target_addr %"PRIx64"\n", target_addr);
        int64_t new_imm21 = (target_addr - new_pc) & 0x1fffff;
        new_imm21 = SIGN_EXTEND(new_imm21, 20);
        uint64_t new_target_addr = new_pc + (uint64_t)(new_imm21);
        if (target_addr == new_target_addr) {
            // We can fit offset into 21 bits.
            new_instr_size = aarch64_put_adr( (uint32_t*)dst
                                            , new_pc
                                            , target_addr
                                            , reg_num);
        } else {
            STDERROR_PRINT("STUB\n");
            // We cannot fit offset into 21 bits. Need to place adr_stub.
            new_instr_size = aarch64_put_adr_stub( (uint32_t*)dst
                                                 , target_addr
                                                 , reg_num);
        }

        break;
    }
    case AARCH64_INSTR_OP_ADRP:
    {
        STDERROR_PRINT("ADRP\n");
        uint8_t reg_num = instr & 0x1F;
        uint64_t old_page_pc = old_pc & (uint64_t)(~0xFFF);
        //STDERROR_PRINT("old_page_pc %"PRIx64"\n", old_page_pc);
        uint64_t new_page_pc = new_pc & (uint64_t)(~0xFFF);
        //STDERROR_PRINT("new_page_pc %"PRIx64"\n", new_page_pc);
        int64_t imm21 = (((instr >> 5) << 2) | ((instr >> 29) & 0x3))
                      & 0x1fffff;
        imm21 = SIGN_EXTEND(imm21, 20);
        //STDERROR_PRINT("imm21 %"PRIx64"\n", imm21);
        uint64_t target_addr = old_page_pc + ((uint64_t)imm21 << 12);
        //STDERROR_PRINT("target_addr %"PRIx64"\n", target_addr);
        int64_t new_imm21 = ((target_addr - new_page_pc) >> 12) & 0x1fffff;
        new_imm21 = SIGN_EXTEND(new_imm21, 20);
        //STDERROR_PRINT("new_imm21 %"PRIx64"\n", new_imm21);
        uint64_t new_target_addr = new_page_pc + ((uint64_t)new_imm21 << 12);
        //STDERROR_PRINT("new_target_addr %"PRIx64"\n", new_target_addr);
        if (target_addr == new_target_addr) {
            // We can fit offset into 21 bits. +- 4GB
            new_instr_size = aarch64_put_adrp((uint32_t*)dst, new_pc, target_addr, reg_num);
            //new_instr_size = aarch64_put_adrp_stub((uint32_t*)dst, target_addr, reg_num);
        } else {
            STDERROR_PRINT("PUT STUB\n");
            // We cannot fit offset into 21 bits. Need to place adr_stub.
            new_instr_size = aarch64_put_adrp_stub((uint32_t*)dst, target_addr, reg_num);
        }

        break;
    }
    case AARCH64_INSTR_OP_B:
    {
        int64_t imm26 = SIGN_EXTEND(instr & 0x3ffffff, 25);
        uint64_t target_addr = old_pc + ((uint64_t)imm26 << 2);
        int64_t new_imm26 = ((target_addr - new_pc) >> 2) & 0x3ffffff;
        new_imm26 = SIGN_EXTEND(new_imm26, 25);
        uint64_t new_target_addr = new_pc + ((uint64_t)new_imm26 << 2);
        if (target_addr == new_target_addr) {
            STDERROR_PRINT("B\n");
            // We can fit offset into 26 bits.
            new_instr_size = aarch64_put_b( (uint32_t*)dst
                                          , new_pc
                                          , target_addr
                                          );
        } else {
            STDERROR_PRINT("B_STUB\n");
            static uint64_t x30_temp = 0;
            // We cannot fit offset into 26 bits. Need to place b_stub.
            new_instr_size = aarch64_put_b_stub( (uint32_t*)dst
                                               , target_addr
                                               , (uint64_t)(&x30_temp));
        }
        break;
    }
    case AARCH64_INSTR_OP_BL:
    {
        STDERROR_PRINT("BL\n");
        int64_t imm26 = SIGN_EXTEND(instr & 0x3ffffff, 25);
        uint64_t target_addr = old_pc + ((uint64_t)imm26 << 2);
        int64_t new_imm26 = (int64_t)(target_addr - new_pc) & 0x3ffffff;
        new_imm26 = SIGN_EXTEND(new_imm26 >> 2, 25);
        uint64_t new_target_addr = new_pc + ((uint64_t)(new_imm26) << 2);
        if (target_addr == new_target_addr) {
            // We can fit offset into 26 bits.
            new_instr_size = aarch64_put_bl((uint32_t*)dst, new_pc, target_addr);
        } else {
            // We cannot fit offset into 26 bits. Need to place bl_stub.
            if (IS_N_BITS_ENOUGH_64(target_addr - new_pc, 33)) {
                STDERROR_PRINT("BL 33 bits enough\n");
                new_instr_size = aarch64_put_bl_stub33( (uint32_t*)dst
                                                , new_pc
                                                , target_addr
                                                );
            } else {
                new_instr_size = aarch64_put_bl_stub_abs( (uint32_t*)dst
                                                , target_addr
                                                );
            }
        }

        break;
    }
    case AARCH64_INSTR_OP_LDR32:
        STDERROR_PRINT("LDR32\n");
        x64 = false;
    case AARCH64_INSTR_OP_LDR64:
    {
        STDERROR_PRINT("LDR64\n");
        uint8_t reg_num = instr & 0x1F;
        int64_t imm19 = SIGN_EXTEND((instr >> 5) & 0x7ffff, 18);
        uint64_t target_addr = old_pc + ((uint64_t)imm19 << 2);
        int64_t new_imm19 = ((target_addr - new_pc) >> 2) & 0x7ffff;
        new_imm19 = SIGN_EXTEND(new_imm19, 18);
        uint64_t new_target_addr = new_pc + ((uint64_t)new_imm19 << 2);
        if (target_addr == new_target_addr) {
            // We can fit offset into 19 bits.
            new_instr_size = aarch64_put_ldr((uint32_t*)dst
                                            , new_pc
                                            , target_addr
                                            , reg_num, x64);
        } else {
            STDERROR_PRINT("LDR_STUB\n");
            // We cannot fit offset into 19 bits. Need to place ldr_stub.
            new_instr_size = aarch64_put_ldr_stub((uint32_t*)dst
                                                    , target_addr
                                                    , reg_num
                                                    , x64);
        }
        break;
    }
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
    case AARCH64_INSTR_OP_CBZ32:
    case AARCH64_INSTR_OP_CBNZ32:
    case AARCH64_INSTR_OP_CBZ64:
    case AARCH64_INSTR_OP_CBNZ64:
    case AARCH64_INSTR_OP_TBZ32:
    case AARCH64_INSTR_OP_TBNZ32:
    case AARCH64_INSTR_OP_TBZ64:
    case AARCH64_INSTR_OP_TBNZ64:
    case AARCH64_INSTR_OP_LDRF32:
    case AARCH64_INSTR_OP_LDRF64:
    case AARCH64_INSTR_OP_LDRF128:
    case AARCH64_INSTR_OP_LDRSW:
    case AARCH64_INSTR_OP_PRFM:
        FALLTHROUGH;
    default:
        //STDERROR_PRINT("dst %p\n", dst);
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
    uint64_t new_code_size = 0;
    uint64_t instr_num = src_size / 4;
    //uint64_t top_bound = old_pc + src_size;
    //uint64_t bottom_bound = old_pc;
    for (i = 0; i < instr_num; ++i) {
        uint64_t new_instr_size = (uint64_t) aarch64_instr_move( src
                                                                , dst
                                                                , old_pc
                                                                , new_pc
                                                                , dst_size
                                                                );

        src += 4;
        dst += new_instr_size;
        old_pc += 4;
        new_pc += new_instr_size;
        new_code_size += new_instr_size;
    }

    return (CODE_MOVE_ERROR)new_code_size;
}


CODE_MOVE_ERROR aarch64_estimate_space( const uint8_t *src
                                      , uint64_t old_pc
                                      , uint64_t new_pc
                                      , uint64_t src_size
                                      )
{
    if (src == NULL || src_size == (uint64_t)(-1)) {
        return CODE_MOVE_ERROR_BAD_ARG;
    }

    bool max_estimate = (new_pc == (uint64_t)-1) ? true : false;

    uint64_t i = 0;
    uint64_t new_code_size = 0;
    uint64_t instr_num = src_size / 4;
    //uint64_t top_bound = old_pc + src_size;
    //uint64_t bottom_bound = old_pc;
    for (i = 0; i < instr_num; ++i) {
        uint32_t instr = *(const uint32_t*)src;

        switch (aarch64_get_instr_type(instr)) {
        case AARCH64_INSTR_TYPE_RESERVED:
        case AARCH64_INSTR_TYPE_UNKNOWN:
            return CODE_MOVE_ERROR_UNKNOWN_INSTR;
        default:
            break;
        }

        uint64_t new_instr_size = 4;
        switch (aarch64_get_instr_op(instr)) {
        case AARCH64_INSTR_OP_ADR:
        {
            int64_t imm21 = (((instr >> 5) << 2) | (instr >> 29)) & 0x1fffff;
            imm21 = SIGN_EXTEND(imm21, 20);
            uint64_t target_addr = old_pc + (uint64_t)imm21;
            int64_t new_imm21 = (target_addr - new_pc) & 0x1fffff;
            new_imm21 = SIGN_EXTEND(new_imm21, 20);
            uint64_t new_target_addr = new_pc + (uint64_t)(new_imm21);
            if (max_estimate || target_addr != new_target_addr) {
                // We cannot fit offset into 21 bits. Need to place adr_stub.
                new_instr_size = SIZE_ADR_STUB;
            }

            break;
        }
        case AARCH64_INSTR_OP_ADRP:
        {
            uint64_t old_page_pc = old_pc & (uint64_t)(~0xFFF);
            uint64_t new_page_pc = new_pc & (uint64_t)(~0xFFF);
            int64_t imm21 = (((instr >> 5) << 2) | ((instr >> 29) & 0x3))
                        & 0x1fffff;
            imm21 = SIGN_EXTEND(imm21, 20);
            uint64_t target_addr = old_page_pc + ((uint64_t)imm21 << 12);
            int64_t new_imm21 = ((target_addr - new_page_pc) >> 12) & 0x1fffff;
            new_imm21 = SIGN_EXTEND(new_imm21, 20);
            uint64_t new_target_addr = new_page_pc + ((uint64_t)new_imm21 << 12);

            if (max_estimate ||target_addr != new_target_addr) {
                // We cannot fit offset into 21 bits. Need to place adr_stub.
                new_instr_size = SIZE_ADRP_STUB;
            }

            break;
        }
        case AARCH64_INSTR_OP_B:
        {
            int64_t imm26 = SIGN_EXTEND(instr & 0x3ffffff, 25);
            uint64_t target_addr = old_pc + ((uint64_t)imm26 << 2);
            int64_t new_imm26 = ((target_addr - new_pc) >> 2) & 0x3ffffff;
            new_imm26 = SIGN_EXTEND(new_imm26, 25);
            uint64_t new_target_addr = new_pc + ((uint64_t)new_imm26 << 2);
            if (max_estimate || target_addr != new_target_addr) {
                // We cannot fit offset into 26 bits. Need to place b_stub.
                new_instr_size = SIZE_B_STUB;
            }
            break;
        }
        case AARCH64_INSTR_OP_BL:
        {
            if (max_estimate) {
                new_instr_size = SIZE_BL_STUB_ABS;
                break;
            }

            int64_t imm26 = SIGN_EXTEND(instr & 0x3ffffff, 25);
            uint64_t target_addr = old_pc + ((uint64_t)imm26 << 2);
            int64_t new_imm26 = (int64_t)(target_addr - new_pc) & 0x3ffffff;
            new_imm26 = SIGN_EXTEND(new_imm26 >> 2, 25);
            uint64_t new_target_addr = new_pc + ((uint64_t)(new_imm26) << 2);
            if (target_addr != new_target_addr) {
                // We cannot fit offset into 26 bits. Need to place bl_stub.
                if (IS_N_BITS_ENOUGH_64(target_addr - new_pc, 33)) {
                    new_instr_size = SIZE_BL_STUB33;
                } else {
                    new_instr_size = SIZE_BL_STUB_ABS;
                }
            }

            break;
        }
        case AARCH64_INSTR_OP_LDR32:
        case AARCH64_INSTR_OP_LDR64:
        {
            int64_t imm19 = SIGN_EXTEND(instr & 0x7ffff, 18);
            uint64_t target_addr = old_pc + (uint64_t)(imm19 << 2);
            int64_t new_imm19 = (target_addr - new_pc) & 0x7ffff;
            new_imm19 = SIGN_EXTEND(new_imm19, 18);
            uint64_t new_target_addr = new_pc + (uint64_t)(new_imm19);
            if (max_estimate || target_addr != new_target_addr) {
                // We cannot fit offset into 19 bits. Need to place ldr_stub.
                new_instr_size = SIZE_LDR_STUB;
            }
            break;
        }
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
        case AARCH64_INSTR_OP_CBZ32:
        case AARCH64_INSTR_OP_CBNZ32:
        case AARCH64_INSTR_OP_CBZ64:
        case AARCH64_INSTR_OP_CBNZ64:
        case AARCH64_INSTR_OP_TBZ32:
        case AARCH64_INSTR_OP_TBNZ32:
        case AARCH64_INSTR_OP_TBZ64:
        case AARCH64_INSTR_OP_TBNZ64:
        case AARCH64_INSTR_OP_LDRF32:
        case AARCH64_INSTR_OP_LDRF64:
        case AARCH64_INSTR_OP_LDRF128:
        case AARCH64_INSTR_OP_LDRSW:
        case AARCH64_INSTR_OP_PRFM:
            FALLTHROUGH;
        default:
            break;
        }

        src += 4;
        old_pc += 4;
        new_pc += new_instr_size;
        new_code_size += new_instr_size;
    }

    return (CODE_MOVE_ERROR)new_code_size;
}


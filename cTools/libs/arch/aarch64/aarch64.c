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
#include "bits.h"
#include "string.h"
#include "aarch64.h"

#include <inttypes.h>

typedef struct {
    Aarch64_instr_type type;
    uint32_t           set_bit_mask;
    uint32_t           clear_bit_mask;
} Aarch64_instr_type_mask;

static
Aarch64_instr_type_mask aarch64_instr_type_mask[AARCH64_INSTR_TYPE_NUM] = {
    { AARCH64_INSTR_TYPE_RESERVED,   0x00000000 , 0x9e000000 },
    { AARCH64_INSTR_TYPE_SME,        0x80000000 , 0x1e000000 },
    { AARCH64_INSTR_TYPE_SVE,        0x04000000 , 0x1a000000 },
    { AARCH64_INSTR_TYPE_IMM_PC_REL, 0x10000000 , 0x0f000000 },
    { AARCH64_INSTR_TYPE_IMM,        0x10000000 , 0x0c000000 },
    { AARCH64_INSTR_TYPE_CBR,        0x54000000 , 0xab000000 },
    { AARCH64_INSTR_TYPE_MBR,        0x55000000 , 0xaa000000 },
    { AARCH64_INSTR_TYPE_UBR_REG,    0xd6000000 , 0x28000000 },
    { AARCH64_INSTR_TYPE_UBR_IMM,    0x14000000 , 0x68000000 },
    { AARCH64_INSTR_TYPE_CMP_BR,     0x34000000 , 0x4a000000 },
    { AARCH64_INSTR_TYPE_TEST_BR,    0x36000000 , 0x48000000 },
    { AARCH64_INSTR_TYPE_CONTROL,    0x14000000 , 0x08000000 },
    { AARCH64_INSTR_TYPE_LOAD_REG_L, 0x18000000 , 0x23000000 },
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
    case AARCH64_INSTR_TYPE_CBR:
    case AARCH64_INSTR_TYPE_MBR:
    case AARCH64_INSTR_TYPE_UBR_IMM:
    case AARCH64_INSTR_TYPE_CMP_BR:
    case AARCH64_INSTR_TYPE_TEST_BR:
    case AARCH64_INSTR_TYPE_LOAD_REG_L:
        return true;
    default:
        return false;
    }
}

typedef struct {
    Aarch64_instr_op op;
    uint32_t         set_bit_mask;
    uint32_t         clear_bit_mask;
} Aarch64_instr_op_mask;

static
Aarch64_instr_op_mask aarch64_instr_op_mask[AARCH64_INSTR_OP_NUM] = {
    { AARCH64_INSTR_OP_RESERVED, 0x00000000 , 0x9e000000 },
    { AARCH64_INSTR_OP_ADR,      0x10000000 , 0x8f000000 },
    { AARCH64_INSTR_OP_ADRP,     0x90000000 , 0x0f000000 },
    { AARCH64_INSTR_OP_BEQ,      0x54000000 , 0xab00001f },
    { AARCH64_INSTR_OP_BNE,      0x54000001 , 0xab00001e },
    { AARCH64_INSTR_OP_BCS,      0x54000002 , 0xab00001d },
    { AARCH64_INSTR_OP_BCC,      0x54000003 , 0xab00001c },
    { AARCH64_INSTR_OP_BMI,      0x54000004 , 0xab00001b },
    { AARCH64_INSTR_OP_BPL,      0x54000005 , 0xab00001a },
    { AARCH64_INSTR_OP_BVS,      0x54000006 , 0xab000019 },
    { AARCH64_INSTR_OP_BVC,      0x54000007 , 0xab000018 },
    { AARCH64_INSTR_OP_BHI,      0x54000008 , 0xab000017 },
    { AARCH64_INSTR_OP_BLS,      0x54000009 , 0xab000016 },
    { AARCH64_INSTR_OP_BGE,      0x5400000a , 0xab000015 },
    { AARCH64_INSTR_OP_BLT,      0x5400000b , 0xab000014 },
    { AARCH64_INSTR_OP_BGT,      0x5400000c , 0xab000013 },
    { AARCH64_INSTR_OP_BLE,      0x5400000d , 0xab000012 },
    { AARCH64_INSTR_OP_BCEQ,     0x54000010 , 0xab00000f },
    { AARCH64_INSTR_OP_BCNE,     0x54000011 , 0xab00000e },
    { AARCH64_INSTR_OP_BCCS,     0x54000012 , 0xab00000d },
    { AARCH64_INSTR_OP_BCCC,     0x54000013 , 0xab00000c },
    { AARCH64_INSTR_OP_BCMI,     0x54000014 , 0xab00000b },
    { AARCH64_INSTR_OP_BCPL,     0x54000015 , 0xab00000a },
    { AARCH64_INSTR_OP_BCVS,     0x54000016 , 0xab000009 },
    { AARCH64_INSTR_OP_BCVC,     0x54000017 , 0xab000008 },
    { AARCH64_INSTR_OP_BCHI,     0x54000018 , 0xab000007 },
    { AARCH64_INSTR_OP_BCLS,     0x54000019 , 0xab000006 },
    { AARCH64_INSTR_OP_BCGE,     0x5400001a , 0xab000005 },
    { AARCH64_INSTR_OP_BCLT,     0x5400001b , 0xab000004 },
    { AARCH64_INSTR_OP_BCGT,     0x5400001c , 0xab000003 },
    { AARCH64_INSTR_OP_BCLE,     0x5400001d , 0xab000002 },
    { AARCH64_INSTR_OP_RETAASPPC,0x5500001f , 0xaae00000 },
    { AARCH64_INSTR_OP_RETABSPPC,0x5520001f , 0xaac00000 },
    { AARCH64_INSTR_OP_B,        0x14000000 , 0xe8000000 },
    { AARCH64_INSTR_OP_BL,       0x94000000 , 0x68000000 },
    { AARCH64_INSTR_OP_CBZ32,    0x34000000 , 0xcb000000 },
    { AARCH64_INSTR_OP_CBNZ32,   0x35000000 , 0xca000000 },
    { AARCH64_INSTR_OP_CBZ64,    0xb4000000 , 0x4b000000 },
    { AARCH64_INSTR_OP_CBNZ64,   0xb5000000 , 0x4a000000 },
    { AARCH64_INSTR_OP_TBZ32,    0x36000000 , 0xc9000000 },
    { AARCH64_INSTR_OP_TBNZ32,   0x37000000 , 0xc8000000 },
    { AARCH64_INSTR_OP_TBZ64,    0xb6000000 , 0x49000000 },
    { AARCH64_INSTR_OP_TBNZ64,   0xb7000000 , 0x48000000 },
    { AARCH64_INSTR_OP_LDR32,    0x18000000 , 0xe7000000 },
    { AARCH64_INSTR_OP_LDR64,    0x58000000 , 0xa7000000 },
    { AARCH64_INSTR_OP_LDRF32,   0x1c000000 , 0xe3000000 },
    { AARCH64_INSTR_OP_LDRF64,   0x5c000000 , 0xa3000000 },
    { AARCH64_INSTR_OP_LDRF128,  0x9c000000 , 0x63000000 },
    { AARCH64_INSTR_OP_LDRSW,    0x98000000 , 0x67000000 },
    { AARCH64_INSTR_OP_PRFM,     0xd8000000 , 0x27000000 },
};

Aarch64_instr_op aarch64_get_instr_op(uint32_t instr)
{
    Aarch64_instr_op i = AARCH64_INSTR_OP_RESERVED;
    for (i = AARCH64_INSTR_OP_RESERVED; i < AARCH64_INSTR_OP_NUM; ++i) {
        uint32_t set_bit_mask = aarch64_instr_op_mask[i].set_bit_mask;
        uint32_t clear_bit_mask = aarch64_instr_op_mask[i].clear_bit_mask;
        if (((instr & set_bit_mask) == set_bit_mask) &&
            ((~instr & clear_bit_mask) == clear_bit_mask)) {
            return i;
        }
    }

    return AARCH64_INSTR_OP_UNKNOWN;
}

uint8_t aarch64_put_bl(uint32_t *dst, uint64_t pc, uint64_t target_addr)
{
    int64_t imm26 = SIGN_EXTEND((int64_t)(target_addr - pc) >> 2, 25);
    uint32_t instr = 0x94000000 | ((uint32_t)imm26);
    *dst = instr;
    return 4;
}

uint8_t aarch64_put_bl_stub33(uint32_t *dst, uint64_t pc, uint64_t target_addr)
{
    uint8_t bl_stub32[] = {
        0x1e, 0x00, 0x00, 0x90, // adrp x30, 0x0
        0xde, 0x03, 0x00, 0x91, // add x30, x30, #0x0
        0xc0, 0x03, 0x3f, 0xd6, // blr x30
    };

    uint64_t target_page_addr = target_addr & (uint64_t)(~0xFFF);
    aarch64_put_adrp((uint32_t*)bl_stub32, pc, target_page_addr, 30);
    *(uint32_t*)(bl_stub32 + 4) = 0x910003de | (target_addr & 0xFFF) << 10;

    memcpy(dst, bl_stub32, sizeof(bl_stub32));

    return sizeof(bl_stub32);
}

uint8_t aarch64_put_bl_stub_abs(uint32_t *dst, uint64_t target_addr)
{
    uint8_t bl_stub_abs[] = {
        0x1e, 0x00, 0x80, 0xd2, // movz x30, #0
        0x1e, 0x00, 0xa0, 0xf2, // movk x30, #0x0, lsl #16
        0x1e, 0x00, 0xc0, 0xf2, // movk x30, #0x0, lsl #32
        0x1e, 0x00, 0xe0, 0xf2, // movk x30, #0x0, lsl #48
        0xc0, 0x03, 0x3f, 0xd6, // blr x30
    };

    *(uint32_t*)(bl_stub_abs)      = 0xd280001e | (uint32_t)((target_addr & 0xFFFF) << 5);
    *(uint32_t*)(bl_stub_abs + 4)  = 0xf2a0001e | (uint32_t)(((target_addr >> 16) & 0xFFFF) << 5);
    *(uint32_t*)(bl_stub_abs + 8)  = 0xf2c0001e | (uint32_t)(((target_addr >> 32) & 0xFFFF) << 5);
    *(uint32_t*)(bl_stub_abs + 12) = 0xf2e0001e | (uint32_t)(((target_addr >> 48) & 0xFFFF) << 5);

    memcpy(dst, bl_stub_abs, sizeof(bl_stub_abs));

    return sizeof(bl_stub_abs);
}

uint8_t aarch64_put_adr( uint32_t *dst
                       , uint64_t pc
                       , uint64_t target_addr
                       , uint8_t reg_num
                       )
{
    reg_num &= 0x1f;
    int64_t imm21 = SIGN_EXTEND(target_addr - pc, 20);
    uint32_t instr = 0x10000000
                   | ((imm21 & 0x3) << 29)
                   | (((imm21 >> 2) & 0x7ffff) << 5)
                   | reg_num;
    *dst = instr;
    return 4;
}

uint8_t aarch64_put_adr_stub( uint32_t *dst
                            , uint64_t target_addr
                            , uint8_t reg_num
                            )
{
    // ldr x[reg_num], 0x8      |
    // b 0xc                    | 0x14000003
    // .quad 0xdeadbeefdeadbeef | 0xdeadbeefdeadbeef
    uint8_t adr_stub[] = {
        0x40, 0x00, 0x00, 0x58, // ldr x0, 0x8
        0x03, 0x00, 0x00, 0x14,
        0xef, 0xbe, 0xad, 0xde, 0xef, 0xbe, 0xad, 0xde,
    };

    *(uint64_t*)(adr_stub + 8) = target_addr;
    *(uint32_t*)adr_stub = (uint32_t)(0x58000040 | (reg_num & 0x1f));
    memcpy(dst, adr_stub, sizeof(adr_stub));

    return sizeof(adr_stub);
}

uint8_t aarch64_put_ldr( uint32_t *dst
                       , uint64_t pc
                       , uint64_t target_addr
                       , uint8_t reg_num
                       , bool x64
                       )
{
    reg_num &= 0x1f;
    int64_t imm19 = SIGN_EXTEND((int64_t)(target_addr - pc) >> 2, 18);
    if (x64)
        *dst = (0x58000000) | ((imm19 & 0x7ffff) << 5) | reg_num;
    else
        *dst = (0x18000000) | ((imm19 & 0x7ffff) << 5) | reg_num;
    return 4;
}

uint8_t aarch64_put_ldr_stub( uint32_t *dst
                            , uint64_t target_addr
                            , uint8_t reg_num
                            , bool x64
                            )
{
    reg_num &= 0x1f;

    // ldr x[reg_num], -0x8        |
    // b 0xc                       | 0x14000003
    // .quad 0xdeadbeefdeadbeef    | 0xdeadbeefdeadbeef
    // lrx x[reg_num], [x[reg_num]]
    uint8_t ldr_stub[] = {
        0x40, 0x00, 0x00, 0x58, // ldr x[reg_num], 0x8
        0x03, 0x00, 0x00, 0x14, // b 0xc
        0xef, 0xbe, 0xad, 0xde, 0xef, 0xbe, 0xad, 0xde,
        0x00, 0x00, 0x40, 0xf9, // ldr x[reg_num], [x[reg_num]]
    };

    *(uint32_t*)ldr_stub = (uint32_t)(0x58000040 | reg_num);
    *(uint64_t*)(ldr_stub + 8) = target_addr;
    if (x64) {
        *((uint32_t*)ldr_stub + 4) = (uint32_t)0xf9400000
                                   | (uint32_t)(reg_num << 5)
                                   | reg_num;
    } else {
        *((uint32_t*)ldr_stub + 4) = (uint32_t)0xb9400000
                                   | (uint32_t)(reg_num << 5)
                                   | reg_num;
    }

    memcpy(dst, ldr_stub, sizeof(ldr_stub));

    return sizeof(ldr_stub);
}

uint8_t aarch64_put_ldr32( uint32_t *dst
                         , uint64_t pc
                         , uint64_t target_addr
                         , uint8_t reg_num
                         )
{
    return aarch64_put_ldr(dst, pc, target_addr, reg_num, false);
}

uint8_t aarch64_put_ldr32_stub( uint32_t *dst
                              , uint64_t target_addr
                              , uint8_t reg_num
                              )
{
    return aarch64_put_ldr_stub(dst, target_addr, reg_num, false);
}

uint8_t aarch64_put_ldr64( uint32_t *dst
                         , uint64_t pc
                         , uint64_t target_addr
                         , uint8_t reg_num
                         )
{
    return aarch64_put_ldr(dst, pc, target_addr, reg_num, true);
}

uint8_t aarch64_put_ldr64_stub( uint32_t *dst
                              , uint64_t target_addr
                              , uint8_t reg_num
                              )
{
    return aarch64_put_ldr_stub(dst, target_addr, reg_num, true);
}

uint8_t aarch64_put_b( uint32_t *dst
                     , uint64_t pc
                     , uint64_t target_addr
                     )
{
    int64_t imm26 = SIGN_EXTEND((int64_t)(target_addr - pc) >> 2, 25);
    *dst = 0x14000000 | (imm26 & 0x3ffffff);
    return 4;
}

uint8_t aarch64_put_b_stub( uint32_t *dst
                          , uint64_t target_addr
                          , uint64_t x30_addr
                          )
{
    uint8_t b_stub[] = {
        0xfe, 0x77, 0xbf, 0xa9, // stp x30, x29, [sp, #-16]!
        0x7d, 0x01, 0x00, 0x58, // ldr x29, 0x2c
        0xbe, 0x03, 0x40, 0xf9, // ldr x30, [x29]
        0x7e, 0x00, 0x00, 0xb5, // cbnz x30, 0xc
        0xfe, 0x03, 0x40, 0xf9, // ldr x30, [sp, #16]
        0xbe, 0x03, 0x00, 0xf9, // str x30, [x29]
        0xfe, 0x77, 0xc1, 0xa8, // ldp x30, x29, [sp], #16
        0xfe, 0x00, 0x00, 0x58, // ldr x30, 0x1c
        0xc0, 0x03, 0x3f, 0xd6, // blr x30
        0x7e, 0x00, 0x00, 0x58, // ldr x30, 0xc
        0xde, 0x03, 0x40, 0xf9, // ldr x30, [x30]
        0xc0, 0x03, 0x5f, 0xd6, // ret
        0xef, 0xbe, 0xad, 0xde, 0xef, 0xbe, 0xad, 0xde, // .quad 0xdeadbeefdeadbeef
        0xef, 0xbe, 0xad, 0xde, 0xef, 0xbe, 0xad, 0xde, // .quad 0xdeadbeefdeadbeef
    };

    *(uint64_t*)(b_stub + 48) = x30_addr;
    *(uint64_t*)(b_stub + 56) = target_addr;

    memcpy(dst, b_stub, sizeof(b_stub));

    return sizeof(b_stub);
}

uint8_t aarch64_put_adrp( uint32_t *dst
                        , uint64_t pc
                        , uint64_t target_page_addr
                        , uint8_t reg_num
                        )
{
    reg_num &= 0x1f;
    pc &= (uint64_t)(~0xFFF);
    int64_t imm21 = SIGN_EXTEND((target_page_addr - pc) >> 12, 20);
    STDERROR_PRINT("imm21 %"PRIx64"\n", imm21);
    STDERROR_PRINT("lo:imm21 %"PRIx64"\n", ((imm21 & 0x3) << 29));
    STDERROR_PRINT("hi:imm21 %"PRIx64"\n", (((imm21 >> 2) & 0x7ffff) << 5));
    uint32_t instr = 0x90000000
                   | ((imm21 & 0x3) << 29)
                   | (((imm21 >> 2) & 0x7ffff) << 5)
                   | reg_num;
    STDERROR_PRINT("instr %"PRIx32"\n", instr);
    *dst = instr;
    return 4;
}

uint8_t aarch64_put_adrp_stub( uint32_t *dst
                             , uint64_t target_page_addr
                             , uint8_t reg_num
                             )
{
    // ldr x[reg_num], 0x8      |
    // b 0xc                    | 0x14000003
    // .quad 0xdeadbeefdeadbeef | 0xdeadbeefdeadbeef
    uint8_t adrp_stub[] = {
        0x40, 0x00, 0x00, 0x58, // ldr x0, 0x8
        0x03, 0x00, 0x00, 0x14,
        0xef, 0xbe, 0xad, 0xde, 0xef, 0xbe, 0xad, 0xde,
    };

    *(uint64_t*)(adrp_stub + 8) = target_page_addr;
    *(uint32_t*)adrp_stub = (uint32_t)(0x58000040 | (reg_num & 0x1f));
    memcpy(dst, adrp_stub, sizeof(adrp_stub));

    return sizeof(adrp_stub);
}


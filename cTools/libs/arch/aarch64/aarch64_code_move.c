/***
 * MIT License
 *
 * Copyright (c) 2023-2026 Konychev Valerii
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
#include "vt_containers.h"

#include <inttypes.h>

#define GET_PRE_INIT_BT_RELOC(sv_rel)                                          \
    (                                                                          \
        {                                                                      \
            static bt_reloc local_r = {0};                                     \
            local_r.old_pc = old_pc;                                           \
            local_r.new_pc = new_pc;                                           \
            local_r.old_size = 4;                                              \
            local_r.new_size = 4;                                              \
            local_r.type = RELOC_GP;                                           \
            bt_reloc *r = &local_r;                                            \
            if (sv_rel)                                                        \
                r = (bt_reloc*) vt_sorted_vector_find_elem((sv_rel), &local_r);\
            r;                                                                 \
        }                                                                      \
    );

static int
pre_init_relocations( vt_sorted_vector_t *rel
                    , uint64_t old_pc
                    , uint64_t new_pc
                    , uint64_t instr_num
                    )
{
    uint64_t i = 0;
    for (i = 0; i < instr_num; ++i) {
        bt_reloc r = {
            .old_pc = old_pc + (i * 4),
            .new_pc = new_pc + (i * 4),
            .type = RELOC_GP,
            .old_size = 4,
            .new_size = 4,
        };

        if (vt_sorted_vector_insert(rel, &r)) {
            STDERROR_PRINT("fail\n");
            return -1;
        }
    }

    return 0;
}

static uint8_t
aarch64_move_adr( uint32_t instr
                , uint8_t  *dst
                , uint64_t old_pc
                , uint64_t new_pc
                , vt_sorted_vector_t *rel
                )
{
    bt_reloc *r = GET_PRE_INIT_BT_RELOC(rel);

    uint8_t reg_num = instr & 0x1F;
    int64_t imm21 = (((instr >> 5) << 2) | ((instr >> 29) & 0x3))
                    & 0x1fffff;
    imm21 = SIGN_EXTEND(imm21, 20);
    uint64_t target_addr = old_pc + (uint64_t)imm21;
    r->old_target = target_addr;
    int64_t new_imm21 = (target_addr - new_pc) & 0x1fffff;
    new_imm21 = SIGN_EXTEND(new_imm21, 20);
    uint64_t new_target_addr = new_pc + (uint64_t)(new_imm21);
    if (target_addr == new_target_addr) {
        // We can fit offset into 21 bits.
        STDERROR_LOG("ADR\n");
        r->type = RELOC_AARCH64_ADR;
        r->new_size = aarch64_put_adr( (uint32_t*)dst
                                     , new_pc
                                     , target_addr
                                     , reg_num
                                     );
    } else if (IS_N_BITS_ENOUGH_64(target_addr - new_pc, 33)) {
        // We cannot fit offset into 21 bits. Need to place adr_stub.
        STDERROR_LOG("ADR 33 bits enough\n");
        r->type = RELOC_AARCH64_ADR_REL33;
        r->new_size = aarch64_put_adr_stub_rel33( (uint32_t*)dst
                                                , new_pc
                                                , target_addr
                                                , reg_num
                                                );
    } else if (IS_N_BITS_ENOUGH_64(target_addr - new_pc, 48)) {
        STDERROR_LOG("ADR 48 bits enough\n");
        r->type = RELOC_AARCH64_ADR_ABS48;
        r->new_size = aarch64_put_adr_stub_abs48( (uint32_t*)dst
                                                , target_addr
                                                , reg_num
                                                );
    } else {
        r->type = RELOC_AARCH64_ADR_ABS;
        r->new_size = aarch64_put_adr_stub_abs( (uint32_t*)dst
                                              , target_addr
                                              , reg_num
                                              );
    }

    return r->new_size;
}

static uint8_t
aarch64_move_adrp( uint32_t instr
                 , uint8_t  *dst
                 , uint64_t old_pc
                 , uint64_t new_pc
                 , vt_sorted_vector_t *rel
                 )
{
    bt_reloc *r = GET_PRE_INIT_BT_RELOC(rel);

    uint8_t reg_num = instr & 0x1F;
    uint64_t old_page_pc = old_pc & (uint64_t)(~0xFFF);
    uint64_t new_page_pc = new_pc & (uint64_t)(~0xFFF);
    int64_t imm21 = (((instr >> 5) << 2) | ((instr >> 29) & 0x3))
                    & 0x1fffff;
    imm21 = SIGN_EXTEND(imm21, 20);
    STDERROR_LOG("ADRP: old_pc: 0x%"PRIx64"\n", old_pc);
    STDERROR_LOG("ADRP: instr: %"PRIx32"\n", instr);
    STDERROR_LOG("ADRP: imm19: %"PRIx64"\n", (uint64_t)imm21);
    uint64_t target_addr = old_page_pc + ((uint64_t)imm21 << 12);
    r->old_target = target_addr;
    STDERROR_LOG("ADRP: target_addr: %"PRIx64"\n", target_addr);

    int64_t new_imm21 = ((target_addr - new_page_pc) >> 12) & 0x1fffff;
    new_imm21 = SIGN_EXTEND(new_imm21, 20);
    uint64_t new_target_addr = new_page_pc + ((uint64_t)new_imm21 << 12);
    if (target_addr == new_target_addr) {
        // We can fit offset into 21 bits. +- 4GB
        STDERROR_LOG("ADRP\n");
        r->type = RELOC_AARCH64_ADRP;
        r->new_size = aarch64_put_adrp( (uint32_t*)dst
                                      , new_pc
                                      , target_addr
                                      , reg_num
                                      );
    } else if (IS_N_BITS_ENOUGH_64(target_addr - new_pc, 48)) {
        // We cannot fit offset into 21 bits. Need to place adr_stub.
        STDERROR_LOG("ADRP 48 bits enough\n");
        r->type = RELOC_AARCH64_ADRP_ABS48;
        r->new_size = aarch64_put_adrp_stub_abs48( (uint32_t*)dst
                                                 , target_addr
                                                 , reg_num
                                                 );
    } else {
        STDERROR_LOG("ADRP 64 bits stub\n");
        r->type = RELOC_AARCH64_ADRP_ABS;
        r->new_size = aarch64_put_adrp_stub_abs( (uint32_t*)dst
                                               , target_addr
                                               , reg_num
                                               );
    }

    return r->new_size;
}

static uint8_t
aarch64_move_b( uint32_t instr
              , uint8_t  *dst
              , uint64_t old_pc
              , uint64_t new_pc
              , vt_sorted_vector_t *rel
              )
{
    bt_reloc *r = GET_PRE_INIT_BT_RELOC(rel);

    int64_t imm26 = SIGN_EXTEND(instr & 0x3ffffff, 25);
    uint64_t target_addr = old_pc + ((uint64_t)imm26 << 2);
    r->old_target = target_addr;
    int64_t new_imm26 = ((target_addr - new_pc) >> 2) & 0x3ffffff;
    new_imm26 = SIGN_EXTEND(new_imm26, 25);
    uint64_t new_target_addr = new_pc + ((uint64_t)new_imm26 << 2);
    if (target_addr == new_target_addr) {
        STDERROR_LOG("B\n");
        // We can fit offset into 26 bits.
        r->type = RELOC_AARCH64_B;
        r->new_size = aarch64_put_b( (uint32_t*)dst
                                   , new_pc
                                   , target_addr
                                   );
    } else {
        STDERROR_LOG("B_STUB\n");
        STDERROR_LOG("target_addr: %"PRIx64"\n", target_addr);
        // We cannot fit offset into 26 bits. Need to place b_stub.
        r->type = RELOC_AARCH64_B_ABS;
        r->new_size = aarch64_put_b_stub_abs( (uint32_t*)dst
                                            , target_addr
                                            );
    }

    return r->new_size;
}

static uint8_t
aarch64_move_bl( uint32_t instr
               , uint8_t  *dst
               , uint64_t old_pc
               , uint64_t new_pc
               , vt_sorted_vector_t *rel
               )
{
    bt_reloc *r = GET_PRE_INIT_BT_RELOC(rel);

    int64_t imm26 = SIGN_EXTEND(instr & 0x3ffffff, 25);
    uint64_t target_addr = old_pc + ((uint64_t)imm26 << 2);
    r->old_target = target_addr;
    int64_t new_imm26 = (int64_t)(target_addr - new_pc) & 0x3ffffff;
    new_imm26 = SIGN_EXTEND(new_imm26 >> 2, 25);
    uint64_t new_target_addr = new_pc + ((uint64_t)(new_imm26) << 2);
    if (target_addr == new_target_addr) {
        // We can fit offset into 26 bits.
        STDERROR_LOG("BL\n");
        r->type = RELOC_AARCH64_BL;
        r->new_size = aarch64_put_bl( (uint32_t*)dst
                                    , new_pc
                                    , target_addr
                                    );
    } else if (IS_N_BITS_ENOUGH_64(target_addr - new_pc, 33)) {
        STDERROR_LOG("BL 33 bits enough\n");
        r->type = RELOC_AARCH64_BL_REL33;
        r->new_size = aarch64_put_bl_stub_rel33( (uint32_t*)dst
                                               , new_pc
                                               , target_addr
                                               );
    } else if (IS_N_BITS_ENOUGH_64(target_addr - new_pc, 48)) {
        STDERROR_LOG("BL 48 bits enough\n");
        r->type = RELOC_AARCH64_BL_ABS48;
        r->new_size = aarch64_put_bl_stub_abs48( (uint32_t*)dst
                                               , target_addr
                                               );
    } else {
        r->type = RELOC_AARCH64_BL_ABS;
        r->new_size = aarch64_put_bl_stub_abs( (uint32_t*)dst
                                             , target_addr
                                             );
    }

    return r->new_size;
}

static uint8_t
aarch64_move_ldr( uint32_t instr
                , uint8_t  *dst
                , uint64_t old_pc
                , uint64_t new_pc
                , bool x64
                , vt_sorted_vector_t *rel
                )
{
    bt_reloc *r = GET_PRE_INIT_BT_RELOC(rel);

    uint8_t reg_num = instr & 0x1F;
    int64_t imm19 = SIGN_EXTEND((instr >> 5) & 0x7ffff, 18);
    uint64_t target_addr = old_pc + ((uint64_t)imm19 << 2);
    r->old_target = target_addr;
    int64_t new_imm19 = ((target_addr - new_pc) >> 2) & 0x7ffff;
    new_imm19 = SIGN_EXTEND(new_imm19, 18);
    uint64_t new_target_addr = new_pc + ((uint64_t)new_imm19 << 2);
    if (target_addr == new_target_addr) {
        // We can fit offset into 19 bits.
        r->type = RELOC_AARCH64_LDR;
        r->new_size = aarch64_put_ldr((uint32_t*)dst
                                     , new_pc
                                     , target_addr
                                     , reg_num, x64
                                     );
    } else if (IS_N_BITS_ENOUGH_64(target_addr - new_pc, 33)) {
        // We cannot fit offset into 19 bits. Need to place ldr_stub.
        STDERROR_LOG("33 bits\n");
        r->type = RELOC_AARCH64_LDR_REL33;
        r->new_size = aarch64_put_ldr_stub_rel33((uint32_t*)dst
                                                , new_pc
                                                , target_addr
                                                , reg_num
                                                , x64
                                                );
    } else if (IS_N_BITS_ENOUGH_64(target_addr - new_pc, 48)) {
        // We cannot fit offset into 19 bits. Need to place ldr_stub.
        STDERROR_LOG("48 bits\n");
        r->type = RELOC_AARCH64_LDR_ABS48;
        r->new_size = aarch64_put_ldr_stub_abs48((uint32_t*)dst
                                                , target_addr
                                                , reg_num
                                                , x64
                                                );
    } else {
        r->type = RELOC_AARCH64_LDR_ABS;
        r->new_size = aarch64_put_ldr_stub_abs((uint32_t*)dst
                                              , target_addr
                                              , reg_num
                                              , x64
                                              );
    }

    return r->new_size;
}

static uint8_t
aarch64_move_b_cond( uint32_t instr
                   , uint8_t  *dst
                   , uint64_t old_pc
                   , uint64_t new_pc
                   , vt_sorted_vector_t *rel
                   )
{
    bt_reloc *r = GET_PRE_INIT_BT_RELOC(rel);

    uint8_t cond = instr & 0xF;
    int64_t imm19 = SIGN_EXTEND((instr >> 5) & 0x7FFFF, 18);

    STDERROR_LOG("BC: old_pc: %"PRIx64"\n", (uint64_t)old_pc);
    STDERROR_LOG("BC: instr: %"PRIx32"\n", instr);
    STDERROR_LOG("BC: imm19: %"PRIx64"\n", (uint64_t)imm19);
    uint64_t target_addr = old_pc + ((uint64_t)imm19 << 2);
    r->old_target = target_addr;
    STDERROR_LOG("BC: original_target: %"PRIx64"\n", target_addr);
    target_addr = get_instr_new_addr(target_addr, rel);
    r->new_target = target_addr;
    STDERROR_LOG("BC: final_target: %"PRIx64"\n", target_addr);

    int64_t new_imm19 = ((target_addr - new_pc) >> 2) & 0x7FFFF;
    new_imm19 = SIGN_EXTEND(new_imm19, 18);

    uint64_t new_target_addr = new_pc + ((uint64_t)new_imm19 << 2);
    if (target_addr == new_target_addr) {
        STDERROR_LOG("BC\n");
        // We can fit offset into 26 bits.
        r->type = RELOC_AARCH64_B_COND;
        r->new_size = aarch64_put_b_cond( (uint32_t*)dst
                                        , new_pc
                                        , target_addr
                                        , cond
                                        );
    } else {
        STDERROR_LOG("BC: cannot fit in the 19 bits\n");
        r->type = RELOC_AARCH64_B_COND_ABS;
        r->new_size = aarch64_put_b_cond_stub_abs( (uint32_t*)dst
                                                 , target_addr
                                                 , cond
                                                 );
    }

    return r->new_size;
}

static uint8_t
aarch64_move_cb( uint32_t instr
               , uint8_t  *dst
               , uint64_t old_pc
               , uint64_t new_pc
               , vt_sorted_vector_t *rel
               )
{
    bt_reloc *r = GET_PRE_INIT_BT_RELOC(rel);

    bool x64 = !!(instr & 0x80000000);
    bool non_zero = !!(instr & 0x01000000);
    uint8_t reg_num = instr & 0x1F;
    int64_t imm19 = SIGN_EXTEND((instr >> 5) & 0x7FFFF, 18);

    uint64_t target_addr = old_pc + ((uint64_t)imm19 << 2);
    r->old_target = target_addr;
    target_addr = get_instr_new_addr(target_addr, rel);
    r->new_target = target_addr;

    int64_t new_imm19 = ((target_addr - new_pc) >> 2) & 0x7FFFF;
    new_imm19 = SIGN_EXTEND(new_imm19, 18);

    uint64_t new_target_addr = new_pc + ((uint64_t)new_imm19 << 2);
    if (target_addr == new_target_addr) {
        STDERROR_LOG("CB\n");
        // We can fit offset into 26 bits.
        r->type = RELOC_AARCH64_CB;
        r->new_size = aarch64_put_cb( (uint32_t*)dst
                                    , new_pc
                                    , target_addr
                                    , reg_num
                                    , non_zero
                                    , x64
                                    );
    } else {
        STDERROR_LOG("CB: cannot fit in the 19 bits\n");
    }

    return r->new_size;
}

CODE_MOVE_ERROR
aarch64_instr_move( const uint8_t *src
                  , uint8_t  *dst
                  , uint64_t old_pc
                  , uint64_t new_pc
                  , uint64_t dst_size
                  , vt_sorted_vector_t *rel
                  )
{
    if (src == NULL || dst == NULL || dst_size == (uint64_t)(-1)) {
        return CODE_MOVE_ERROR_BAD_ARG;
    }

    STDERROR_LOG("old_pc 0x%"PRIx64", new_pc 0x%"PRIx64"\n", old_pc, new_pc);
    uint32_t instr = *(const uint32_t*)src;
    switch (aarch64_get_instr_type(instr)) {
    case AARCH64_INSTR_TYPE_RESERVED:
    case AARCH64_INSTR_TYPE_UNKNOWN:
        return CODE_MOVE_ERROR_UNKNOWN_INSTR;
    default:
        break;
    }

    bool x64 = true;
    bt_reloc *r = GET_PRE_INIT_BT_RELOC(rel);
    r->new_pc = new_pc;
    r->new_size = 4;
    switch (aarch64_get_instr_op(instr)) {
    case AARCH64_INSTR_OP_ADR:
    {
        r->new_size = aarch64_move_adr(instr, dst, old_pc, new_pc, rel);
        break;
    }
    case AARCH64_INSTR_OP_ADRP:
    {
        r->new_size = aarch64_move_adrp(instr, dst, old_pc, new_pc, rel);
        break;
    }
    case AARCH64_INSTR_OP_B:
    {
        r->new_size = aarch64_move_b(instr, dst, old_pc, new_pc, rel);
        break;
    }
    case AARCH64_INSTR_OP_BL:
    {
        r->new_size = aarch64_move_bl(instr, dst, old_pc, new_pc, rel);
        break;
    }
    case AARCH64_INSTR_OP_LDR32:
        x64 = false;
        FALLTHROUGH;
    case AARCH64_INSTR_OP_LDR64:
    {
        r->new_size = aarch64_move_ldr(instr, dst, old_pc, new_pc, x64, rel);
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
        FALLTHROUGH;
    case AARCH64_INSTR_OP_BLE:
    {
        r->new_size = aarch64_move_b_cond(instr, dst, old_pc, new_pc, rel);
        break;
    }
    case AARCH64_INSTR_OP_CBZ32:
    case AARCH64_INSTR_OP_CBZ64:
    case AARCH64_INSTR_OP_CBNZ32:
        FALLTHROUGH;
    case AARCH64_INSTR_OP_CBNZ64:
    {
        STDERROR_LOG("CB: old instr: %"PRIx32"\n", instr);
        r->new_size = aarch64_move_cb(instr, dst, old_pc, new_pc, rel);
        STDERROR_LOG("CB: new instr: %"PRIx32"\n", *(uint32_t*)dst);
        break;
    }
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
        //STDERROR_LOG("dst %p\n", dst);
        *(uint32_t*)dst = instr;
        break;
    }

    return (CODE_MOVE_ERROR)r->new_size;
}

extern void
resolve_relocations( vt_sorted_vector_t *rel
                   , uint8_t *dst
                   , uint64_t old_pc
                   , uint64_t instr_num
                   );
void
resolve_relocations( vt_sorted_vector_t *rel
                   , uint8_t *dst
                   , uint64_t old_pc
                   , uint64_t instr_num
                   )
{
    bt_reloc start_reloc = {
        .old_pc = old_pc,
    };

    bt_reloc *r = (bt_reloc*) vt_sorted_vector_find_elem(rel, &start_reloc);
    uint64_t i = 0;
    uint8_t *instr_p = dst;
    for (i = 0; i < instr_num; instr_p += r[i].new_size, ++i) {
        STDERROR_PRINT("\tinstr_p: %p | new_size %u\n", (void*)instr_p, r[i].new_size);
        if (r[i].type == RELOC_GP) {
            // General instructions doesn't need to rebase
            continue;
        }

        bt_reloc target_r = {
            .old_pc = r[i].old_target,
        };
        bt_reloc *old_target = (bt_reloc*) vt_sorted_vector_find_elem(rel, &target_r);
        if (old_target == NULL) {
            // Old target wasn't relocated, so don't need to rebase
            continue;
        }
        if (old_target->new_pc == r[i].new_target) {
            // Relocated target has position as we expected, don't need to rebase
            continue;
        }

        r[i].new_target = old_target->new_pc;
        uint8_t reg_num = (*((uint32_t*)instr_p)) & 0x1F;
        switch (r[i].type) {
        case RELOC_AARCH64_ADR:
            aarch64_put_adr( (uint32_t*)instr_p
                           , r[i].new_pc
                           , r[i].new_target
                           , reg_num
                           );
            break;
        case RELOC_AARCH64_ADR_REL33:
            aarch64_put_adr_stub_rel33( (uint32_t*)instr_p
                                      , r[i].new_pc
                                      , r[i].new_target
                                      , reg_num
                                      );
            break;
        case RELOC_AARCH64_ADR_ABS48:
            aarch64_put_adr_stub_abs48( (uint32_t*)instr_p
                                      , r[i].new_target
                                      , reg_num
                                      );
            break;
        case RELOC_AARCH64_ADR_ABS:
            aarch64_put_adr_stub_abs( (uint32_t*)instr_p
                                    , r[i].new_target
                                    , reg_num
                                    );
            break;
        case RELOC_AARCH64_ADRP:
            /*STDERROR_LOG("Reloc ADRP:\n");
            STDERROR_LOG("\told_target: 0x%"PRIx64", new_pc_old_target: 0x%"PRIx64"\n", r[i].old_target
                    , old_target->new_pc);

            aarch64_put_adrp( (uint32_t*)instr_p
                            , r[i].new_pc
                            , r[i].new_target
                            , reg_num
                            );*/
            break;
        case RELOC_AARCH64_ADRP_ABS48:
            aarch64_put_adrp_stub_abs48( (uint32_t*)instr_p
                                       , r[i].new_target
                                       , reg_num
                                       );
            break;
        case RELOC_AARCH64_ADRP_ABS:
            aarch64_put_adrp_stub_abs( (uint32_t*)instr_p
                                     , r[i].new_target
                                     , reg_num
                                     );
            break;
        case RELOC_AARCH64_B:
            STDERROR_LOG("Reloc B:\n");
            STDERROR_LOG("\told_target: 0x%"PRIx64", new_pc_old_target: 0x%"PRIx64"\n", r[i].old_target
                    , old_target->new_pc);
            aarch64_put_b( (uint32_t*)instr_p
                         , r[i].new_pc
                         , r[i].new_target);
            break;
        /*
        case RELOC_AARCH64_B_X16_REL33:
        aarch64_put_b_x16_stub_rel33( (uint32_t*)instr_p
                                     , uint64_t pc
                                     , r[i].new_target);
            break;
        case RELOC_AARCH64_B_X16_ABS48:
        aarch64_put_b_x16_stub_abs48( (uint32_t*)instr_p
                                    , r[i].new_target);
            break;
        case RELOC_AARCH64_B_X16_ABS:
        aarch64_put_b_x16_stub_abs( (uint32_t*)instr_p
                                  , r[i].new_target);
            break;
        case RELOC_AARCH64_B_X17_REL33:
            break;
        case RELOC_AARCH64_B_X17_ABS48:
            break;
        case RELOC_AARCH64_B_X17_ABS:
            break;
        case RELOC_AARCH64_B_ABS48:
            break;
        */
        case RELOC_AARCH64_B_ABS:
        {
            //uint64_t x30_addr = *(uint64_t*)(instr_p + 48);
            aarch64_put_b_stub_abs( (uint32_t*)instr_p
                                  , r[i].new_target
                                  );
            break;
        }
        case RELOC_AARCH64_BL:
            STDERROR_LOG("Reloc BL\n");
            aarch64_put_bl( (uint32_t*)instr_p
                          , r[i].new_pc
                          , r[i].new_target);
            break;
        case RELOC_AARCH64_BL_REL33:
            STDERROR_LOG("Reloc BL_REL33\n");
            aarch64_put_bl_stub_rel33( (uint32_t*)instr_p
                                     , r[i].new_pc
                                     , r[i].new_target
                                     );
            break;
        case RELOC_AARCH64_BL_ABS48:
            aarch64_put_bl_stub_abs48( (uint32_t*)instr_p
                                     , r[i].new_target
                                     );
            break;
        case RELOC_AARCH64_BL_ABS:
            aarch64_put_bl_stub_abs( (uint32_t*)instr_p
                                   , r[i].new_target
                                   );
            break;
        case RELOC_AARCH64_LDR:
        {
            bool x64 = !!((*((uint32_t*)instr_p)) & 0x40000000);
            aarch64_put_ldr( (uint32_t*)instr_p
                           , r[i].new_pc
                           , r[i].new_target
                           , reg_num
                           , x64
                           );
            break;
        }
        case RELOC_AARCH64_LDR_REL33:
        {
            bool x64 = !!((*(((uint32_t*)instr_p) + 2)) & 0x40000000);
            aarch64_put_ldr_stub_rel33( (uint32_t*)instr_p
                                      , r[i].new_pc
                                      , r[i].new_target
                                      , reg_num
                                      , x64
                                      );
            break;
        }
        case RELOC_AARCH64_LDR_ABS48:
        {
            bool x64 = !!((*(((uint32_t*)instr_p) + 3)) & 0x40000000);
            aarch64_put_ldr_stub_abs48( (uint32_t*)instr_p
                                      , r[i].new_target
                                      , reg_num
                                      , x64
                                      );
            break;
        }
        case RELOC_AARCH64_LDR_ABS:
        {
            bool x64 = !!((*(((uint32_t*)instr_p) + 4)) & 0x40000000);
            aarch64_put_ldr_stub_abs( (uint32_t*)instr_p
                                    , r[i].new_target
                                    , reg_num
                                    , x64
                                    );
            break;
        }
        case RELOC_AARCH64_B_COND:
        {
            STDERROR_PRINT("Reloc B_COND:\n");
            STDERROR_LOG("\told_target: 0x%"PRIx64", new_pc_old_target: 0x%"PRIx64"\n", r[i].old_target
                    , old_target->new_pc);
            uint8_t cond = (*((uint32_t*)instr_p)) & 0xF;
            aarch64_put_b_cond( (uint32_t*)instr_p
                              , r[i].new_pc
                              , r[i].new_target
                              , cond
                              );
            break;
        }
        case RELOC_AARCH64_B_COND_ABS:
        {
            STDERROR_PRINT("Reloc B_COND_ABS:\n");
            uint8_t cond = (*((uint32_t*)instr_p)) & 0xF;
            aarch64_put_b_cond_stub_abs( (uint32_t*)instr_p
                                       , r[i].new_target
                                       , cond
                                       );
            break;
        }
        case RELOC_AARCH64_CB:
        {
            STDERROR_LOG("Reloc CB:\n");
            STDERROR_LOG("\told_target: 0x%"PRIx64", new_pc_old_target: 0x%"PRIx64"\n", r[i].old_target
                    , old_target->new_pc);
            bool x64 = !!((*((uint32_t*)instr_p)) & 0x80000000);
            bool non_zero = !!((*((uint32_t*)instr_p)) & 0x01000000);

            STDERROR_LOG("\tPre Final CB: %"PRIx32"\n", *(uint32_t*)instr_p);
            aarch64_put_cb( (uint32_t*)instr_p
                          , r[i].new_pc
                          , r[i].new_target
                          , reg_num
                          , non_zero
                          , x64
                          );
            STDERROR_LOG("\tFinal CB: %"PRIx32"\n", *(uint32_t*)instr_p);
            break;
        }
        default:
            break;
        }
    }
}

CODE_MOVE_ERROR
aarch64_code_move( const uint8_t *src
                 , uint8_t  *dst
                 , uint64_t old_pc
                 , uint64_t new_pc
                 , uint64_t src_size
                 , uint64_t dst_size
                 , vt_sorted_vector_t *rel
                 )
{
    if (src == NULL                || dst == NULL ||
        src_size == (uint64_t)(-1) || dst_size == (uint64_t)(-1)) {
        return CODE_MOVE_ERROR_BAD_ARG;
    }

    vt_sorted_vector_t local_rel = {0};
    vt_sorted_vector_init(&local_rel, 1, sizeof(bt_reloc), cmp_bt_reloc);
    if (rel == NULL) {
        rel = &local_rel;
    }

    uint64_t i = 0;
    uint64_t new_code_size = 0;
    uint64_t instr_num = src_size / 4;
    //uint64_t top_bound = old_pc + src_size;
    //uint64_t bottom_bound = old_pc;
    if (pre_init_relocations(rel, old_pc, new_pc, instr_num)) {
        return CODE_MOVE_ERROR_NO_MEM;
    }

    for (i = 0; i < instr_num; ++i) {
        uint64_t new_instr_size = (uint64_t) aarch64_instr_move( src + (i * 4)
                                                               , dst + new_code_size
                                                               , old_pc + (i * 4)
                                                               , new_pc + new_code_size
                                                               , dst_size
                                                               , rel
                                                               );
        //print_bytes(stderr, dst + new_code_size, new_instr_size);
        //putchar('\n');

        new_code_size += new_instr_size;
    }

    resolve_relocations(rel, dst, old_pc, instr_num);

    vt_sorted_vector_fini(&local_rel);
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
            if (max_estimate) {
                new_instr_size = SIZE_ADR_STUB_ABS;
                break;
            }

            int64_t imm21 = (((instr >> 5) << 2) | (instr >> 29)) & 0x1fffff;
            imm21 = SIGN_EXTEND(imm21, 20);
            uint64_t target_addr = old_pc + (uint64_t)imm21;
            int64_t new_imm21 = (target_addr - new_pc) & 0x1fffff;
            new_imm21 = SIGN_EXTEND(new_imm21, 20);
            uint64_t new_target_addr = new_pc + (uint64_t)(new_imm21);
            if (target_addr != new_target_addr) {
                // We cannot fit offset into 21 bits. Need to place adr_stub.
                if (IS_N_BITS_ENOUGH_64(target_addr - new_pc, 33)) {
                    new_instr_size = SIZE_ADR_STUB_REL33;
                } else {
                    new_instr_size = SIZE_ADR_STUB_ABS;
                }
            }

            break;
        }
        case AARCH64_INSTR_OP_ADRP:
        {
            if (max_estimate) {
                new_instr_size = SIZE_ADRP_STUB_ABS;
                break;
            }

            uint64_t old_page_pc = old_pc & (uint64_t)(~0xFFF);
            uint64_t new_page_pc = new_pc & (uint64_t)(~0xFFF);
            int64_t imm21 = (((instr >> 5) << 2) | ((instr >> 29) & 0x3))
                        & 0x1fffff;
            imm21 = SIGN_EXTEND(imm21, 20);
            uint64_t target_addr = old_page_pc + ((uint64_t)imm21 << 12);
            int64_t new_imm21 = ((target_addr - new_page_pc) >> 12) & 0x1fffff;
            new_imm21 = SIGN_EXTEND(new_imm21, 20);
            uint64_t new_target_addr = new_page_pc + ((uint64_t)new_imm21 << 12);
            if (target_addr != new_target_addr) {
                // We cannot fit offset into 21 bits. Need to place adr_stub.
                new_instr_size = SIZE_ADRP_STUB_ABS;
            }

            break;
        }
        case AARCH64_INSTR_OP_B:
        {
            if (max_estimate) {
                new_instr_size = SIZE_B_STUB_ABS;
                break;
            }

            int64_t imm26 = SIGN_EXTEND(instr & 0x3ffffff, 25);
            uint64_t target_addr = old_pc + ((uint64_t)imm26 << 2);
            int64_t new_imm26 = ((target_addr - new_pc) >> 2) & 0x3ffffff;
            new_imm26 = SIGN_EXTEND(new_imm26, 25);
            uint64_t new_target_addr = new_pc + ((uint64_t)new_imm26 << 2);
            if (target_addr != new_target_addr) {
                // We cannot fit offset into 26 bits. Need to place b_stub.
                new_instr_size = SIZE_B_STUB_ABS;
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
                    new_instr_size = SIZE_BL_STUB_REL33;
                } else {
                    new_instr_size = SIZE_BL_STUB_ABS;
                }
            }

            break;
        }
        case AARCH64_INSTR_OP_LDR32:
        case AARCH64_INSTR_OP_LDR64:
        {
            if (max_estimate) {
                new_instr_size = SIZE_LDR_STUB_ABS;
                break;
            }

            int64_t imm19 = SIGN_EXTEND(instr & 0x7ffff, 18);
            uint64_t target_addr = old_pc + (uint64_t)(imm19 << 2);
            int64_t new_imm19 = (target_addr - new_pc) & 0x7ffff;
            new_imm19 = SIGN_EXTEND(new_imm19, 18);
            uint64_t new_target_addr = new_pc + (uint64_t)(new_imm19);
            if (target_addr != new_target_addr) {
                // We cannot fit offset into 19 bits. Need to place ldr_stub.
                if (IS_N_BITS_ENOUGH_64(target_addr - new_pc, 33)) {
                    new_instr_size = SIZE_LDR_STUB_REL33;
                } else {
                    new_instr_size = SIZE_LDR_STUB_ABS;
                }
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


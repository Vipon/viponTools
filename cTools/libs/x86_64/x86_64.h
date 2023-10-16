/***
 * MIT License
 *
 * Copyright (c) 2020-2023 Konychev Valera
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

#ifndef _X86_64_H
#define _X86_64_H

/* C standard headers */
#include <stdint.h>

/* disassembler */
#include <capstone/capstone.h>

#define x86_MOD(modrm)          ((modrm >> 6) & 0x03)
#define x86_REG(modrm)          ((modrm >> 3) & 0x07)
#define x86_RM(modrm)           (modrm & 0x07)
#define IS_SIB(modrm)           (x86_RM(modrm) == 0x04 && x86_MOD(modrm) != 0x03)
#define x86_IS_RIPREL(modrm)    (x86_MOD(modrm) == 0 && x86_RM(modrm) == 0x05)

#define x86_SCALE(SIB)          ((modrm >> 6) & 0x03)
#define x86_INDEX(SIB)          ((modrm >> 3) & 0x07)
#define x86_BASE(SIB)           (SIB & 0x07)

#define X86_MAX_INSTR_LEN       15


/*
 * Description:
 *  Addressing types of x86 instructions.
 */
typedef enum {
    NO_X86_ADDR_TYPE = 0,       /* Instruction without addressing. */
    RELATIVE_X86_ADDR_TYPE,     /* Instruction with relative addressing. */
    RIP_RELATIVE_X86_ADDR_TYPE, /* Instruction with RIP-relative addressing. */
    ABSOLUTE_X86_ADDR_TYPE      /* Instruction with absolute addressing. */
} X86_ADDR_TYPE;


#define CLOBBERED_ALL_REGS_X86_64   "rax", "rbx", "rcx", "rdx", \
                                    "rdi", "rsi", "rbp", "rsp", \
                                    "r8", "r9", "r10", "r11",   \
                                    "r12", "r13", "r14", "r15", "memory"


/*
 *  jmp to the next instruction. In x86 arhitecture jmp flushes pipeline.
 */
#define VALIDATE_CONTROL_FLOW   asm volatile (".byte 0xEB, 0x00");

typedef enum {  INVAL_GROUP = -1,
                OTHER_GROUP,
                JCC_GROUP,
                JMP_GROUP,
                CALL_GROUP } X86_INSTR_GROUP;

typedef enum {  INVAL = -1,
                OTHER,
                JMP,
                JA,
                JAE,
                JB,
                JBE,
                JC,
                JCXZ,
                JECXZ,
                JRCXZ,
                JE,
                JG,
                JGE,
                JL,
                JLE,
                JNA,
                JNAE,
                JNB,
                JNBE,
                JNC,
                JNE,
                JNG,
                JNGE,
                JNL,
                JNLE,
                JNO,
                JNP,
                JNS,
                JNZ,
                JO,
                JP,
                JPE,
                JPO,
                JS,
                JZ,
                CALL,
                TEST    } X86_INSTR_TYPE;

/* ret commands */
extern const uint8_t RET_FAR[];
#define SIZE_RET_FAR        1
extern const uint8_t RET_NEAR[];
#define SIZE_RET_NEAR       1

/* push commands */
/* push 8bit to stack */
extern const uint8_t PUSH_BYTE[];
#define SIZE_PUSH_BYTE      2
/* push 64-bits register */
extern const uint8_t PUSH_RAX[];
#define SIZE_PUSH_RAX       1
extern const uint8_t PUSH_RSI[];
#define SIZE_PUSH_RSI       1
extern const uint8_t PUSH_RDI[];
#define SIZE_PUSH_RDI       1

/* pop commands */
/* rip-relative pop */
/* pop QWORD PTR [rip + disp32] */
extern const uint8_t POP_Q_RIPREL[];
#define SIZE_POP_Q_RIPREL   6
/* pop 64-bit register */
extern const uint8_t POP_RAX[];
#define SIZE_POP_RAX        1
extern const uint8_t POP_RSI[];
#define SIZE_POP_RSI        1
extern const uint8_t POP_RDI[];
#define SIZE_POP_RDI        1

/* mov commands */
/* mov imm64 to register */
extern const uint8_t MOVABS_RAX_0[];
#define SIZE_MOVABS_RAX_0   10
extern const uint8_t MOVABS_RSI_0[];
#define SIZE_MOVABS_RSI_0   10
extern const uint8_t MOVABS_RDI_0[];
#define SIZE_MOVABS_RDI_0   10

/* logic operations */
extern const uint8_t TEST_ECX_ECX[];
#define SIZE_TEST_ECX_ECX   2
extern const uint8_t TEST_RCX_RCX[];
#define SIZE_TEST_RCX_RCX   3

/* cmp */
/* rip-relative cmp */
/* cmp BYTE PTR [rip + disp32], byte */
extern const uint8_t CMP_B_RIPREL[];
#define SIZE_CMP_B_RIPREL   7

/* jump */
/* RIP-relative jmp */
extern const uint8_t JMP_Q_RIPREL[];
#define SIZE_JMP_Q_RIPREL   6
/* jump reg */
extern const uint8_t JMP_RAX[];
#define SIZE_JMP_RAX        2

/* 8-bits branches */
#define SIZE_JCC_REL8       2
extern const uint8_t JMP_REL8[];
#define SIZE_JMP_REL8       2
extern const uint8_t JA_REL8[];
#define SIZE_JA_REL8        2
extern const uint8_t JAE_REL8[];
#define SIZE_JAE_REL8       2
extern const uint8_t JB_REL8[];
#define SIZE_JB_REL8        2
extern const uint8_t JBE_REL8[];
#define SIZE_JBE_REL8       2
extern const uint8_t JC_REL8[];
#define SIZE_JC_REL8        2
extern const uint8_t JCXZ_REL8[];
#define SIZE_JCXZ_REL8      2
extern const uint8_t JECXZ_REL8[];
#define SIZE_JECXZ_REL8     2
extern const uint8_t JRCXZ_REL8[];
#define SIZE_JRCXZ_REL8     2
extern const uint8_t JE_REL8[];
#define SIZE_JE_REL8        2
extern const uint8_t JG_REL8[];
#define SIZE_JG_REL8        2
extern const uint8_t JGE_REL8[];
#define SIZE_JGE_REL8       2
extern const uint8_t JL_REL8[];
#define SIZE_JL_REL8        2
extern const uint8_t JLE_REL8[];
#define SIZE_JLE_REL8       2
extern const uint8_t JNA_REL8[];
#define SIZE_JNA_REL8       2
extern const uint8_t JNAE_REL8[];
#define SIZE_JNAE_REL8      2
extern const uint8_t JNB_REL8[];
#define SIZE_JNB_REL8       2
extern const uint8_t JNBE_REL8[];
#define SIZE_JNBE_REL8      2
extern const uint8_t JNC_REL8[];
#define SIZE_JNC_REL8       2
extern const uint8_t JNE_REL8[];
#define SIZE_JNE_REL8       2
extern const uint8_t JNG_REL8[];
#define SIZE_JNG_REL8       2
extern const uint8_t JNGE_REL8[];
#define SIZE_JNGE_REL8      2
extern const uint8_t JNL_REL8[];
#define SIZE_JNL_REL8       2
extern const uint8_t JNLE_REL8[];
#define SIZE_JNLE_REL8      2
extern const uint8_t JNO_REL8[];
#define SIZE_JNO_REL8       2
extern const uint8_t JNP_REL8[];
#define SIZE_JNP_REL8       2
extern const uint8_t JNS_REL8[];
#define SIZE_JNS_REL8       2
extern const uint8_t JNZ_REL8[];
#define SIZE_JNZ_REL8       2
extern const uint8_t JO_REL8[];
#define SIZE_JO_REL8        2
extern const uint8_t JP_REL8[];
#define SIZE_JP_REL8        2
extern const uint8_t JPE_REL8[];
#define SIZE_JPE_REL8       2
extern const uint8_t JPO_REL8[];
#define SIZE_JPO_REL8       2
extern const uint8_t JS_REL8[];
#define SIZE_JS_REL8        2
extern const uint8_t JZ_REL8[];
#define SIZE_JZ_REL8        2

/* 32-bits branches */
extern const uint8_t JMP_REL32[];
#define SIZE_JMP_REL32      5
/* 32-bits conditional branches */
#define SIZE_JCC_REL32      6
extern const uint8_t JA_REL32[];
#define SIZE_JA_REL32       6
extern const uint8_t JAE_REL32[];
#define SIZE_JAE_REL32      6
extern const uint8_t JB_REL32[];
#define SIZE_JB_REL32       6
extern const uint8_t JBE_REL32[];
#define SIZE_JBE_REL32      6
extern const uint8_t JC_REL32[];
#define SIZE_JC_REL32       6
extern const uint8_t JCXZ_REL32[];
#define SIZE_JCXZ_REL32     6
extern const uint8_t JECXZ_REL32[];
#define SIZE_JECXZ_REL32    6
extern const uint8_t JRCXZ_REL32[];
#define SIZE_JRCXZ_REL32    6
extern const uint8_t JE_REL32[];
#define SIZE_JE_REL32       6
extern const uint8_t JG_REL32[];
#define SIZE_JG_REL32       6
extern const uint8_t JGE_REL32[];
#define SIZE_JGE_REL32      6
extern const uint8_t JL_REL32[];
#define SIZE_JL_REL32       6
extern const uint8_t JLE_REL32[];
#define SIZE_JLE_REL32      6
extern const uint8_t JNA_REL32[];
#define SIZE_JNA_REL32      6
extern const uint8_t JNAE_REL32[];
#define SIZE_JNAE_REL32     6
extern const uint8_t JNB_REL32[];
#define SIZE_JNB_REL32      6
extern const uint8_t JNBE_REL32[];
#define SIZE_JNBE_REL32     6
extern const uint8_t JNC_REL32[];
#define SIZE_JNC_REL32      6
extern const uint8_t JNE_REL32[];
#define SIZE_JNE_REL32      6
extern const uint8_t JNG_REL32[];
#define SIZE_JNG_REL32      6
extern const uint8_t JNGE_REL32[];
#define SIZE_JNGE_REL32     6
extern const uint8_t JNL_REL32[];
#define SIZE_JNL_REL32      6
extern const uint8_t JNLE_REL32[];
#define SIZE_JNLE_REL32     6
extern const uint8_t JNO_REL32[];
#define SIZE_JNO_REL32      6
extern const uint8_t JNP_REL32[];
#define SIZE_JNP_REL32      6
extern const uint8_t JNS_REL32[];
#define SIZE_JNS_REL32      6
extern const uint8_t JNZ_REL32[];
#define SIZE_JNZ_REL32      6
extern const uint8_t JO_REL32[];
#define SIZE_JO_REL32       6
extern const uint8_t JP_REL32[];
#define SIZE_JP_REL32       6
extern const uint8_t JPE_REL32[];
#define SIZE_JPE_REL32      6
extern const uint8_t JPO_REL32[];
#define SIZE_JPO_REL32      6
extern const uint8_t JS_REL32[];
#define SIZE_JS_REL32       6
extern const uint8_t JZ_REL32[];
#define SIZE_JZ_REL32       6

/* calls */
/* calls reg */
extern const uint8_t CALL_RAX[];
#define SIZE_CALL_RAX       2
/* relative calls */
extern const uint8_t CALL_REL32[];
#define SIZE_CALL_REL32     5
/* RIP-relative calls */
extern const uint8_t CALL_Q_RIPREL[];
#define SIZE_CALL_Q_RIPREL  6

/* multibyte NOP instruction */
extern const uint8_t NOP_1_BYTES[];
#define SIZE_NOP_1_BYTES    1
extern const uint8_t NOP_2_BYTES[];
#define SIZE_NOP_2_BYTES    2
extern const uint8_t NOP_3_BYTES[];
#define SIZE_NOP_3_BYTES    3
extern const uint8_t NOP_4_BYTES[];
#define SIZE_NOP_4_BYTES    4
extern const uint8_t NOP_5_BYTES[];
#define SIZE_NOP_5_BYTES    5
extern const uint8_t NOP_6_BYTES[];
#define SIZE_NOP_6_BYTES    6
extern const uint8_t NOP_7_BYTES[];
#define SIZE_NOP_7_BYTES    7
extern const uint8_t NOP_8_BYTES[];
#define SIZE_NOP_8_BYTES    8
extern const uint8_t NOP_9_BYTES[];
#define SIZE_NOP_9_BYTES    9

/*
 * Description:
 *  Function writes @num NOP-instructions to @addr.
 * Input:
 *  @addr - address where instructions will be.
 *  @num - how many NOPs should be writed.
 */
void write_nops(uint8_t *addr, size_t num);


/*
 * Description:
 *  Function writes NOP-instructions to @addr with common length is 16 bytes.
 * Input:
 *  @addr - address where instructions will be.
 */
void write_16_bytes_nops(void *addr);


/*
 * Description:
 *  Function puts pop QWORD PTR [rip + disp32] ar @dest addr as instruction has
 *  RIP = @ip and @pop_addr - target RIP.
 * Input:
 *  @dest - addr where instruction will be.
 *  @ip - instruction pointer (RIP).
 *  @pop_addr - RIP of target memory address.
 * Output:
 *  Pointer to the byte exactly after a placed instruction.
 */
uint8_t *put_pop_q_riprel(uint8_t *dest, uint64_t ip, uint64_t pop_addr);


/*
 * Description:
 *  Function puts cmp BYTE PTR [rip + disp32], byte ar @dest addr as instruction
 *  has RIP = @ip and @mem_addr - RIP of memory where is placed the first
 *  operand.
 * Input:
 *  @dest - addr where instruction will be.
 *  @ip - instruction pointer (RIP).
 *  @mem_addr - RIP of memory where is placed the first operand.
 *  @byte - second operand - immediate.
 * Output:
 *  Pointer to the byte exactly after a placed instruction.
 */
uint8_t *put_cmp_b_riprel(uint8_t *dest, uint64_t ip, uint64_t mem_addr, int8_t byte);


/*
 * Description:
 *  Function puts jmp rel8 or jcc rel8 to @dest addr as instruction has
 *  RIP = @ip and @jmp_target - target RIP.
 * Input:
 *  @dest - addr where instruction will be.
 *  @ip - instruction pointer (RIP).
 *  @jmp_target - RIP of target instruction.
 *  @type - type of instruction which should be placed to @dest addr.
 * Output:
 *  Success:
 *      Pointer to the byte exactly after a placed instruction.
 *  Fail:
 *      NULL pointer.
 */
uint8_t *put_jmp_rel8(uint8_t *dest, uint64_t ip, uint64_t jmp_addr, X86_INSTR_TYPE type);


/*
 * Description:
 *  Function puts jmp rel32 as instruction has RIP = @ip and
 *  @jmp_target - target RIP.
 * Input:
 *  @dest - addr where instruction will be.
 *  @ip - instruction pointer (RIP).
 *  @jmp_target - RIP of target instruction.
 * Output:
 *  Success:
 *      Pointer to the byte exactly after a placed instruction.
 *  Fail:
 *      NULL pointer.
 */
uint8_t *put_jmp_rel32(uint8_t *dest, uint64_t ip, uint64_t jmp_addr);


/*
 * Description:
 *  Function puts jcc rel32 to @dest addr as instruction has
 *  RIP = @ip and @jmp_target - target RIP.
 * Input:
 *  @dest - addr where instruction will be.
 *  @ip - instruction pointer (RIP).
 *  @jmp_target - RIP of target instruction.
 *  @type - type of instruction which should be placed to @dest addr.
 * Output:
 *  Success:
 *      Pointer to the byte exactly after a placed instruction.
 *  Fail:
 *      NULL pointer.
 */
uint8_t *put_jcc_rel32(uint8_t *dest, size_t ip, size_t jmp_addr, X86_INSTR_TYPE type);


/*
 * Description:
 *  Function puts call rel32 as instruction has RIP = @ip and
 *  @jmp_target - target RIP.
 * Input:
 *  @dest - addr where instruction will be.
 *  @ip - instruction pointer (RIP).
 *  @jmp_target - RIP of target instruction.
 * Output:
 *  Success:
 *      Pointer to the byte exactly after a placed instruction.
 *  Fail:
 *      NULL pointer.
 */
uint8_t *put_call_rel32(uint8_t *dest, size_t ip, size_t call_addr);

#endif /* _X86_64_H */


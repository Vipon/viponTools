/***
 * MIT License
 *
 * Copyright (c) 2020 Konychev Valera
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
#include "comdef.h"
#include "x86_64.h"
#include "x86_64_DisassemblerWrap.h"

/* disassembler */
#include <capstone/capstone.h>

#include <stdint.h>

/* ret commands */
const uint8_t RET_FAR[]         = { 0xCB };
const uint8_t RET_NEAR[]        = { 0xC3 };

/* push commands */
/* push 8bit to stack */
const uint8_t PUSH_BYTE[]       = { 0x6A, 0x00 };
/* push 64-bit to register */
const uint8_t PUSH_RAX[]        = { 0x50 };
const uint8_t PUSH_RSI[]        = { 0x56 };
const uint8_t PUSH_RDI[]        = { 0x57 };

/* rip-relative pop */
/* pop QWORD PTR [rip + disp32] */
const uint8_t POP_Q_RIPREL[]    = { 0x8F, 0x05, 0x00, 0x00, 0x00, 0x00 };
/* pop 64-bit register */
const uint8_t POP_RAX[]         = { 0x58 };
const uint8_t POP_RSI[]         = { 0x5E };
const uint8_t POP_RDI[]         = { 0x5F };

/* mov commands */
/* mov imm64 to register */
const uint8_t MOVABS_RAX_0[]    = { 0x48, 0xB8, 0x00, 0x00, 0x00, 0x00, 0x00,
                                    0x00, 0x00, 0x00 };
const uint8_t MOVABS_RSI_0[]    = { 0x48, 0xBE, 0x00, 0x00, 0x00, 0x00, 0x00,
                                    0x00, 0x00, 0x00 };
const uint8_t MOVABS_RDI_0[]    = { 0x48, 0xBF, 0x00, 0x00, 0x00, 0x00, 0x00,
                                    0x00, 0x00, 0x00 };

/* logic operations */
const uint8_t TEST_ECX_ECX[]    = { 0x85, 0xC9 };
const uint8_t TEST_RCX_RCX[]    = { 0x48, 0x85, 0xC9 };

/* cmp */
/* rip-relative cmp */
/* cmp BYTE PTR [rip + disp32], byte */
//                                  |Op| |R/M|  |       disp32        | imm8 |
const uint8_t CMP_B_RIPREL[]    = { 0x80, 0x3D, 0xFF, 0xFF, 0xFF, 0xFF, 0x00 };

/* jump */
/* RIP-relative jmp */
const uint8_t JMP_Q_RIPREL[]    = { 0xFF, 0x25, 0x00, 0x00, 0x00, 0x00 };
/* jump reg */
const uint8_t JMP_RAX[]         = { 0xFF, 0xE0 };

/* 8-bits branches */
const uint8_t JMP_REL8[]        = { 0xEB, 0x00};
const uint8_t JA_REL8[]         = { 0x77, 0x00};
const uint8_t JAE_REL8[]        = { 0x73, 0x00};
const uint8_t JB_REL8[]         = { 0x72, 0x00};
const uint8_t JBE_REL8[]        = { 0x76, 0x00};
const uint8_t JC_REL8[]         = { 0x72, 0x00};
const uint8_t JCXZ_REL8[]       = { 0xE3, 0x00};
const uint8_t JECXZ_REL8[]      = { 0xE3, 0x00};
const uint8_t JRCXZ_REL8[]      = { 0xE3, 0x00};
const uint8_t JE_REL8[]         = { 0x74, 0x00};
const uint8_t JG_REL8[]         = { 0x7F, 0x00};
const uint8_t JGE_REL8[]        = { 0x7D, 0x00};
const uint8_t JL_REL8[]         = { 0x7C, 0x00};
const uint8_t JLE_REL8[]        = { 0x7E, 0x00};
const uint8_t JNA_REL8[]        = { 0x76, 0x00};
const uint8_t JNAE_REL8[]       = { 0x72, 0x00};
const uint8_t JNB_REL8[]        = { 0x73, 0x00};
const uint8_t JNBE_REL8[]       = { 0x77, 0x00};
const uint8_t JNC_REL8[]        = { 0x73, 0x00};
const uint8_t JNE_REL8[]        = { 0x75, 0x00};
const uint8_t JNG_REL8[]        = { 0x7E, 0x00};
const uint8_t JNGE_REL8[]       = { 0x7C, 0x00};
const uint8_t JNL_REL8[]        = { 0x7D, 0x00};
const uint8_t JNLE_REL8[]       = { 0x7F, 0x00};
const uint8_t JNO_REL8[]        = { 0x71, 0x00};
const uint8_t JNP_REL8[]        = { 0x7B, 0x00};
const uint8_t JNS_REL8[]        = { 0x79, 0x00};
const uint8_t JNZ_REL8[]        = { 0x75, 0x00};
const uint8_t JO_REL8[]         = { 0x70, 0x00};
const uint8_t JP_REL8[]         = { 0x7A, 0x00};
const uint8_t JPE_REL8[]        = { 0x7A, 0x00};
const uint8_t JPO_REL8[]        = { 0x7B, 0x00};
const uint8_t JS_REL8[]         = { 0x78, 0x00};
const uint8_t JZ_REL8[]         = { 0x74, 0x00};

/* 32-bits branches */
const uint8_t JMP_REL32[]       = { 0xE9, 0x00, 0x00, 0x00, 0x00 };
/* 32-bits conditional branches */
const uint8_t JA_REL32[]        = { 0x0F, 0x87, 0x00, 0x00, 0x00, 0x00};
const uint8_t JAE_REL32[]       = { 0x0F, 0x83, 0x00, 0x00, 0x00, 0x00};
const uint8_t JB_REL32[]        = { 0x0F, 0x82, 0x00, 0x00, 0x00, 0x00};
const uint8_t JBE_REL32[]       = { 0x0F, 0x86, 0x00, 0x00, 0x00, 0x00};
const uint8_t JC_REL32[]        = { 0x0F, 0x82, 0x00, 0x00, 0x00, 0x00};
//const uint8_t JCXZ_REL32[]      = { 0x0F, 0x8, 0x00, 0x00, 0x00, 0x00};
//const uint8_t JECXZ_REL32[]     = { 0x0F, 0x8, 0x00, 0x00, 0x00, 0x00};
//const uint8_t JRCXZ_REL32[]     = { 0x0F, 0x8, 0x00, 0x00, 0x00, 0x00};
const uint8_t JE_REL32[]        = { 0x0F, 0x84, 0x00, 0x00, 0x00, 0x00};
const uint8_t JG_REL32[]        = { 0x0F, 0x8F, 0x00, 0x00, 0x00, 0x00};
const uint8_t JGE_REL32[]       = { 0x0F, 0x8D, 0x00, 0x00, 0x00, 0x00};
const uint8_t JL_REL32[]        = { 0x0F, 0x8C, 0x00, 0x00, 0x00, 0x00};
const uint8_t JLE_REL32[]       = { 0x0F, 0x8E, 0x00, 0x00, 0x00, 0x00};
const uint8_t JNA_REL32[]       = { 0x0F, 0x86, 0x00, 0x00, 0x00, 0x00};
const uint8_t JNAE_REL32[]      = { 0x0F, 0x82, 0x00, 0x00, 0x00, 0x00};
const uint8_t JNB_REL32[]       = { 0x0F, 0x83, 0x00, 0x00, 0x00, 0x00};
const uint8_t JNBE_REL32[]      = { 0x0F, 0x87, 0x00, 0x00, 0x00, 0x00};
const uint8_t JNC_REL32[]       = { 0x0F, 0x83, 0x00, 0x00, 0x00, 0x00};
const uint8_t JNE_REL32[]       = { 0x0F, 0x85, 0x00, 0x00, 0x00, 0x00};
const uint8_t JNG_REL32[]       = { 0x0F, 0x8E, 0x00, 0x00, 0x00, 0x00};
const uint8_t JNGE_REL32[]      = { 0x0F, 0x8C, 0x00, 0x00, 0x00, 0x00};
const uint8_t JNL_REL32[]       = { 0x0F, 0x8D, 0x00, 0x00, 0x00, 0x00};
const uint8_t JNLE_REL32[]      = { 0x0F, 0x8F, 0x00, 0x00, 0x00, 0x00};
const uint8_t JNO_REL32[]       = { 0x0F, 0x81, 0x00, 0x00, 0x00, 0x00};
const uint8_t JNP_REL32[]       = { 0x0F, 0x8B, 0x00, 0x00, 0x00, 0x00};
const uint8_t JNS_REL32[]       = { 0x0F, 0x89, 0x00, 0x00, 0x00, 0x00};
const uint8_t JNZ_REL32[]       = { 0x0F, 0x85, 0x00, 0x00, 0x00, 0x00};
const uint8_t JO_REL32[]        = { 0x0F, 0x80, 0x00, 0x00, 0x00, 0x00};
const uint8_t JP_REL32[]        = { 0x0F, 0x8A, 0x00, 0x00, 0x00, 0x00};
const uint8_t JPE_REL32[]       = { 0x0F, 0x8A, 0x00, 0x00, 0x00, 0x00};
const uint8_t JPO_REL32[]       = { 0x0F, 0x8B, 0x00, 0x00, 0x00, 0x00};
const uint8_t JS_REL32[]        = { 0x0F, 0x88, 0x00, 0x00, 0x00, 0x00};
const uint8_t JZ_REL32[]        = { 0x0F, 0x84, 0x00, 0x00, 0x00, 0x00};

/* calls */
/* calls reg */
const uint8_t CALL_RAX[]        = { 0xFF, 0xD0 };
/* relative calls */
const uint8_t CALL_REL32[]      = { 0xE8, 0x00, 0x00, 0x00, 0x00 };
/* RIP-relative calls */
const uint8_t CALL_Q_RIPREL[]   = { 0xFF, 0x15, 0x02, 0x00, 0x00, 0x00 };

/* multibyte NOP instruction */
const uint8_t NOP_1_BYTES[] = { 0x90 };
const uint8_t NOP_2_BYTES[] = { 0x66, 0x90 };
const uint8_t NOP_3_BYTES[] = { 0x0f, 0x1f, 0x00 };
const uint8_t NOP_4_BYTES[] = { 0x0f, 0x1f, 0x40, 0x00 };
const uint8_t NOP_5_BYTES[] = { 0x0f, 0x1f, 0x44, 0x00, 0x00 };
const uint8_t NOP_6_BYTES[] = { 0x66, 0x0f, 0x1f, 0x44, 0x00, 0x00 };
const uint8_t NOP_7_BYTES[] = { 0x0f, 0x1f, 0x80, 0x00, 0x00, 0x00, 0x00 };
const uint8_t NOP_8_BYTES[] = { 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00 };
const uint8_t NOP_9_BYTES[] = { 0x66, 0x0F, 0x1F, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00 };

void write_nops(uint8_t *addr, size_t num)
{
    if (addr == NULL)
        return;

    size_t i = 0;
    for (i = 0; i < num; ++i)
        addr[i] = 0x90;

    return;
}


void write_16_bytes_nops(void *addr)
{
    if (addr == NULL)
        return;

    directCopyBytes((const uint8_t*)&NOP_8_BYTES, (uint8_t*)addr, 8);
    directCopyBytes((const uint8_t*)&NOP_8_BYTES, (uint8_t*)((size_t)addr + 8), 8);

    return;
}


uint8_t *put_pop_q_riprel(uint8_t *dest, uint64_t ip, uint64_t pop_addr)
{
    int32_t disp = pop_addr - ip - SIZE_POP_Q_RIPREL;

    directCopyBytes(POP_Q_RIPREL, dest, 2); /* Opcode + ModR/M */
    directCopyBytes((uint8_t*)&disp, &dest[2], sizeof(int32_t)); /* disp32 */

    return &dest[SIZE_POP_Q_RIPREL];
}


uint8_t *put_cmp_b_riprel(uint8_t *dest, uint64_t ip, uint64_t mem_addr, int8_t byte)
{
    int32_t disp = mem_addr - ip - SIZE_CMP_B_RIPREL;

    directCopyBytes(CMP_B_RIPREL, dest, 2); /* Opcode + ModR/M */
    directCopyBytes((uint8_t*)&disp, &dest[2], sizeof(int32_t)); /* disp32 */
    directCopyBytes((uint8_t*)&byte, &dest[6], sizeof(uint8_t)); /* imm8 */

    return &dest[SIZE_CMP_B_RIPREL];
}


uint8_t *put_jmp_rel8(uint8_t *dest, uint64_t ip, uint64_t jmp_addr, X86_INSTR_TYPE type)
{
    uint8_t disp = jmp_addr - ip - SIZE_JMP_REL8;

    size_t num = 1; // opcode size

    switch (type) {
    case JMP:
        LOG("JUMP");
        directCopyBytes(JMP_REL8, dest, num);
        break;
    case JA:
        LOG("JA");
        directCopyBytes(JA_REL8, dest, num);
        break;
    case JAE:
        LOG("JAE");
        directCopyBytes(JAE_REL8, dest, num);
        break;
    case JB:
        LOG("JB");
        directCopyBytes(JB_REL8, dest, num);
        break;
    case JBE:
        LOG("JBE");
        directCopyBytes(JBE_REL8, dest, num);
        break;
    case JC:
        LOG("JC");
        directCopyBytes(JC_REL8, dest, num);
        break;
    case JCXZ:
        LOG("JCXZ");
        directCopyBytes(JCXZ_REL8, dest, num);
        break;
    case JECXZ:
        LOG("JECXZ");
        directCopyBytes(JECXZ_REL8, dest, num);
        break;
    case JRCXZ:
        LOG("JRCXZ");
        directCopyBytes(JRCXZ_REL8, dest, num);
        break;
    case JE:
        LOG("JE");
        directCopyBytes(JE_REL8, dest, num);
        break;
    case JG:
        LOG("JG");
        directCopyBytes(JG_REL8, dest, num);
        break;
    case JGE:
        LOG("JGE");
        directCopyBytes(JGE_REL8, dest, num);
        break;
    case JL:
        LOG("JL");
        directCopyBytes(JL_REL8, dest, num);
        break;
    case JLE:
        LOG("JLE");
        directCopyBytes(JLE_REL8, dest, num);
        break;
    case JNA:
        LOG("JNA");
        directCopyBytes(JNA_REL8, dest, num);
        break;
    case JNAE:
        LOG("JNAE");
        directCopyBytes(JNAE_REL8, dest, num);
        break;
    case JNB:
        LOG("JNB");
        directCopyBytes(JNB_REL8, dest, num);
        break;
    case JNBE:
        LOG("JNBE");
        directCopyBytes(JNBE_REL8, dest, num);
        break;
    case JNC:
        LOG("JNC");
        directCopyBytes(JNC_REL8, dest, num);
        break;
    case JNE:
        LOG("JNE");
        directCopyBytes(JNE_REL8, dest, num);
        break;
    case JNG:
        LOG("JNG");
        directCopyBytes(JNG_REL8, dest, num);
        break;
    case JNGE:
        LOG("JNGE");
        directCopyBytes(JNGE_REL8, dest, num);
        break;
    case JNL:
        LOG("JNL");
        directCopyBytes(JNL_REL8, dest, num);
        break;
    case JNLE:
        LOG("JNLE");
        directCopyBytes(JNLE_REL8, dest, num);
        break;
    case JNO:
        LOG("JNO");
        directCopyBytes(JNO_REL8, dest, num);
        break;
    case JNP:
        LOG("JNP");
        directCopyBytes(JNP_REL8, dest, num);
        break;
    case JNS:
        LOG("JNS");
        directCopyBytes(JNS_REL8, dest, num);
        break;
    case JNZ:
        LOG("JNZ");
        directCopyBytes(JNZ_REL8, dest, num);
        break;
    case JO:
        LOG("JO");
        directCopyBytes(JO_REL8, dest, num);
        break;
    case JP:
        LOG("JP");
        directCopyBytes(JP_REL8, dest, num);
        break;
    case JPE:
        LOG("JPE");
        directCopyBytes(JPE_REL8, dest, num);
        break;
    case JPO:
        LOG("JPO");
        directCopyBytes(JPO_REL8, dest, num);
        break;
    case JS:
        LOG("JS");
        directCopyBytes(JS_REL8, dest, num);
        break;
    case JZ:
        LOG("JZ");
        directCopyBytes(JZ_REL8, dest, num);
        break;
    default:
        ERROR("UNKNOWN COMMAND.");
        return NULL;
    }

    directCopyBytes(&disp, &dest[num], sizeof(uint8_t));

    return &dest[SIZE_JMP_REL8];
}


uint8_t *put_jmp_rel32(uint8_t *dest, uint64_t ip, uint64_t jmp_addr)
{
    uint32_t disp = jmp_addr - ip - sizeof(JMP_REL32);

    size_t num = 1; // opcode size

    directCopyBytes(JMP_REL32, dest, num);
    directCopyBytes((uint8_t*)(&disp), &dest[num], sizeof(uint32_t));

    return &dest[sizeof(JMP_REL32)];
}


uint8_t *put_jcc_rel32(uint8_t *dest, size_t ip, size_t jmp_addr, X86_INSTR_TYPE type)
{
    uint32_t disp = jmp_addr - ip - SIZE_JCC_REL32;

    size_t num = 2; // opcode size

    switch (type) {
    case JA:
        LOG("JA");
        directCopyBytes(JA_REL32, dest, num);
        break;
    case JAE:
        LOG("JAE");
        directCopyBytes(JAE_REL32, dest, num);
        break;
    case JB:
        LOG("JB");
        directCopyBytes(JB_REL32, dest, num);
        break;
    case JBE:
        LOG("JBE");
        directCopyBytes(JBE_REL32, dest, num);
        break;
    case JE:
        LOG("JE");
        directCopyBytes(JE_REL32, dest, num);
        break;
    case JG:
        LOG("JG");
        directCopyBytes(JG_REL32, dest, num);
        break;
    case JGE:
        LOG("JGE");
        directCopyBytes(JGE_REL32, dest, num);
        break;
    case JL:
        LOG("JL");
        directCopyBytes(JL_REL32, dest, num);
        break;
    case JLE:
        LOG("JLE");
        directCopyBytes(JLE_REL32, dest, num);
        break;
    case JNE:
        LOG("JNE");
        directCopyBytes(JNE_REL32, dest, num);
        break;
    case JNO:
        LOG("JNO");
        directCopyBytes(JNO_REL32, dest, num);
        break;
    case JNP:
        LOG("JNP");
        directCopyBytes(JNP_REL32, dest, num);
        break;
    case JNS:
        LOG("JNS");
        directCopyBytes(JNS_REL32, dest, num);
        break;
    case JO:
        LOG("JO");
        directCopyBytes(JO_REL32, dest, num);
        break;
    case JP:
        LOG("JP");
        directCopyBytes(JP_REL32, dest, num);
        break;
    case JS:
        LOG("JS");
        directCopyBytes(JS_REL32, dest, num);
        break;
    default:
        ERROR("UNKNOWN COMMAND.");
        return NULL;
    }

    directCopyBytes((uint8_t*)(&disp), &dest[num], sizeof(uint32_t));

    return &dest[SIZE_JCC_REL32];
}


uint8_t *put_call_rel32(uint8_t *dest, size_t ip, size_t call_addr)
{
    uint32_t disp = call_addr - ip - sizeof(CALL_REL32);

    size_t num = 1; // opcode size

    directCopyBytes(CALL_REL32, dest, num);
    directCopyBytes((uint8_t*)(&disp), &dest[num], sizeof(uint32_t));

    return &dest[sizeof(CALL_REL32)];
}


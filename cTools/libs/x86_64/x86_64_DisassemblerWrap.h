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

#ifndef _X86_64_DISASSEMBLERWRAP_H
#define _X86_64_DISASSEMBLERWRAP_H

#include "x86_64.h"

#ifdef CAPSTONE_DISASSEMBLER
    /* disassembler */
    #include <capstone/capstone.h>
    #define MAX_MNEMONIC_SIZE   193

    typedef cs_insn x86_64_instr;

    typedef enum {
        ERR_DISASM_WRAP_OK = 0,
        ERR_DISASM_WRAP_BAD_ARG = (int64_t)0xFFFFFFFFFFFFFFF0, /* (int64_t) -16 */
        ERR_DISASM_WRAP_INIT,
        ERR_DISASM_WRAP_CHANGE_OPT,
        ERR_DISASM_WRAP_DISASM_PROCESS,
        ERR_DISASM_WRAP_ALLOC,
        ERR_DISASM_WRAP_DETECT_JMP_TARGET,
        ERR_DISASM_WRAP_UNKNOWN
    } ERR_DISASM_WRAP;

#else
    #error "*** ERROR: Unknown disassembler. ***"
#endif /* CAPSTONE_DISASSEMBLER */

#ifdef __STDC__
    #if __STDC__ == 1
        #include <stdio.h>
        #include <stdint.h>
    #endif /* __STDC__ == 1 */
#else
    #error "*** ERROR: Need standard C library or equivalent. ***"
#endif /* __STDC__ */


/*
 * Description:
 *  Function inits global structure disasm.
 * Output:
 *  Success:
 *      ERR_DISASM_WRAP_OK
 *  Fail:
 *      Other ERR_DISASM_WRAP_*. To understanding sence of error call function
 *      get_disasm_error_string().
 */
ERR_DISASM_WRAP init_x86_64_disassembler(void);
void stop_x86_64_disassembler(void);


/*
 * Description:
 *  Function inits instruction descriptor.
 * Input:
 *  @insn - pointer to instruction descriptor should be initialized.
 * Output:
 *  Success:
 *      ERR_DISASM_WRAP_OK.
 *  Fail:
 *      Other ERR_DISASM_WRAP_*. To understanding sence of error call function
 *      get_disasm_error_string().
 */
ERR_DISASM_WRAP init_instr(x86_64_instr **insn);
void free_instr(x86_64_instr **insn);

/*
 * Description:
 *  Function disasm instruction on @addr with instruction pointer @ip.
 * Input:
 *  @insn - pointer to instruction descriptor.
 *  @addr - place where instruction is.
 *  @ip - istruction pointer.
 * Output:
 *  Success:
 *      Size of readed instruction.
 *  Fail:
 *      Other ERR_DISASM_WRAP_*. To understanding sence of error call function
 *      get_disasm_error_string().
 */
int get_instr(x86_64_instr *insn, const uint8_t *addr, uint64_t ip);


/*
 * Description:
 *  Function returns string with description of errors.
 * Input:
 *  @err - number of error.
 * Output:
 *  Description of errors.
 */
const char *get_disasmwrap_error_string(ERR_DISASM_WRAP err);


#ifdef __STDC__
    #if __STDC__ == 1
/*
 * Description:
 *  Function inits global structure disasm.
 * Input:
 *  @insn - pointer to descriptor of instruction should be printed.
 */
void print_x86_instr(FILE *f, const x86_64_instr *insn);


/*
 * Description:
 *  Function puts mnemonic of instruction in the @com array of chars. Size of
 *  array should equal MAX_MNEMONIC_SIZE.
 * Input:
 *  @insn - pointer to instruction descriptor.
 *  @com - pointer to array of chars.
 */
void get_instr_mnemonic(char *com, const x86_64_instr *insn);
    #endif /* __STDC__ == 1 */
#endif /* __STDC__ */


/*
 * Description:
 *  Function returns pointer to array contains encoding of instruction.
 * Input:
 *  @insn - pointer to instruction descriptor.
 * Output:
 *  Pointer to array contains encoding of instruction.
 */
const uint8_t *get_instr_encoding(const x86_64_instr *insn);


/*
 * Description:
 *  Function returns size of instruction in bytes.
 * Input:
 *  @insn - pointer to instruction descriptor.
 * Output:
 *  Size of instruction in bytes.
 */
uint8_t get_instr_size(const x86_64_instr *insn);


/*
 * Description:
 *  Function returns size of different prefixes.
 * Input:
 *  @insn - pointer to instruction descriptor.
 * Output:
 *  Size of certain prefix of instruction in bytes.
 */
uint8_t get_legacy_prefix_size(const x86_64_instr *insn);
uint8_t get_vex_prefix_size(const x86_64_instr *insn);
uint8_t get_rex_prefix_size(const x86_64_instr *insn);


/*
 * Description:
 *  Function returns size of all prefixes.
 * Input:
 *  @insn - pointer to instruction descriptor.
 * Output:
 *  Size of prefixes-field of instruction in bytes.
 */
uint8_t get_prefixes_size(const x86_64_instr *insn);


/*
 * Description:
 *  Function returns size of opcode.
 * Input:
 *  @insn - pointer to instruction descriptor.
 * Output:
 *  Size of opcode-field of instruction in bytes.
 */
uint8_t get_opcode_size(const x86_64_instr *insn);


/*
 * Description:
 *  Function returns size of ModRM byte.
 * Input:
 *  @insn - pointer to instruction descriptor.
 * Output:
 *  Size of modrm-field of instruction in bytes.
 */
uint8_t get_modrm_size(const x86_64_instr *insn);


/*
 * Description:
 *  Function returns size of SIB byte.
 * Input:
 *  @insn - pointer to instruction descriptor.
 * Output:
 *  Size of sib-field of instruction in bytes.
 */
uint8_t get_sib_size(const x86_64_instr *insn);


/*
 * Description:
 *  Function returns size of Displacement.
 * Input:
 *  @insn - pointer to instruction descriptor.
 * Output:
 *  Size of disp-field of instruction in bytes.
 */
uint8_t get_disp_size(const x86_64_instr *insn);


/*
 * Description:
 *  Function returns size of Immediate.
 * Input:
 *  @insn - pointer to instruction descriptor.
 * Output:
 *  Size of imm-field of instruction in bytes.
 */
uint8_t get_imm_size(const x86_64_instr *insn);


/*
 * Description:
 *  Function returns legacy prefixes. The highest bytes will be zero if they
 *  irrelevant.
 * Input:
 *  @insn - pointer to instruction descriptor.
 * Output:
 *  legacy_prefixes
 */
uint32_t get_legacy_prefixes(const x86_64_instr *insn);


/*
 * Description:
 *  Function returns legacy prefixes. The highest bytes will be zero if they
 *  irrelevant.
 * Input:
 *  @insn - pointer to instruction descriptor.
 * Output:
 *  vex_prefix
 */
uint32_t get_vex_prefix(const x86_64_instr *insn);


/*
 * Description:
 *  Function returns rex-prefix.
 * Input:
 *  @insn - pointer to instruction descriptor.
 * Output:
 *  rex_prefix
 */
uint8_t get_rex_prefix(const x86_64_instr *insn);


/*
 * Description:
 *  Function returns opcode. The highest bytes will be zero if they irrelevant.
 * Input:
 *  @insn - pointer to instruction descriptor.
 * Output:
 *  opcode
 */
uint32_t get_opcode(const x86_64_instr *insn);


/*
 * Description:
 *  Function returns modrm-byte.
 * Input:
 *  @insn - pointer to instruction descriptor.
 * Output:
 *  modrm
 */
uint8_t get_modrm(const x86_64_instr *insn);


/*
 * Description:
 *  Function returns sib-byte.
 * Input:
 *  @insn - pointer to instruction descriptor.
 * Output:
 *  sib
 */
uint8_t get_sib(const x86_64_instr *insn);


/*
 * Description:
 *  Function returns displacement. The highest bytes will be zero if they
 *  irrelevant.
 * Input:
 *  @insn - pointer to instruction descriptor.
 * Output:
 *  disp
 */
int32_t get_disp(const x86_64_instr *insn);


/*
 * Description:
 *  Function returns immediate. The highest bytes will be zero if they
 *  irrelevant.
 * Input:
 *  @insn - pointer to instruction descriptor.
 * Output:
 *  immediate
 */
int64_t get_imm(const x86_64_instr *insn);


/*
 * Description:
 *  Function returns instruction pointer.
 * Input:
 *  @insn - pointer to instruction descriptor.
 * Output:
 *  Instruction pointer
 */
uint64_t get_ip(const x86_64_instr *insn);


/*
 * Description:
 *  Function counts how many instruction on addr @src in @src_bytes.
 * Input:
 *  @src - memory addr where instruction are placed.
 *  @src_bytes - size of code in bytes.
 *  @ip - istruction pointer.
 * Output:
 *  Success:
 *      Size of readed instruction.
 *  Fail:
 *      Other ERR_DISASM_WRAP_*. To understanding sence of error call function
 *      get_disasm_error_string().
 */
int64_t get_instr_num(const uint8_t *src, uint64_t src_size, uint64_t ip);


/*
 * Description:
 *  Sometimes need to modify code sequence. Size of result code is known, but
 *  size of old code sequence isn't, so function checks all intructions are in
 *  virtual memory from @src to @src + @src_size and count all their length
 *  include part out of @src to @src + @src_size.
 *  Returns real size of code sequence.
 * Input:
 *  @src - memory addr where instruction are placed.
 *  @src_bytes - size of code in bytes.
 *  @ip - istruction pointer.
 * Output:
 *  Success:
 *      Real size of code sequence.
 *  Fail:
 *      Other ERR_DISASM_WRAP_*. To understanding sence of error call function
 *      get_disasm_error_string().
 */
int64_t get_code_size(const uint8_t *src, uint64_t src_size);


/*
 * Description:
 *  Function returns @jmp_target - instruction pointer of target of branch or
 *  call instructions.
 * Input:
 *  @insn - pointer to instruction descriptor.
 *  @src - pointer to position of intruction in memory.
 *  @ip - value of instruction pointer.
 *  @jmp_target - pointer to place where result will be saved.
 * Output:
 *  Success:
 *      ERR_DISASM_WRAP_OK
 *  Fail:
 *      ERR_DISASM_WRAP_DETECT_JMP_TARGET - if cannot calculate jmp_target.
 *      ERR_DISASM_WRAP_UNKNOWN - if bad instruction descriptor.
 *      ERR_DISASM_WRAP_BAD_ARG - if bad arguments.
 */
ERR_DISASM_WRAP get_jmp_target( x86_64_instr *insn,
                                const uint8_t *src,
                                size_t ip,
                                uint64_t *jmp_target    );


/*
 * Description:
 *  Function returns addressing type of x86_64 instruction.
 * Input:
 *  @pInsn - pointer to instruction descriptor.
 * Output:
 *  Addressing type, see x86_64.h
 */
X86_ADDR_TYPE get_addr_type(const x86_64_instr *pInsn);


/*
 * Descriprion:
 *  Function will update RIP-relative x86_64 instruction and return encoding of
 *  new asm command in new_com. Size of new_com array should equal
 *  X86_MAX_INSTR_LEN.
 * Input:
 *  @new_com - pointer to array of bytes, where new command will be saved.
 *  @pInsn - pointer to descriptor of instruction  wich is needed to update.
 *  @offs - offset from old position to new.
 * Output:
 *  Size of @new_com
 */
uint8_t update_riprel_insn(uint8_t *new_com, const x86_64_instr *pInsn, int32_t offs);


/*
 * Descriprion:
 *  Function checks instruction is jump of not.
 * Input:
 *  @pInsn - pointer to descriptor of instruction  wich is needed to check.
 * Output:
 *  true or false
 */
int is_instr_jump(const x86_64_instr *pInsn);


/*
 * Description:
 *  Function return identificator of instruction type. See X86_INSTR_TYPE.
 * Input:
 *  @pInsn - pointer to instruction descriptor.
 * Output:
 *  X86_INSTR_TYPE
 */
X86_INSTR_TYPE get_instr_type(const x86_64_instr *pInsn);


/*
 * Description:
 *  Function return identificator of instruction group. See X86_INSTR_GROUP.
 * Input:
 *  @insn - pointer to instruction descriptor.
 * Output:
 *  X86_INSTR_GROUP
 */
X86_INSTR_GROUP get_instr_group(const x86_64_instr *pInsn);

/*
 * Description:
 *  Function puts jcc rel32 instruction a @dest addr, as instruction point of
 *  jump is @ip and jmp_target is @jmp_point. Type of conditional jmp function
 *  will take from instruction descriptor @insn.
 * Input:
 *  @insn - pointer to instruction descriptor.
 *  @dest - address where instruction shoul be.
 *  @ip - instruction pointer of instruction.
 *  @jmp_point - jmp_target.
 * Output:
 *  Success:
 *      Addr of the byte is exactly after jcc rel32 instruction.
 *  Fail:
 *      Null - if it's not jcc or it's unknown intruction.
 */
uint8_t *put_jcc_32(const x86_64_instr *insn,
                    uint8_t *dest,
                    uint64_t ip,
                    uint64_t jmp_point);


/*
 * Input:
 *  @start - start virtual addr to check.
 *  @size - size of memory area which starts at @start addr and should be checked.
 *  @addr - addr of instruction in which should be checked.
 *  @ip - real instruction pointer of this instruction.
 * Output:
 *  Success:
 *      1 - if istruction is jmp_target.
 *      0 - if it isn't.
 *  Fail:
 *      ERR_DISASM_WRAP_*
 */
int64_t is_instr_jmp_target(const uint8_t  *start,
                            uint64_t size,
                            const uint8_t  *addr,
                            uint64_t ip );

#endif /* _X86_64_DISASSEMBLERWRAP_H */


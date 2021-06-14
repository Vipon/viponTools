#include "bits.h"
#include "comdef.h"
#include "x86_64.h"
#include "x86_64_DisassemblerWrap.h"

#include <capstone/capstone.h>

#ifdef __STDC__
    #if __STDC__ == 1
        #include <string.h>
    #else
        #error "*** ERROR: Need standard C library or equivalent. ***"
    #endif /* __STDC__ == 1 */
#endif /* __STDC__ */

typedef csh x86_64_disasm;

static uint8_t init = 0;
x86_64_disasm  disasm;


ERR_DISASM_WRAP init_x86_64_disassembler()
{
    LOG("start init_x86_64_disassembler\n");
    if (init) {
        LOG("disassembler is already initialized\n");
        return ERR_DISASM_WRAP_OK;
    }

    /* csopen(hardware architecture, hardware mode, handler) */
    if (cs_open(CS_ARCH_X86, CS_MODE_64, &disasm) != CS_ERR_OK) {
        ERROR("Cannot initialize disassembler.");
        return ERR_DISASM_WRAP_INIT;
    }

    /* turn on detail information. */
    if (cs_option(disasm, CS_OPT_DETAIL, CS_OPT_ON) != CS_ERR_OK) {
        ERROR("Cannot turn on detail feature.");
        cs_close(&disasm);
        return ERR_DISASM_WRAP_CHANGE_OPT;
    }

    LOG("end init_x86_64_disassembler\n");
    init = 1;
    return ERR_DISASM_WRAP_OK;
}


void stop_x86_64_disassembler()
{
    LOG("start stop_x86_64_disassembler\n");
    if (init) {
        cs_close(&disasm);
        init = 0;
    }

    LOG("end stop_x86_64_disassembler\n");
}


ERR_DISASM_WRAP init_instr(x86_64_instr **insn)
{
    LOG("start init_instr\n");
    *insn = cs_malloc(disasm);
    if (insn == NULL) {
        ERROR("Cannot allocate memory\n");
        return ERR_DISASM_WRAP_ALLOC;
    }

    LOG("end init_instr\n");
    return ERR_DISASM_WRAP_OK;
}


int get_instr(x86_64_instr *insn, const uint8_t *addr, uint64_t ip)
{
    LOG("start get_instr\n");
    int code_size = X86_MAX_INSTR_LEN;

    if (!cs_disasm_iter(disasm, &addr, (size_t*)&code_size, &ip, insn)) {
        ERROR("Cannot diasm instruction IP %"PRIx64".", ip);
        return (int)ERR_DISASM_WRAP_DISASM_PROCESS;
    }

    LOG("end get_instr\n");
    return (X86_MAX_INSTR_LEN - code_size);
}


void free_instr(x86_64_instr **insn)
{
    LOG("start free_instr\n");

    cs_free(*insn, 1);
    *insn = NULL;

    LOG("end free_instr\n");
}


const char *get_disasmwrap_error_string(ERR_DISASM_WRAP err)
{
    switch (err) {
    case ERR_DISASM_WRAP_OK:
        return "Ok.";
    case ERR_DISASM_WRAP_UNKNOWN:
        return "Unknown error.";
    case ERR_DISASM_WRAP_BAD_ARG:
        return "Bad arguments.";
    case ERR_DISASM_WRAP_INIT:
        return "Problem with disassembler initialization.";
    case ERR_DISASM_WRAP_CHANGE_OPT:
        return "Problem with changing disassemblers option.";
    case ERR_DISASM_WRAP_DISASM_PROCESS:
        return "Cannot disassemble instruction.";
    case ERR_DISASM_WRAP_ALLOC:
        return "Cannot allocate memory.";
    case ERR_DISASM_WRAP_DETECT_JMP_TARGET:
        return "Cannot calculate jmp target of control instruction.";
    default:
        return "Unknown error number";
    }
}


#ifdef __STDC__
    #if __STDC__ == 1
void print_x86_instr(FILE *f, const x86_64_instr *insn)
{
    LOG("start print_x86_instr\n");
    cs_x86 *x86 = &(insn->detail->x86);
    cs_x86_op *x86_op = x86->operands;

    /* Print mnemonic */
    fprintf(f, "%s %s\n", insn->mnemonic, insn->op_str);

    /* Print command bytes */
    uint16_t i = 0;
    uint16_t size = insn->size;
    const uint8_t *com = insn->bytes;
    for (i = 0; i < size; ++i)
        fprintf(f, "%.2x ", com[i]);
    putchar('\n');

    /* Print legacy prefixes */
    if (get_legacy_prefix_size(insn))
        fprintf(f, "legacy_prefixes: %x\n", get_legacy_prefixes(insn));

    /* Print vex-prefix */
    if (get_vex_prefix_size(insn))
        fprintf(f, "vex_prefix: %x\n", get_vex_prefix(insn));

    /* Print rex-prefix */
    if (get_rex_prefix_size(insn))
        fprintf(f, "rex: %.2x\n", get_rex_prefix(insn));

    /* Print opcode */
    fprintf(f, "opcode: %0*x\n", get_opcode_size(insn) << 1, get_opcode(insn));

    /* Print modrm */
    if (get_modrm_size(insn))
        fprintf(f, "modrm: %.2x\n", get_modrm(insn));

    /* Print sib */
    if (get_sib_size(insn))
        fprintf(f, "sib: %.2x\n", get_sib(insn));

    /* Print displacement */
    if (get_disp_size(insn))
        fprintf(f, "disp: %#0*x\n", get_disp_size(insn) << 1, get_disp(insn));

    /* Print immediate */
    if (get_imm_size(insn))
        fprintf(f, "imm: %#0*"PRIx64"\n", get_imm_size(insn) << 1, get_imm(insn));

    /* Print information of operands */
    fprintf(f, "\tOperands num: %u\n", x86->op_count);
    for (i = 0; i < x86->op_count; ++i) {
        fprintf(f, "\t\top->type:\n\t\t\t");
        switch (x86_op[i].type) {
        case X86_OP_INVALID:
            fprintf(f, "X86_OP_INVALID\n");
            break;

        case X86_OP_REG:
            fprintf(f, "X86_OP_REG\n");
            fprintf(f, "\t\treg:\n\t\t\t%s\n", cs_reg_name(disasm, x86_op[i].reg));
            break;

        case X86_OP_IMM:
            fprintf(f, "X86_OP_IMM\n");
            fprintf(f, "\t\timm:\n\t\t\t%"PRIx64"\n", x86_op[i].imm);
            break;

        case X86_OP_MEM:
            fprintf(f, "X86_OP_MEM\n");
            if (x86_op[i].mem.base != X86_REG_INVALID)
                fprintf(f, "\t\tbase:\n\t\t\t%s\n", cs_reg_name(disasm, x86_op[i].mem.base));
            if (x86_op[i].mem.index != X86_REG_INVALID) {
                fprintf(f, "\t\tindex:\n\t\t\t%s\n", cs_reg_name(disasm, x86_op[i].mem.index));
                fprintf(f, "\t\tscale:\n\t\t\t%d\n", x86_op[i].mem.scale);
            }

            fprintf(f, "\t\tdisp:\n\t\t\t%"PRIx64"\n", x86_op[i].mem.disp);
            break;

        case X86_OP_FP:
            fprintf(f, "X86_OP_FP\n");
            fprintf(f, "\t\tfp_val:\n\t\t\t%lf\n", x86_op[i].fp);
            break;

        default:
            break;
        }

    }

    LOG("end print_x86_instr\n");
}
    #endif /* __STDC__ == 1 */
#endif /* __STDC__ */


void get_instr_mnemonic(char *com, const x86_64_instr *insn)
{
    LOG("start get_instr_mnemonic\n");

    strcpy(com, insn->mnemonic);
    strcat(com, " ");
    strcat(com, insn->op_str);

    LOG("end get_instr_mnemonic\n");
}


const uint8_t *get_instr_encoding(const x86_64_instr *insn)
{
    LOG("start get_instr_encoding\n");
    LOG("end get_instr_encoding\n");
    return insn->bytes;
}


uint8_t get_instr_size(const x86_64_instr *insn)
{
    LOG("start get_instr_size\n");
    LOG("end get_instr_size\n");
    return (uint8_t)insn->size;
}


uint8_t get_legacy_prefix_size(const x86_64_instr *insn)
{
    LOG("start get_legacy_prefix_size\n");
    uint8_t legacy_prefix_size = 0;
    cs_x86 *x86 = &(insn->detail->x86);

    uint8_t i = 0;
    for (i = 0; i < 4; ++i) // could be up to 4 legacy prefixes
        if (x86->prefix[i] != 0)
            ++legacy_prefix_size;

    LOG("end get_legacy_prefix_size\n");
    return legacy_prefix_size;
}


uint8_t get_vex_prefix_size(const x86_64_instr *insn)
{
    LOG("start get_vex_prefix_size\n");
    uint8_t vex_prefix_size = 0;
    cs_x86 *x86 = &(insn->detail->x86);

    /* Disassembler put VEX/EVEX-prefixes in the x86->opcode field. */
    switch (x86->opcode[0]) {
    case 0xC5:
        /* 2-byte VEX-pref (128bit registers XMM) */
        vex_prefix_size = 2;
        break;
    case 0xC4:
        /* 3-byte VEX-pref (256bit registers YMM) */
        vex_prefix_size = 3;
        break;
    case 0x62:
        /* 4-byte EVEX-pref (512bit registers ZMM) */
        vex_prefix_size = 4;
        break;
    default:
        break;
    }

    LOG("end get_vex_prefix_size\n");
    return vex_prefix_size;
}


uint8_t get_rex_prefix_size(const x86_64_instr *insn)
{
    LOG("start get_rex_prefix_size\n");
    uint8_t rex_prefix_size = 0;
    cs_x86 *x86 = &(insn->detail->x86);

    if (get_vex_prefix_size(insn) == 0) {
        /*
         * REX-prefix could be only without VEX-prefix. Disassembler has bug
         * x86->rex with VEX-prefix instructions isn't zero.
         */
        if (x86->rex)
            ++rex_prefix_size;
    }

    LOG("end get_rex_prefix_size\n");
    return rex_prefix_size;
}


uint8_t get_prefixes_size(const x86_64_instr *insn)
{
    LOG("start get_prefixes_size\n");
    uint8_t prefix_size = get_legacy_prefix_size(insn)
                        + get_vex_prefix_size(insn)
                        + get_rex_prefix_size(insn);

    LOG("end get_prefixes_size\n");
    return prefix_size;
}


uint8_t get_opcode_size(const x86_64_instr *insn)
{
    LOG("start get_opcode_size\n");

    uint8_t opcode_size = 0;
    uint8_t prefix_size = get_prefixes_size(insn);
    const uint8_t *opcode = &(insn->bytes[prefix_size]);

    if (get_vex_prefix_size(insn)) {
        /* All VEX/EVEX-instructions have 1-byte opcode */
        opcode_size = 1;

    } else {
        switch (opcode[0]) {
        case 0x0F:
            /* 2 or 3-bytes opcode */
            if (opcode[1] == 0x38 || opcode[1] == 0x3A) {
                /* 3-bytes opcode */
                opcode_size = 3;
            } else {
                /* 2-bytes opcode */
                opcode_size = 2;
            }

            break;

        default:
            /* 1-byte opcode */
            opcode_size = 1;
            break;
        }
    }

    LOG("end get_opcode_size\n");
    return opcode_size;
}


uint8_t get_modrm_size(const x86_64_instr *insn)
{
    LOG("start get_modrm_size\n");
    uint8_t modrm_size = 0;
    cs_x86 *x86 = &(insn->detail->x86);

    if (get_vex_prefix_size(insn)) {
        /* VEX-EVEX instructions have necessarily ModRM */
        modrm_size = 1;
    } else {
        /* Ordinary instructions could have optional ModRM byte */
        if (x86->modrm) {
            modrm_size = 1;

        } else {
            if (x86->op_count == 0)
                modrm_size = 0;
            else {
                uint8_t i = 0;
                uint8_t reg_op_count = 0;
                for (i = 0; i < x86->op_count; ++i) {
                    if (x86->operands[i].type == X86_OP_MEM
                        || x86->operands[i].type == X86_OP_FP) {
                        modrm_size = 1;
                        break;
                    }
                    if (x86->operands[i].type == X86_OP_REG) {
                        ++reg_op_count;
                        if (reg_op_count == 2) {
                            modrm_size = 1;
                            break;
                        }
                    }
                }
            }
        }

    }

    LOG("end get_modrm_size\n");
    return modrm_size;
}


uint8_t get_sib_size(const x86_64_instr *insn)
{
    LOG("start get_sib_size\n");
    uint8_t sib_size = 0;
    cs_x86 *x86 = &(insn->detail->x86);

    if (get_modrm_size(insn)) {
        uint8_t modrm = x86->modrm;

        if (IS_SIB(modrm))
            sib_size = 1;
    }

    LOG("end get_sib_size\n");
    return sib_size;
}


uint8_t get_disp_size(const x86_64_instr *insn)
{
    LOG("start get_disp_size\n");
    uint8_t disp_size = 0;
    cs_x86 *x86 = &(insn->detail->x86);

    if (get_modrm_size(insn)) {
        uint8_t modrm = x86->modrm;
        uint8_t mod = x86_MOD(modrm);
        uint8_t rm = x86_RM(modrm);

        LOG("mod: %.2x\n", mod);
        switch (mod) {
        case 0x00:
            if (rm == 0x05)
                disp_size = 4;
            else if (IS_SIB(modrm)) {
                uint8_t sib = x86->sib;
                uint8_t base = x86_BASE(sib);

                if (base == 0x05)
                    disp_size = 4;
            }

            break;

        case 0x01:
            disp_size = 1;
            break;

        case 0x02:
            disp_size = 4;
            break;

        default:
            /* 0x03 */
            disp_size = 0;
            break;
        }
    } else {
        /* Capstone bugs: Disasm in these cases make insn->disp = 0. */
        switch (get_instr_group(insn)) {
        case JCC_GROUP:
        {
            uint32_t opcode = get_opcode(insn);
            if (((uint8_t*)&opcode)[0] == 0x0F)
                disp_size = 4;
            else
                disp_size = 1;
            break;
        }
        case JMP_GROUP:
        {
            uint32_t opcode = get_opcode(insn);
            if (opcode == 0xEB)
                disp_size = 1;
            else if (opcode == 0xE9)
                disp_size = 4;

            break;
        }
        case CALL_GROUP:
        {
            disp_size = 4;
            break;
        }
        default:
            break;
        }
    }

    LOG("end get_disp_size\n");
    return disp_size;
}


uint8_t get_imm_size(const x86_64_instr *insn)
{
    LOG("start get_imm_size\n");
    uint8_t imm_size = 0;
    cs_x86 *x86 = &(insn->detail->x86);

    if (x86->op_count == 0) {
        imm_size = 0;

    } else {
        uint8_t i = 0;
        for (i = 0; i < x86->op_count; ++i)
            if (x86->operands[i].type == X86_OP_IMM) {
                imm_size = x86->operands[i].size;
                break;
            }
    }

    LOG("end get_imm_size\n");
    return imm_size;
}


uint32_t get_legacy_prefixes(const x86_64_instr *insn)
{
    LOG("start get_legacy_prefixes\n");
    uint32_t legacy_prefixes = 0;
    cs_x86 *x86 = &(insn->detail->x86);

    uint8_t i = 0;
    for (i = 0; i < 4; ++i) // could be up to 4 legacy prefixes
        if (x86->prefix[i] != 0) {
            legacy_prefixes <<= 8;
            legacy_prefixes |= x86->prefix[i];
        }

    LOG("end get_legacy_prefixes\n");
    return legacy_prefixes;
}


uint32_t get_vex_prefix(const x86_64_instr *insn)
{
    LOG("start get_vex_prefix\n");
    uint32_t vex_prefix = 0;
    uint8_t vex_prefix_size = get_vex_prefix_size(insn);
    cs_x86 *x86 = &(insn->detail->x86);

    vex_prefix = GET_HIGHEST_N_BYTES_32(vex_prefix_size, *((uint32_t*)x86->opcode));

    LOG("end get_vex_prefix\n");
    return vex_prefix;
}


uint8_t get_rex_prefix(const x86_64_instr *insn)
{
    LOG("start get_rex_prefix\n");
    uint32_t rex_prefix = 0;
    cs_x86 *x86 = &(insn->detail->x86);

    if (get_rex_prefix_size(insn))
        rex_prefix = x86->rex;

    LOG("end get_rex_prefix\n");
    return rex_prefix;
}


uint32_t get_opcode(const x86_64_instr *insn)
{
    LOG("start get_opcode\n");
    uint8_t prefixex_size = get_prefixes_size(insn);
    uint8_t opcode_size = get_opcode_size(insn);
    const uint8_t *op_bytes = &(insn->bytes[prefixex_size]);
    uint8_t i = 0;

    uint32_t opcode = 0;
    for (i = 0; i < opcode_size; ++i) {
        opcode <<= 8;
        opcode |= op_bytes[i];
    }

    LOG("end get_opcode\n");
    return opcode;
}


uint8_t get_modrm(const x86_64_instr *insn)
{
    LOG("start get_modrm\n");
    uint8_t modrm = 0;
    cs_x86 *x86 = &(insn->detail->x86);

    if (get_modrm_size(insn))
        modrm = x86->modrm;

    LOG("end get_modrm\n");
    return modrm;
}


uint8_t get_sib(const x86_64_instr *insn)
{
    LOG("start get_sib\n");
    uint8_t sib = 0;
    cs_x86 *x86 = &(insn->detail->x86);

    if (get_sib_size(insn))
        sib = x86->sib;

    LOG("end get_sib\n");
    return sib;
}


int32_t get_disp(const x86_64_instr *insn)
{
    LOG("start get_disp\n");
    int32_t disp = 0;
    cs_x86 *x86 = &(insn->detail->x86);

    if (get_disp_size(insn))
        switch (get_instr_group(insn)) {
        case JCC_GROUP:
        case JMP_GROUP:
        case CALL_GROUP:
            /* Capstone bugs: Disasm in these cases make insn->disp = 0. */
            /* call/jmp rel8/32 */
            if (get_modrm_size(insn))
                disp = x86->disp;
            else
                disp = (uint64_t)get_imm(insn) - insn->address - insn->size;
            break;

        default:
            disp = x86->disp;
            break;
        }


    LOG("end get_disp\n");
    return disp;
}


int64_t get_imm(const x86_64_instr *insn)
{
    LOG("start get_imm\n");
    int64_t imm = 0;
    cs_x86 *x86 = &(insn->detail->x86);

    uint8_t i = 0;
    uint8_t op_count = x86->op_count;
    for (i = 0; i < op_count; ++i)
        if (x86->operands[i].type == X86_OP_IMM)
            imm = x86->operands[i].imm;

    LOG("end get_imm\n");
    return imm;
}


uint64_t get_ip(const x86_64_instr *insn)
{
    LOG("start get_imm\n");
    LOG("end get_imm\n");
    return insn->address;
}


int64_t get_instr_num(const uint8_t *src, uint64_t src_size, uint64_t ip)
{
    LOG("START get_instr_num\n");

    /* init instruction descriptor */
    x86_64_instr *insn = NULL;
    if (init_instr(&insn) == ERR_DISASM_WRAP_ALLOC) {
        ERROR("Cannot alloc memory for instruction descriptor.");
        return ERR_DISASM_WRAP_ALLOC;
    }

    uint64_t RIP = ip;
    uint8_t instr_len = 0;
    int64_t instr_num = 0;
    const uint8_t *addr = src;
    for (RIP = ip; RIP < ip + src_size; RIP += instr_len, addr += instr_len) {
        if (get_instr(insn, addr, RIP) == ERR_DISASM_WRAP_DISASM_PROCESS) {
            ERROR("Cannot diasm instruction RIP %#"PRIx64".", RIP);
            free_instr(&insn);
            return ERR_DISASM_WRAP_DISASM_PROCESS;
        }

        /* get instruction length */
        instr_len = get_instr_size(insn);

        ++instr_num;
    }

    LOG("END get_instr_num\n");
    return instr_num;
}


int64_t get_code_size(const uint8_t *src, uint64_t src_size)
{
    LOG("START get_code_size\n");

    /* init instruction descriptor */
    x86_64_instr *insn = NULL;
    if (init_instr(&insn) == ERR_DISASM_WRAP_ALLOC) {
        ERROR("Cannot alloc memory for instruction descriptor.");
        return ERR_DISASM_WRAP_ALLOC;
    }

    int64_t real_size = 0;
    uint64_t ip = 0x100000000;
    uint64_t RIP = 0x100000000;
    uint8_t instr_len = 0;
    const uint8_t *addr = src;
    for (RIP = ip; RIP < ip + src_size; RIP += instr_len, addr += instr_len) {
        if (get_instr(insn, addr, RIP) == ERR_DISASM_WRAP_DISASM_PROCESS) {
            ERROR("Cannot diasm instruction RIP %#"PRIx64".", RIP);
            free_instr(&insn);
            return ERR_DISASM_WRAP_DISASM_PROCESS;
        }

        /* get instruction length */
        instr_len = get_instr_size(insn);
        real_size += instr_len;
    }

    LOG("END get_code_size\n");
    return real_size;
}


ERR_DISASM_WRAP get_jmp_target( x86_64_instr *insn,
                                const uint8_t *src,
                                size_t ip,
                                uint64_t *jmp_target    )
{
    LOG("start get_jmp_target\n");
    UNUSED(ip);
    if (insn == NULL || jmp_target == NULL) {
        ERROR("Invalid arguments.");
        return ERR_DISASM_WRAP_BAD_ARG;
    }

    uint64_t instr_len = insn->size;
    ERR_DISASM_WRAP err = ERR_DISASM_WRAP_OK;

    cs_x86 *x86 = &(insn->detail->x86);
    cs_x86_op *x86_op = x86->operands;
    switch (x86_op[0].type) {
    case X86_OP_REG:
        /* !TODO: absolute jmp/call register */
        ERROR("Face with absolute jmp/call register.");
        err = ERR_DISASM_WRAP_DETECT_JMP_TARGET;
        break;

    case X86_OP_IMM:
        /* if it's relative ip instruction: jmp/call rel32 */
        LOG("ip 0x%zx imm 0x%"PRIx64"\n", ip, x86_op[0].imm);
        *jmp_target = (uint64_t)x86_op[0].imm;
        break;

    case X86_OP_MEM:
        if (x86_op[0].mem.base == X86_REG_RIP &&
            x86_op[0].mem.index == X86_REG_INVALID) {
            *jmp_target = *((size_t*)((size_t)src + instr_len + S32_TO_U64(x86->disp)));
        } else {
            /* !TODO: absolute jmp/call mem */
            ERROR("Face with absolute jmp/call mem.");
            err = ERR_DISASM_WRAP_DETECT_JMP_TARGET;
        }

        break;

    default:
        err = ERR_DISASM_WRAP_UNKNOWN;
        break;

    }

    LOG("end get_jmp_target\n");
    return err;
}


X86_ADDR_TYPE get_addr_type(const x86_64_instr *insn)
{
    LOG("start get_addr_type\n");
    X86_ADDR_TYPE type = NO_X86_ADDR_TYPE;
    cs_x86 *x86 = &(insn->detail->x86);
    uint8_t modrm = x86->modrm;

    if (x86_IS_RIPREL(modrm))
        type = RIP_RELATIVE_X86_ADDR_TYPE;
    else if (cs_insn_group(disasm, insn, X86_GRP_JUMP)
            || cs_insn_group(disasm, insn, X86_GRP_CALL))
    {
        uint8_t op_count = x86->op_count;
        if (op_count == 1 && x86->operands[0].type == X86_OP_IMM)
            type = RELATIVE_X86_ADDR_TYPE;
        else
            type = ABSOLUTE_X86_ADDR_TYPE;
    } else {
        uint8_t i = 0;
        uint8_t op_count = x86->op_count;
        for (i = 0; i < op_count; ++i)
            if (x86->operands[i].type == X86_OP_MEM)
                type = ABSOLUTE_X86_ADDR_TYPE;
    }


    LOG("end get_addr_type\n");
    return type;
}


uint8_t update_riprel_insn(uint8_t *new_com, const x86_64_instr *pInsn, int32_t offs)
{
    LOG("start update_riprel_insn\n");

    const uint8_t *old_com = get_instr_encoding(pInsn);

    uint8_t i = 0;
    uint8_t new_instr_len = 0;
    uint8_t old_instr_len = get_instr_size(pInsn);

    /* Copy all prefixes. */
    uint8_t prefixes_size = get_prefixes_size(pInsn);
    for (i = 0; i < prefixes_size; ++i)
        new_com[i] = old_com[i];

    new_instr_len = prefixes_size;

    /* Copy opcode. */
    uint8_t opcode_size = get_opcode_size(pInsn);
    for (i = 0; i < opcode_size; ++i, ++new_instr_len)
        new_com[new_instr_len] = old_com[new_instr_len];

    /* Copy ModRM byte. */
    new_com[new_instr_len] = old_com[new_instr_len];
    ++new_instr_len;

    /* Update and copy displacement. */
    int32_t disp = get_disp(pInsn) - offs;
    for (i = 0; i < 4; ++i, ++new_instr_len)
        new_com[new_instr_len] = ((uint8_t*)(&disp))[i];

    /* Copy immediate. */
    for (i = new_instr_len; i < old_instr_len; ++i, ++new_instr_len)
        new_com[i] = old_com[i];

    LOG("end update_riprel_insn\n");
    return new_instr_len;
}


int is_instr_jump(const x86_64_instr *pInsn)
{
    if (cs_insn_group(disasm, pInsn, X86_GRP_JUMP))
        return 1;
    else
        return 0;
}

X86_INSTR_TYPE get_instr_type(const x86_64_instr *pInsn)
{
    switch (pInsn->id) {
    case X86_INS_JA:
        return JA;
    case X86_INS_JAE:
        return JAE;
    case X86_INS_JB:
        return JB;
    case X86_INS_JBE:
        return JBE;
    case X86_INS_JCXZ:
        return JCXZ;
    case X86_INS_JECXZ:
        return JECXZ;
    case X86_INS_JRCXZ:
        return JRCXZ;
    case X86_INS_JE:
        return JE;
    case X86_INS_JG:
        return JG;
    case X86_INS_JGE:
        return JGE;
    case X86_INS_JL:
        return JL;
    case X86_INS_JLE:
        return JLE;
    case X86_INS_JNE:
        return JNE;
    case X86_INS_JNO:
        return JNO;
    case X86_INS_JNP:
        return JNP;
    case X86_INS_JNS:
        return JNS;
    case X86_INS_JO:
        return JO;
    case X86_INS_JP:
        return JP;
    case X86_INS_JS:
        return JS;
    case X86_INS_JMP:
        return JMP;
    case X86_INS_CALL:
        return CALL;
    case X86_INS_TEST:
        return TEST;
    default:
        return OTHER;
    }
}


X86_INSTR_GROUP get_instr_group(const x86_64_instr *pInsn)
{
    switch (pInsn->id) {
    case X86_INS_JA:
    case X86_INS_JAE:
    case X86_INS_JB:
    case X86_INS_JBE:
    case X86_INS_JCXZ:
    case X86_INS_JECXZ:
    case X86_INS_JRCXZ:
    case X86_INS_JE:
    case X86_INS_JG:
    case X86_INS_JGE:
    case X86_INS_JL:
    case X86_INS_JLE:
    case X86_INS_JNE:
    case X86_INS_JNO:
    case X86_INS_JNP:
    case X86_INS_JNS:
    case X86_INS_JO:
    case X86_INS_JP:
    case X86_INS_JS:
        return JCC_GROUP;
    case X86_INS_JMP:
        return JMP_GROUP;
    case X86_INS_CALL:
        return CALL_GROUP;
    default:
        return OTHER_GROUP;
    }
}


uint8_t *put_jcc_32(const x86_64_instr *insn,
                    uint8_t *dest,
                    uint64_t ip,
                    uint64_t jmp_point)
{
    switch (insn->id) {
    case X86_INS_JA:
        return put_jcc_rel32(dest, ip, jmp_point, JA);
    case X86_INS_JAE:
        return put_jcc_rel32(dest, ip, jmp_point, JAE);
    case X86_INS_JB:
        return put_jcc_rel32(dest, ip, jmp_point, JB);
    case X86_INS_JBE:
        return put_jcc_rel32(dest, ip, jmp_point, JBE);
    case X86_INS_JCXZ:
    case X86_INS_JECXZ:
    case X86_INS_JRCXZ:
        ERROR("There is no JCXZ rel32 instr.\n");
        break;
    case X86_INS_JE:
        return put_jcc_rel32(dest, ip, jmp_point, JE);
    case X86_INS_JG:
        return put_jcc_rel32(dest, ip, jmp_point, JG);
    case X86_INS_JGE:
        return put_jcc_rel32(dest, ip, jmp_point, JGE);
    case X86_INS_JL:
        return put_jcc_rel32(dest, ip, jmp_point, JL);
    case X86_INS_JLE:
        return put_jcc_rel32(dest, ip, jmp_point, JLE);
    case X86_INS_JNE:
        return put_jcc_rel32(dest, ip, jmp_point, JNE);
    case X86_INS_JNO:
        return put_jcc_rel32(dest, ip, jmp_point, JNO);
    case X86_INS_JNP:
        return put_jcc_rel32(dest, ip, jmp_point, JNP);
    case X86_INS_JNS:
        return put_jcc_rel32(dest, ip, jmp_point, JNS);
    case X86_INS_JO:
        return put_jcc_rel32(dest, ip, jmp_point, JO);
    case X86_INS_JP:
        return put_jcc_rel32(dest, ip, jmp_point, JP);
    case X86_INS_JS:
        return put_jcc_rel32(dest, ip, jmp_point, JS);
    default:
        ERROR("UNKNOWN COMMAND.");
        break;
    }

    return NULL;
}


int64_t is_instr_jmp_target(const uint8_t  *start,
                            uint64_t size,
                            const uint8_t  *addr,
                            uint64_t ip )
{
    LOG("START is_instr_jmp_target\n");

    x86_64_instr *pInsn = NULL;
    ERR_DISASM_WRAP d_err = init_instr(&pInsn);
    if (d_err != ERR_DISASM_WRAP_OK) {
        ERROR("init_instr.\n"
            "%s", get_disasmwrap_error_string(d_err));
        return d_err;
    }

    uint8_t instr_len = 0;
    const uint8_t *source = start;
    uint64_t start_ip = ip + (uint64_t)start - (uint64_t)addr;
    uint64_t instr_ip = start_ip;
    for (instr_ip = start_ip; instr_ip < start_ip + size; instr_ip += instr_len) {
        uint64_t target = 0;

        source += instr_len;

        d_err = (ERR_DISASM_WRAP)get_instr(pInsn, source, instr_ip);
        if (d_err < 0) {
            ERROR("Cannot diasm instr RIP %#"PRIx64"."
                "%s", instr_ip, get_disasmwrap_error_string(d_err));
            free_instr(&pInsn);
            return d_err ;
        }

        instr_len = get_instr_size(pInsn);
        X86_INSTR_GROUP group = get_instr_group(pInsn);
        if (group == JCC_GROUP || group == JMP_GROUP || group == CALL_GROUP) {
            d_err = get_jmp_target(pInsn, source, instr_ip, &target);
            if (d_err != ERR_DISASM_WRAP_OK) {
                EXEC_CODE_DEBUG(print_x86_instr(stdout, pInsn));
                ERROR("get_jmp_target().\n"
                    "%s", get_disasmwrap_error_string(d_err));
                free_instr(&pInsn);
                return d_err;
            }
        }

        if (target == ip) {
            LOG("ENDFUNC: Instr is jmp target\n");
            return 1;
        }
    }

    LOG("ENDFUNC: Instr is not jmp target\n");
    return 0;
}

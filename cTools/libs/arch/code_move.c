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

#include "code_move.h"

int
cmp_bt_reloc(const void *a, const void *b)
{
    bt_reloc a_rel = *(const bt_reloc*)a;
    bt_reloc b_rel = *(const bt_reloc*)b;
    if (a_rel.old_pc < b_rel.old_pc)
        return -1;
    if (a_rel.old_pc > b_rel.old_pc)
        return 1;

    return 0;
}

uint64_t
get_instr_new_addr(uint64_t old_pc, const Sorted_vector *rel)
{
    bt_reloc old = {
        .old_pc = old_pc,
    };

    bt_reloc *r = sorted_vector_find_elem(rel, &old);
    if (r != NULL)
        return r->new_pc;
    else
        return old_pc;
}

const char *get_code_move_err_str(CODE_MOVE_ERROR err)
{
    switch(err) {
    case CODE_MOVE_ERROR_OK:
        return "Ok.";
    case CODE_MOVE_ERROR_BAD_ARG:
        return "Bad arguments.";
    case CODE_MOVE_ERROR_NO_MEM:
        return "There is no enougth memory.";
    case CODE_MOVE_ERROR_DISASM_INIT_ERROR:
        return "Problem with initialization of disassembler.";
    case CODE_MOVE_ERROR_BAD_DST:
        return "Size of desctination buffer less then needed.";
    case CODE_MOVE_ERROR_UNKNOWN_INSTR:
        return "Disassembler cannot process an invalid instruction.";
    case CODE_MOVE_ERROR_UNKNOWN:
    default:
        return "Unknown error.";
    }
}


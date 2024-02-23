/***
 * MIT License
 *
 * Copyright (c) 2024 Konychev Valera
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
#include "mod_code.h"
#include "binParse.h"
#include "binDynMod.h"
#include <inttypes.h>

mod_code_t *mc = NULL;
uint64_t num_mc = 0;

int
mod_code_init(const char *fn)
{
    if (initBinParser(fn)) {
        return -1;
    }
    if (initBinDynMod(binParser.type)) {
        return -1;
    }

    BinSectPtr mc_struct = binParser.getSectByName(binParser.bin, MC_STRUCT_SECTION);
    if (mc_struct == NULL) {
        STDERROR_PRINT( "Cannot get information about "MC_STRUCT_SECTION" section.");
        goto eexit_0;
    }

    uint64_t mc_struct_vaddr = binParser.getSectAddr(mc_struct);
    STDERROR_PRINT("mc_struct_vaddr: %"PRIx64"\n", mc_struct_vaddr);
    if (mc_struct_vaddr == (uint64_t)-1) {
        STDERROR_PRINT_DEBUG( "Cannot get vaddr of "MC_STRUCT_SECTION" section.");
        goto eexit_0;
    }

    uint64_t seed = binDynMod.get_seed();
    mc = (mod_code_t*)(mc_struct_vaddr + seed);
    STDERROR_PRINT("seed: %"PRIx64"\n", seed);

    uint64_t mc_struct_size = binParser.getSectSize(mc_struct);
    if (mc_struct_size == (size_t)-1) {
        STDERROR_PRINT_DEBUG( "Cannot get size of "MC_STRUCT_SECTION" section.");
        goto eexit_0;
    }

    num_mc = mc_struct_size / sizeof(mod_code_t);

    finiBinParser();
    return 0;

eexit_0:
    finiBinParser();
    return -1;
}

void
mod_code_print(mod_code_t *mc)
{
    STDERROR_PRINT("%12s: %p\n", "insert_point", (void*)mc->insert_point);
    STDERROR_PRINT("%12s: %p\n", "start", (void*)mc->start);
    STDERROR_PRINT("%12s: %p\n", "end", (void*)mc->end);
}

void
mod_code_dump(void)
{
    uint64_t i = 0;
    for (i = 0; i < num_mc; ++i) {
        mod_code_print(mc + i);
    }
}


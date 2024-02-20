/***
 * MIT License
 *
 * Copyright (c) 2023-2024 Konychev Valera
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
 { * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR AARCH64_INSTR_TYPE_CBR },
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include "mem.h"
#include "arch.h"
#include "comdef.h"
#include "binParse.h"
#include "aarch64_code_move.h"
#include "aarch64_global_stack.h"

#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

#if AARCH64_DEFINED == 1
extern
void call_func0(void *arg);
void call_func0(void *arg)
{
    UNUSED(arg);
    putchar('a');
    putchar('\n');
}

extern
void call_func1(void *arg);
void call_func1(void *arg)
{
    UNUSED(arg);
    printf("Hello call_function\n");
}

extern
void call_func2(uint32_t *n);
void call_func2(uint32_t *n)
{
    printf("n %u\n", *n);
    printf("n_addr %p\n", n);
}

static volatile
uint32_t temp = 0;
extern
void call_func3(uint32_t *n);
void call_func3(uint32_t *n)
{
    temp = *n;
    printf("temp %"PRIx32" n_addr %p\n", temp, n);
}
extern
void call_func4(uint32_t *n);
void call_func4(uint32_t *n)
{
    if (*n) {
        printf("call_func4\n");
    }
}

extern
void call_func5(uint32_t *n);
void call_func5(uint32_t *n)
{
    uint32_t i = 0;
    for (i = 0; i < *n; ++i) {
        printf("call_func5\n");
    }
}

static volatile
uint32_t temp6 = 0;
extern
void call_func6(uint32_t *n);
void call_func6(uint32_t *n)
{
    ++temp6;
    if (temp6 == 3) {
        return;
    }

    call_func6(n);
}

extern
void test_move_and_exec(void *func, const char *fn, void *arg);// __attribute__((section("__TEXT,__my_sect")));

void test_move_and_exec(void *func, const char *fn, void *arg)
{
    BinSymPtr sym = binParser.getSymByName(binParser.bin, fn);
    uint64_t func_size = binParser.getSSymSize(binParser.bin, sym);
    uint64_t buff_size = (uint64_t)aarch64_estimate_space( (const uint8_t*)func
                                               , (uint64_t)func
                                               , (uint64_t)-1
                                               , func_size
                                               );

    buff_size += 1024;
    uint8_t *buff = Calloc(buff_size, 1);
    if (buff == NULL)
        return;

    vt_sorted_vector_t sv;
    vt_sorted_vector_init(&sv, 1, sizeof(bt_reloc), cmp_bt_reloc);

    aarch64_code_move( (const uint8_t*)func
                     , buff
                     , (uint64_t)func
                     , (uint64_t)buff
                     , func_size
                     , buff_size
                     , NULL
                     );

    void *addr = (void*)alignToPageSize((size_t)buff);
    if (vt_mprotect(addr, 4096, VT_PROT_READ | VT_PROT_EXEC)) {
        PERROR("Cannot change memory protection");
    }

    UNUSED(arg);
    ((void (*)(void*))buff)(arg);

    if (vt_mprotect(addr, 4096, VT_PROT_READ | VT_PROT_WRITE)) {
        PERROR("Cannot change memory protection");
    }
    Free(buff);
    vt_sorted_vector_fini(&sv);
}

#endif /* AARCH64_DEFINED == 1 */

int main(int argc, const char *argv[])
{
    UNUSED(argc);

    initBinParser(argv[0]);

#if AARCH64_DEFINED == 1
    test_move_and_exec(call_func0, SYM_PREFIX"call_func0", NULL);
    test_move_and_exec(call_func1, SYM_PREFIX"call_func1", NULL);
    uint32_t arg32 = 3;
    test_move_and_exec(call_func2, SYM_PREFIX"call_func2", &arg32);
    test_move_and_exec(call_func3, SYM_PREFIX"call_func3", &arg32);
    test_move_and_exec(call_func4, SYM_PREFIX"call_func4", &arg32);
    test_move_and_exec(call_func5, SYM_PREFIX"call_func5", &arg32);
    test_move_and_exec(call_func6, SYM_PREFIX"call_func6", &arg32);
#endif /* AARCH64_DEFINED == 1 */

    finiBinParser();
    return 0;
}

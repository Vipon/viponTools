/***
 * MIT License
 *
 * Copyright (c) 2024 Konychev Valerii
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

#include "vt_containers.h"
#include "aarch64_global_stack.h"
#include <inttypes.h>

typedef struct {
    uint64_t addr;
    uint64_t key;
} ret_addr;

static vt_stack_t global_stack = {
    .v.capacity = 0,
    .v.data = NULL,
    .v.elem_size = sizeof(ret_addr),
    .v.end = 0,
};
static __attribute__ ((destructor(101)))
void desctructor_global_stack(void)
{
    vt_stack_fini(&global_stack);
}

extern void _push_x30(uint64_t x30, uint16_t key);
void
_push_x30(uint64_t x30, uint16_t key)
{
    STDERROR_PRINT("push_x30 start: x30 0x%"PRIx64 ", key %"PRIu16"\n", x30, key);
    ret_addr ret = {
        .addr = x30,
        .key = key,
    };
    vt_stack_push(&global_stack, &ret);
    STDERROR_PRINT("push_x30 end\n");
}

#if AARCH64_DEFINED == 1
asm(
".globl _push_x30\n"
"_push_x30:\n"
    "stp x0, x1, [sp, #-16]!\n"
    "stp x2, x3, [sp, #-16]!\n"
    "stp x4, x5, [sp, #-16]!\n"
    "stp x6, x7, [sp, #-16]!\n"
    "stp x8, x9, [sp, #-16]!\n"
    "stp x10, x11, [sp, #-16]!\n"
    "stp x12, x13, [sp, #-16]!\n"
    "stp x14, x15, [sp, #-16]!\n"
    "stp x16, x17, [sp, #-16]!\n"
    "stp x18, x19, [sp, #-16]!\n"
    "stp x20, x21, [sp, #-16]!\n"
    "stp x22, x23, [sp, #-16]!\n"
    "stp x24, x25, [sp, #-16]!\n"
    "stp x26, x27, [sp, #-16]!\n"
    "stp x28, x29, [sp, #-16]!\n"
    "str x30, [sp, #-16]!\n"
    "bl __push_x30\n"
    "ldr x30, [sp], #16\n"
    "ldp x28, x29, [sp], #16\n"
    "ldp x26, x27, [sp], #16\n"
    "ldp x24, x25, [sp], #16\n"
    "ldp x22, x23, [sp], #16\n"
    "ldp x20, x21, [sp], #16\n"
    "ldp x18, x19, [sp], #16\n"
    "ldp x16, x17, [sp], #16\n"
    "ldp x14, x15, [sp], #16\n"
    "ldp x12, x13, [sp], #16\n"
    "ldp x10, x11, [sp], #16\n"
    "ldp x8, x9, [sp], #16\n"
    "ldp x6, x7, [sp], #16\n"
    "ldp x4, x5, [sp], #16\n"
    "ldp x2, x3, [sp], #16\n"
    "ldp x0, x1, [sp], #16\n"
    "ret\n"
);
#else // AARCH64_DEFINED
void
push_x30(uint64_t x30, uint16_t key)
{
    UNUSED(x30);
    UNUSED(key);
}
#endif // AARCH64_DEFINED

extern uint64_t _pop_x30(uint16_t key);
uint64_t
_pop_x30(uint16_t key)
{
    STDERROR_PRINT("pop_x30: key %"PRIu16"\n", key);

    ret_addr *ret;
    do {
        ret = (ret_addr*) vt_stack_pop(&global_stack);
        STDERROR_PRINT("pop_x30: addr: 0x%"PRIx64", key %"PRIu64"\n", ret->addr, ret->key);
    } while (ret->key != key);

    return ret->addr;
}

#if AARCH64_DEFINED == 1
asm(
".globl _pop_x30\n"
"_pop_x30:\n"
    "str x1, [sp, #-16]!\n"
    "stp x2, x3, [sp, #-16]!\n"
    "stp x4, x5, [sp, #-16]!\n"
    "stp x6, x7, [sp, #-16]!\n"
    "stp x8, x9, [sp, #-16]!\n"
    "stp x10, x11, [sp, #-16]!\n"
    "stp x12, x13, [sp, #-16]!\n"
    "stp x14, x15, [sp, #-16]!\n"
    "stp x16, x17, [sp, #-16]!\n"
    "stp x18, x19, [sp, #-16]!\n"
    "stp x20, x21, [sp, #-16]!\n"
    "stp x22, x23, [sp, #-16]!\n"
    "stp x24, x25, [sp, #-16]!\n"
    "stp x26, x27, [sp, #-16]!\n"
    "stp x28, x29, [sp, #-16]!\n"
    "str x30, [sp, #-16]!\n"
    "bl __pop_x30\n"
    "ldr x30, [sp], #16\n"
    "ldp x28, x29, [sp], #16\n"
    "ldp x26, x27, [sp], #16\n"
    "ldp x24, x25, [sp], #16\n"
    "ldp x22, x23, [sp], #16\n"
    "ldp x20, x21, [sp], #16\n"
    "ldp x18, x19, [sp], #16\n"
    "ldp x16, x17, [sp], #16\n"
    "ldp x14, x15, [sp], #16\n"
    "ldp x12, x13, [sp], #16\n"
    "ldp x10, x11, [sp], #16\n"
    "ldp x8, x9, [sp], #16\n"
    "ldp x6, x7, [sp], #16\n"
    "ldp x4, x5, [sp], #16\n"
    "ldp x2, x3, [sp], #16\n"
    "ldr x1, [sp], #16\n"
    "ret\n"
);
#else // AARCH64_DEFINED
void
pop_x30(uint16_t key)
{
    UNUSED(key);
}
#endif // AARCH64_DEFINED


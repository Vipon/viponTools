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

#ifndef __PERF_H
#define __PERF_H

// vipon headers
#include "comdef.h"

// C-standard headers
#include <stdint.h>

#if IA32_DEFINED
# include <x86intrin.h>

static INLINE uint64_t getCPUCycles(void)
{
    uint32_t IA32_TSC_AUX = 0;

    // From Intel SDM:
    //  The RDTSCP instruction is not a serializing instruction, but it does wait
    //  until all previous instructions have executed and all previous loads are
    //  globally visible. But it does not wait for previous stores to be globally
    //  visible, and subsequent instructions may begin execution before the read
    //  operation is performed. The following items may guide software seeking to
    //  order executions of RDTSCP:
    //
    //  If software requires RDTSCP to be executed only after all previous stores
    //  are globally visible, it can execute MFENCE immediately before RDTSCP.
    //
    //  If software requires RDTSCP to be executed prior to execution of any
    //  subsequent instruction (including any memory accesses), it can execute
    //  LFENCE immediately after RDTSCP.
    _mm_mfence();
    uint64_t cycles = __rdtscp(&IA32_TSC_AUX);
    _mm_lfence();

    return cycles;
}

#elif ARM32_DEFINED || ARM64_DEFINED
# include <arm_acle.h>

static INLINE uint64_t readPmccntr(void)
{
    uint64_t cycles = 0;

# if ARM64_DEFINED
    // pmccntr_el0 (performance counter) counts cpu cycles.
    // Using "memory" to prevent compiler instruction reordering.
    asm volatile("mrs %0, pmccntr_el0" : "=r" (cycles) : : "memory");
# elif ARM32_DEFINED
    // MRC - Move to CPU Register from Coreprocessor register
    asm volatile("mrc p15, 0, %0, c9, c13, 0" : "=r"(cycles) : : "memory");
# else
#  error Unknown ARM Arch
# endif

    return cycles;
}

static INLINE uint64_t getCPUCycles(void)
{
    // PMCCNTR can be read out-of-order, so add instruction barrier - ISB.
    // From the ARM C Language Extensions (ACLE): The only supported value for
    // the __isb intrinsic is 15
    __isb(15);
    uint64_t cycles = readPmccntr();
    __isb(15);

    return cycles;
}

#else
# error "Unsupported instruction architecture"
#endif

#endif /* __PERF_H */


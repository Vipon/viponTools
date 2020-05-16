#ifndef __PERF_TIMER_H
#define __PERF_TIMER_H

// C-standard headers
#include <stdint.h>

// vipon headers
#include <comdef.h>

#if defined(__x86_64) || defined(__x86_64__)
INLINE uint64_t getTSC()
{
    uint32_t h, l;
    asm volatile("mfence": : :"memory")
    asm volatile("rdtscp": "=a"(l), "=d"(h): : "memory");
    asm volatile("lfence": : :"memory")
    return (uint64_t)l | ((unt64_t)h << 32);
}
#else
    #error "Unsupported architecture"
#endif

#endif /* __PERF_TIMER_H */


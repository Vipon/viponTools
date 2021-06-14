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

#ifndef _BITS_H
#define _BITS_H

#ifdef __STDC__
    #if __STDC__ == 1
#include <stdio.h>
#include <stdint.h>

inline void print_bytes(FILE *f, const uint8_t *array, size_t num)
{
    size_t i = 0;
    for (i = 0; i < num; ++i)
        fprintf(f, "%.2x", array[i]);

    fprintf(f, "\n");
}
    #endif /* __STDC__ == 1 */
#else /* __STDC__ */
    #error "*** ERROR: Need standard C library or equivalent. ***"
#endif /* __STDC__ */


/*
 * Description:
 *  Result is the highest N bytes of 32-bits value.
 */
#define GET_HIGHEST_N_BYTES_32(n, val32) \
    (((uint32_t)(val32)) >> ((4 - n) << 3))


/*
 * Description:
 *  Result is the highest N bytes of 64-bits value.
 */
#define GET_HIGHEST_N_BYTES_64(n, val64) \
    (((uint64_t)(val32)) >> ((8 - n) << 3))


/*
 * Description:
 *  Macros checks: could 64-bits signed value be represent in 32-bits value.
 */
#define IS_32_BITS_ENOUGH_64(val64) \
    (((((int64_t)val64) >> 33) == 0) || \
        ((~(((int64_t)val64) >> 33)) == 0))


/*
 * Description:
 *  Macros checks: could sum of 8-bits signed value be represent in 8-bits sign
 *  value.
 */
#define IS_SUM_OF_8_ENOUGH_8(a8, b8) \
    (((((int16_t)(a8 + b8)) >> 9) == 0) || \
        ((~(((int16_t)(a8 + b8)) >> 9)) == 0))


/*
 * Description:
 *  Macros checks: could sum of 32-bits signed value and 8-bits signed value
 *  be represent in 32-bits signed value.
 */
#define IS_SUM_OF_8_32_ENOUGH_32(a32, b8) \
    (((((int64_t)(a32 + b8)) >> 31) == 0) || \
        ((~(((int64_t)(a32 + b8)) >> 31)) == 0))


/*
 * Description:
 *  If sign of two 16-bits value is different macros will return true.
 *  Else false.
 */
#define IS_SIGN_DIFF_16(a16, b16) \
    (((uint16_t)a16 ^ (uint16_t)b16) & 0x8000)


#define ROUND_UP_2(v) \
    ({                \
        --v;          \
        v |= v >> 1;  \
        v |= v >> 2;  \
        v |= v >> 4;  \
        v |= v >> 8;  \
        v |= v >> 16; \
        ++v;          \
    })


#define S32_TO_U64(s32) \
    ((uint64_t)((int64_t)s32))

#endif /* _BITS_H */


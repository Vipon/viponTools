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

#ifndef __ENDIAN_H
#define __ENDIAN_H

#include <stdint.h>

#define __swap16(n) ((uint16_t) (         \
    (((uint16_t)(n) & 0xFF) << 8)      |  \
    (((uint16_t)(n) & 0xFF00) >> 8)))

#define __swap32(n) ((uint32_t) (         \
    (((uint32_t)(n) & 0xFF) << 24)      | \
    (((uint32_t)(n) & 0xFF00) << 8)     | \
    (((uint32_t)(n) & 0xFF0000) >> 8)   | \
    (((uint32_t)(n) & 0xFF000000) >> 24)))

#define __swap64(n) ((uint64_t) (                     \
    (((uint64_t)(n) & 0xff) << 56) |                  \
    ((uint64_t)(n) & 0xff00ULL) << 40 |               \
    ((uint64_t)(n) & 0xff0000ULL) << 24 |             \
    ((uint64_t)(n) & 0xff000000ULL) << 8 |            \
    ((uint64_t)(n) & 0xff00000000ULL) >> 8 |          \
    ((uint64_t)(n) & 0xff0000000000ULL) >> 24 |       \
    ((uint64_t)(n) & 0xff000000000000ULL) >> 40 |     \
    ((uint64_t)(n) & 0xff00000000000000ULL) >> 56))

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
# define __htobe16(n) __swap16(n)
# define __htobe32(n) __swap32(n)
# define __htobe64(n) __swap64(n)
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
# define __htobe16(n) (n)
# define __htobe32(n) (n)
# define __htobe64(n) (n)
#else
# error unsupported endianness
#endif

#define htobe16(n) __htobe16(n)
#define htobe32(n) __htobe32(n)
#define htobe64(n) __htobe64(n)

#define be16toh(n) __htobe16(n)
#define be32toh(n) __htobe32(n)
#define be64toh(n) __htobe64(n)

#endif /* __ENDIAN_H */


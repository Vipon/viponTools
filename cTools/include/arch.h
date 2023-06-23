/***
 * MIT License
 *
 * Copyright (c) 2021 Konychev Valera
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

#ifndef __ARCH_H
#define __ARCH_H

typedef enum {
    UNKNOWN_ARCH = -1,
    // x86 32-bit
    IA32 = 0,
    X86 = IA32,
    // x86 64-bit
    IA32E,
    X86_64 = IA32E,
    AMD64 = IA32E,
    // arm 32-bit
    ARM,
    // arm 64-bit
    ARM64,
    AARCH64 = ARM64
} Arch;

#if defined(__GNUC__) || defined(__clang__)
# if defined(i386) || defined(__i386) || defined(__i386__)
#  define IA32_DEFINED 1
#  define ARCH X86
# endif

# if defined(__amd64) || defined(__amd64__) || defined(__x86_64) || defined(__x86_64__)
#  define IA32_DEFINED 1
#  define IA32E_DEFINED 1
#  define AMD64_DEFINED 1
#  define X86_64_DEFINED 1
#  define ARCH X86_64
# endif

# if defined(__aarch64__)
#  define ARM64_DEFINED 1
#  define ARMV8_DEFINED 1
#  define AARCH64_DEFINED 1
#  define ARCH AARCH64
# endif

# if defined(__arm__)
#  define ARM32_DEFINED 1
#  define ARCH ARM
# endif
#else
# error Unknown compiler type
#endif

#endif /* __ARCH_H */


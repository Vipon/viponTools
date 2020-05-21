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

#ifndef __COMDEF_H
#define __COMDEF_H

#include <stdio.h>

#define PAGE_SIZE 4096

extern int VERBOSE;

#ifndef UNUSED
    #define UNUSED(expr) do { (void)(expr); } while (0)
#endif /* UNUSED */

#define RESET_COLOR_TEXT   "\033[0m"
#define SET_RED_COLOR_TEXT "\033[1;31m"
#define SET_YLW_COLOR_TEXT "\033[1;33m"
#define LOG(...)                            \
    do {                                    \
        if (VERBOSE) {                      \
            fprintf(stdout, __VA_ARGS__);   \
            fprintf(stdout, "\n");          \
            fflush(stdout);                 \
        }                                   \
    } while (0);

#define WARNING(...)                                                    \
    do {                                                                \
        fprintf(stderr, SET_YLW_COLOR_TEXT "WARNING: " RESET_COLOR_TEXT \
                        __VA_ARGS__);                                   \
        fprintf(stderr, "\n\t%s line %d\n", __FILE__, __LINE__);        \
        fprintf(stderr, "\n");                                          \
    } while (0);

#define ERROR(...)                                                    \
    do {                                                              \
        fprintf(stderr, SET_RED_COLOR_TEXT "ERROR: " RESET_COLOR_TEXT \
                        __VA_ARGS__);                                 \
        fprintf(stderr, "\n\t%s line %d\n", __FILE__, __LINE__);      \
        fprintf(stderr, "\n");                                        \
    } while (0);

#define PERROR(func)                                                \
    do {                                                            \
        perror(SET_RED_COLOR_TEXT "ERROR: " RESET_COLOR_TEXT func); \
        fprintf(stderr, "\t%s line %d\n", __FILE__, __LINE__);      \
        fprintf(stderr, "\n");                                      \
    } while (0);

#define TEST_EXEC_CODE(code) \
    {                        \
        code;                \
    }

#define STRINGIZE(s) #s
#define STR(s) STRINGIZE(s)

#define BIT_MASK(n) ((size_t)0x1 << n)
#define BIT(n, val) (val & BIT_MASK(n))

#define ROUND_UP(mul, val) (val + mul - (val % mul))

#ifdef __clang__
    #if __has_attribute(__always_inline__)
        #define INLINE inline __attribute__((__always_inline__))
    #else
        #define INLINE inline
    #endif
#elif defined(__GNUC__)
    #define INLINE inline __attribute__((__always_inline__))
#elif defined(_MSC_VER)
    #define INLINE __forceinline
#else
    #define INLINE inline
#endif

#endif /* __COMDEF_H */


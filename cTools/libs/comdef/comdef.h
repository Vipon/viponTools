/***
 * MIT License
 *
 * Copyright (c) 2020-2023 Konychev Valera
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

#include "os.h"
#include "arch.h"

#ifndef PAGE_SIZE
    #define PAGE_SIZE 4096
#endif /* PAGE_SIZE */

#ifdef COMDEF_SHARED_LIB
EXPORT_VAR
#else /* COMDEF_SHARED_LIB */
# ifndef STATIC_LIB
IMPORT_VAR
# endif /* STATIC_LIB*/
#endif /* COMDEF_SHARED_LIB */
extern int VERBOSE;

#ifndef UNUSED
    #define UNUSED(expr) \
        DEF_GUARD((void)(expr))
#endif /* UNUSED */

#define RESET_COLOR_TEXT   "\033[0m"
#define SET_RED_COLOR_TEXT "\033[1;31m"
#define SET_YLW_COLOR_TEXT "\033[1;33m"

#define DEF_GUARD(def)  \
    do {                \
        def;            \
    } while (0);

#ifndef __cplusplus
    #include <stdio.h>

    #define NEW_LINE       \
        DEF_GUARD(         \
            putchar('\n'); \
        )

    #define SPACE         \
        DEF_GUARD(        \
            putchar(' '); \
        )

    #define SPACEs(num)              \
        DEF_GUARD(                   \
            printf("%*s", num, " "); \
        )

    #define TAB            \
        DEF_GUARD(         \
            putchar('\t'); \
        )

    #define LOG(...)                            \
        DEF_GUARD(                              \
            if (VERBOSE) {                      \
                fprintf(stdout, __VA_ARGS__);   \
                fprintf(stdout, "\n");          \
                fflush(stdout);                 \
            }                                   \
        )
    #define WARNING(...)                                                    \
        DEF_GUARD(                                                          \
            fprintf(stderr, SET_YLW_COLOR_TEXT "WARNING: " RESET_COLOR_TEXT \
                            __VA_ARGS__);                                   \
            fprintf(stderr, "\n\t%s line %d\n", __FILE__, __LINE__);        \
            fprintf(stderr, "\n");                                          \
        )
    #define LOG_WARNING(...)                    \
        DEF_GUARD(                              \
            if (VERBOSE) {                      \
                WARNING(__VA_ARGS__);           \
            }                                   \
        )
    #define VT_ERROR(...)                                                 \
        DEF_GUARD(                                                        \
            fprintf(stderr, SET_RED_COLOR_TEXT "ERROR: " RESET_COLOR_TEXT \
                            __VA_ARGS__);                                 \
            fprintf(stderr, "\n\t%s line %d\n", __FILE__, __LINE__);      \
            fprintf(stderr, "\n");                                        \
        )
    #define LOG_ERROR(...)              \
        DEF_GUARD(                      \
            if (VERBOSE) {              \
                VT_ERROR(__VA_ARGS__);  \
            }                           \
        )
    #define PERROR(func)                                                \
        DEF_GUARD(                                                      \
            perror(SET_RED_COLOR_TEXT "ERROR: " RESET_COLOR_TEXT func); \
            fprintf(stderr, "\t%s line %d\n", __FILE__, __LINE__);      \
            fprintf(stderr, "\n");                                      \
        )
    #define STDERROR_PRINT(...)                                         \
        DEF_GUARD(                                                      \
            fprintf(stderr, __VA_ARGS__);                               \
        )
    #define STDERROR_LOG(...)                                           \
        DEF_GUARD(                                                      \
            if (VERBOSE) {                                              \
                fprintf(stderr, __VA_ARGS__);                           \
            }                                                           \
        )

#else // __cplusplus
    #include <iostream>

    #define NEW_LINE                \
        DEF_GUARD(                  \
            std::cout << std::endl; \
        )

    #define SPACE             \
        DEF_GUARD(            \
            std::cout << ' '; \
        )

    #define TAB                \
        DEF_GUARD(             \
            std::cout << '\t'; \
        )

    template<typename ...Args>
    void LOG(Args&& ...args)
    {
        if (VERBOSE) {
            (std::cout << ... << args);
            std::cout << std::endl;
        }
    }

    template<typename ...Args>
    void WARNING(Args&& ...args)
    {
        std::cerr << SET_YLW_COLOR_TEXT "WARNING: " RESET_COLOR_TEXT;
        (std::cerr << ... << args);
        std::cerr << std::endl;
        std::cerr << "\t" << __FILE__ << " line " << __LINE__ << std::endl;
    }

    template<typename ...Args>
    void VT_PERROR(Args&& ...args)
    {
        std::cerr << SET_RED_COLOR_TEXT "ERROR: " RESET_COLOR_TEXT;
        (std::cerr << ... << args);
        std::cerr << std::endl;
        std::cerr << "\t" << __FILE__ << " line " << __LINE__ << std::endl;
    }

    #define PERROR(func)                                                     \
        DEF_GUARD(                                                           \
            std::perror(SET_RED_COLOR_TEXT "ERROR: " RESET_COLOR_TEXT func); \
            std::cerr << "\t" << __FILE__ << " line " << __LINE__;           \
            std::cerr << std::endl;                                          \
        )

    template<typename ...Args>
    void STDERROR_PRINT(Args&& ...args)
    {
        (std::cerr << ... << args);
        std::cerr << std::endl;
        std::cerr << "\t" << __FILE__ << " line " << __LINE__ << std::endl;
    }

#endif // __cplusplus

#define EXEC_CODE(code) \
    DEF_GUARD(          \
        code;           \
    )

#ifdef DEBUG
    #define WARNING_DEBUG(...)      \
        {                           \
            WARNING(__VA_ARGS__)    \
        }
    #define ERROR_DEBUG(...)      \
        {                         \
            VT_ERROR(__VA_ARGS__) \
        }
    #define PERROR_DEBUG(func)  \
        {                       \
            PERROR(func)        \
        }
    #define STDERROR_PRINT_DEBUG(...)   \
        {                               \
            STDERROR_PRINT(__VA_ARGS__) \
        }
    #define EXEC_CODE_DEBUG(code)   \
        {                           \
            EXEC_CODE(code)         \
        }
#else
    #define WARNING_DEBUG(...)
    #define ERROR_DEBUG(...)
    #define PERROR_DEBUG(func)
    #define STDERROR_PRINT_DEBUG(...)
    #define EXEC_CODE_DEBUG(code)
#endif

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

#ifndef FALLTHROUGH
# define FALLTHROUGH __attribute__((fallthrough))
#endif /* FALLTHROUGH */

#if __has_attribute(packed)
# define PACKED __attribute__((packed))
#else
# define PACKED
#endif

#endif /* __COMDEF_H */


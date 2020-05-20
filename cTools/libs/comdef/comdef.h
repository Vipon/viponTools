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


#ifndef _STRING_H
#define _STRING_H

/* C standard headers */
#include <stddef.h>
#include <stdint.h>

/* OS standard headers */
#if defined(unix) || defined(__unix) || \
    defined(__unix__) || (defined(__APPLE__) && defined(__MACH__))
    #include <sys/types.h>
#else
    #error "*** ERROR: Unknown OS. ***"
#endif

/*
 * Description:
 *  Function is similar to strlen.
 */
size_t strlen(const char *addr);


int strcmp(const char *s1, const char *s2);


char *strncpy(char * __restrict dst, const char * __restrict src, size_t n);


/*
 * Description:
 *  Function copies data from src strig to dest string. Function expects null
 *  teminated scr string.
 * Output:
 *  Success:
 *      point to dest.
 *  Fail:
 *      NULL point.
 */
char *copyString(const char *src, char* dest);

#endif


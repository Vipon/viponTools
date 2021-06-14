#include "string.h"
#include "comdef.h"

/* C standard headers */
#include <stddef.h>
#include <stdint.h>

size_t strlen(const char *addr)
{
    if (addr == NULL) {
        STDERROR_PRINT_DEBUG("Invalid arguments.");
        return (size_t)-1;
    }

    size_t len = 0;
    while (addr[len] != '\0')
        ++len;

    return len;
}


int strcmp(const char *s1, const char *s2)
{
    while (*s1 == *s2++)
        if (*s1++ == 0)
            return (0);

    return (*(unsigned const char *)s1 - *(unsigned const char *)--s2);
}


char *strncpy(char * __restrict dst, const char * __restrict src, size_t n)
{
    if (n != 0) {
        register char *d = dst;
        register const char *s = src;

        do {
            if ((*d++ = *s++) == 0) {
                while (--n != 0)
                *d++ = 0;
                break;
            }
        } while (--n != 0);
    }

    return (dst);
}


char *copyString(const char *src, char* dest)
{
    if (src == NULL || dest == NULL) {
        STDERROR_PRINT_DEBUG("Invalid arguments.");
        return NULL;
    }

    size_t i = 0;
    while (src[i] != '\0') {
        dest[i] = src[i];
        ++i;
    }

    dest[i] = '\0';
    return &dest[i];
}


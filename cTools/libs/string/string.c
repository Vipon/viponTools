/***
 * MIT License
 *
 * Copyright (c) 2021 Konychev Valerii
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

#include "string.h"
#include "comdef.h"

/* C standard headers */
#include <stddef.h>

size_t strlen(const char *addr)
{
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

void *vt_memset(void *dst, int c, size_t n)
{
    uint8_t *p = (uint8_t*)dst;
    uint8_t v = (uint8_t)c;

    size_t i = 0;
    for (i = 0; i < n; ++i) {
        p[i] = v;
    }

    return dst;
}


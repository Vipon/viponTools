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


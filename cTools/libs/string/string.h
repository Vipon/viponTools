/***
 * MIT License
 *
 * Copyright (c) 2021-2023 Konychev Valerii
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

#include "os.h"

/* C standard headers */
#include <stddef.h>
#include <stdint.h>

/*
 * Description:
 *  Function is similar to strlen.
 */
EXPORT_FUNC
size_t strlen(const char *addr);

EXPORT_FUNC
int strcmp(const char *s1, const char *s2);

EXPORT_FUNC
char *strncpy(char * __restrict dst, const char * __restrict src, size_t n);

/***
 *  The strtoi64() function converts the initial part of the string in @str to
 *  an int64_t value according to the given base, which must be between 2 and 36
 *  inclusive, or be the special value 0.
 *
 *  The string may begin with an arbitrary amount of white space (as determined
 *  by isspace(3)) followed by a single optional '+' or '-' sign. If base is zero
 *  or 16, the string may then include a "0x" or "0X" prefix, and the number will
 *  be read in base 16; otherwise, a zero base is taken as 10 (decimal) unless
 *  the next character is '0', in which case it is taken as 8 (octal).
 *
 *  The remainder of the string is converted to a long value in the obvious
 *  manner, stopping at the first character which is not a valid digit in the
 *  given base. (In bases above 10, the letter 'A' in either uppercase or
 *  lowercase represents 10, 'B' represents 11, and so forth, with 'Z'
 *  representing 35.)
 *
 *  If @end is not NULL, strtoi64() stores the address of the first invalid
 *  character in @end. If there were no digits at all, strtoi64() stores the
 *  original value of @str in @end (and returns 0). In particular, if @str is
 *  not '\0' but @end is '\0' on return, the entire string is valid.
 *
 *  The strntoi64() function works just like the strtoi64() function, but looks
 *  only at first sz base.
 */
EXPORT_FUNC
int64_t strtoi64(const char *str, char **end, int base);
EXPORT_FUNC
int64_t strntoi64(const char *str, size_t sz, char **end, int base);

/***
 *  Description:
 *      The memcpy() function copies n bytes from memory area src to
 *      memory area dest. The memory areas must not overlap.
 *  Return:
 *      The memcpy() function returns a pointer to dest.
 */
EXPORT_FUNC
void *memcpy(void *dest, const void *src, size_t n);

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
EXPORT_FUNC
char *copyString(const char *src, char* dest);

#endif


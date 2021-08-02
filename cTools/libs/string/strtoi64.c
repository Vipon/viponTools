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
#include <ctype.h>
#include <stdbool.h>

int64_t strtoi64(const char *str, char **end, int base)
{
    if (str == NULL)
        return 0;

    int i = 0;
    bool neg = false;

    // skip spaces
    while (isspace(str[i]))
        ++i;

    // skip +/- sign
    if (str[i] == '-') {
        neg = true;
        ++i;
    } else if (str[i] == '+') {
        neg = false;
        ++i;
    }

    // Process base: 0, 8, 16
    if (base == 0 || base == 16) {
        if (str[i] == '0' && (str[i+1] == 'x' || str[i+1] == 'X')) {
            base = 16;
            i += 2;
        }
    }

    if (base == 0 || base == 8) {
        if (str[i] == '0') {
            base = 8;
            ++i;
        }
    }

    if (base == 0) {
        base = 10;
    }

    if (base < 2 || base > 36)
        return 0;

    int64_t res = 0;
    for (; ; ++i) {
        char c = str[i];
        if (isdigit(c))
            c -= '0';
        else if (isupper(c))
            c -= 'A' - 10;
        else if (islower(c))
            c -= 'a' - 10;
        else
            break; // Non-digit character

        if (c > base)
            break;

        res *= base;
        res += c;
    }

    if (neg)
        res = 0 - res;

    if (end != NULL)
        *end = (char*)((size_t)str + (size_t)i);

    return res;
}

int64_t strntoi64(const char *str, size_t num, char **end, int base)
{
    if (str == NULL || num > 66 || base > 36)
        return 0;

    char buf[67];
    memcpy(buf, str, num);
    buf[num] = '\0';
    int64_t ret = strtoi64(buf, end, base);
    if (end)
        *end = (char*)((size_t)str + (size_t)(*end - buf));

    return ret;
}


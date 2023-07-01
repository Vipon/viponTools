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

#include "test.h"
#include "string.h"
#include <stdint.h>
#include <inttypes.h>

static int zeroBase16(void)
{
    char str0[] = "0xDEADBEEF";
    int64_t val0 = 0xDEADBEEF;
    if (strntoi64(str0, sizeof(str0), NULL, 0) != val0)
        return -1;

    char str1[] = "-0xDEADBEEF";
    int64_t val1 = (int64_t)0 - (int64_t)0xDEADBEEF;
    if (strntoi64(str1, sizeof(str1), NULL, 0) != val1)
        return -1;

    return 0;
}

static int zeroBase10(void)
{
    char str[] = "1234567890";
    int64_t val = 1234567890;
    if (strntoi64(str, sizeof(str), NULL, 0) != val)
        return -1;

    char str1[] = "-1234567890";
    int64_t val1 = (int64_t)0 - (int64_t)1234567890;
    if (strntoi64(str1, sizeof(str1), NULL, 0) != val1)
        return -1;

    return 0;
}

static int zeroBase8(void)
{
    char str[] = "0744";
    int64_t val = 0744;
    if (strntoi64(str, sizeof(str), NULL, 0) != val)
        return -1;

    char str1[] = "-0744";
    int64_t val1 = (int64_t)0 - (int64_t)0744;
    if (strntoi64(str1, sizeof(str1), NULL, 0) != val1)
        return -1;

    return 0;
}

int main(void)
{
    EXPECT_FUNC_EQ(zeroBase16(), 0);
    EXPECT_FUNC_EQ(zeroBase10(), 0);
    EXPECT_FUNC_EQ(zeroBase8(), 0);
    return 0;
}


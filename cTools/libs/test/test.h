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

#ifndef __TEST_H
#define __TEST_H

#include "comdef.h"
#include "string.h"

#ifndef __cplusplus
    #include <stdlib.h>

    #define EXPECT_FUNC_EQ(func, res)          \
        if (func != res) {                     \
            VT_ERROR(STRINGIZE(func) " fail"); \
            exit(EXIT_FAILURE);                \
        }

    #define EXPECT_VAL_EQ(val, res, format)                                            \
        if (val != res) {                                                              \
            VT_ERROR(STRINGIZE(val) "- expect: " format " result: " format, res, val); \
            exit(EXIT_FAILURE);                                                        \
        }

    #define EXPECT_VAL_NOT_EQ(val, res, format) \
        if (val == res) {                       \
            VT_ERROR(format);                   \
            exit(EXIT_FAILURE);                 \
        }

    #define EXPECT_INT_EQ(val, res)   EXPECT_VAL_EQ(val, res, "%d")
    #define EXPECT_SIZET_EQ(val, res) EXPECT_VAL_EQ(val, res, "%zu")
    #define EXPECT_BOOL_EQ(val, res) EXPECT_VAL_EQ(val, res, "%d")

    #define EXPECT_STR_EQ(str0, str1)                        \
        if (strcmp(str0, str1)) {                            \
            VT_ERROR("expect: %s | result: %s", str0, str1); \
            exit(EXIT_FAILURE);                              \
        }

#else // __cplusplus
    #include <cstdlib>

    #define EXPECT_EQ(func, res)                  \
        if (func != res) {                        \
            VT_PERROR(func, " isn't equal ", res); \
            exit(EXIT_FAILURE);                   \
        }

    #define EXPECT_VAL_EQ(val, res, format)                                            \
        if (val != res) {                                                              \
            VT_PERROR(STRINGIZE(val) "- expect: " format " result: " format, res, val); \
            exit(EXIT_FAILURE);                                                        \
        }

    #define EXPECT_BOOL_EQ(val, res) EXPECT_VAL_EQ(val, res, "%d")
#endif // __cplusplus

#endif /* __TEST_H */


#ifndef __TEST_H
#define __TEST_H

#include "comdef.h"
#include <stdlib.h>

#define EXPECT_FUNC_EQ(func, res)       \
    if (func != res) {                  \
        ERROR(STRINGIZE(func) " fail"); \
        exit(EXIT_FAILURE);             \
    }

#define EXPECT_VAL_EQ(val, res, format)                                         \
    if (val != res) {                                                           \
        ERROR(STRINGIZE(val) "- expect: " format " result: " format, val, val); \
        exit(EXIT_FAILURE);                                                     \
    }

#define EXPECT_INT_EQ(val, res)   EXPECT_VAL_EQ(val, res, "%d")
#define EXPECT_SIZET_EQ(val, res) EXPECT_VAL_EQ(val, res, "%zu")


#endif /* __TEST_H */


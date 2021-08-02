#include "test.h"
#include "string.h"
#include <stdint.h>
#include <inttypes.h>

static int zeroBase16()
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

static int zeroBase10()
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

static int zeroBase8()
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


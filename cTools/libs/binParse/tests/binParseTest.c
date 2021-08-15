#include "foo.h"
#include "bar.h"
#include "test.h"
#include "binParse.h"
#include "pe64Printer.h"
#include "comdef.h"

static void hookFooWithBar(char *argv0)
{
    initBinParser(argv0);
    if (binParser.type == MACHO64)
        binHook(MACHO64_SYM_PREF "foo", bar);
    else
        binHook("foo", bar);
    finiBinParser();
}

int main(int argc, char *argv[])
{
    UNUSED(argc);

    VERBOSE = 1;

    foo();
    bar();
    hookFooWithBar(argv[0]);

    char *str1 = foo();
    char *str2 = bar();
    LOG("str1: %s", str1);
    LOG("str2: %s", str2);
    EXPECT_STR_EQ(str1, str2);

    return 0;
}


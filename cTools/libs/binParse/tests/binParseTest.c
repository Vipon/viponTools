#include "foo.h"
#include "bar.h"
#include "test.h"
#include "binParse.h"
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

    foo();
    bar();
    hookFooWithBar(argv[0]);

    char *str1 = foo();
    char *str2 = bar();
    EXPECT_STR_EQ(str1, str2);

    return 0;
}


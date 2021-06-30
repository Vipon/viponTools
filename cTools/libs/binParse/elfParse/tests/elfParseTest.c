#include "foo.h"
#include "bar.h"
#include "test.h"
#include "elf64Parse.h"
#include "comdef.h"

#ifdef __WIN__
# undef ERROR
# define ERROR(...)
#endif

static void hookFooWithBar(char *argv0)
{
    Elf64File *elf64 = elf64Parse(argv0);
    if (elf64 == NULL) {
        ERROR("Cannot parse %s", argv0);
        return;
    }

    elf64Hook(elf64, "foo", bar);
    elf64Free(elf64);
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


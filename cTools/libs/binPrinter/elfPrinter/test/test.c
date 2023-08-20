#include <stdio.h>

extern void test(void) __attribute__ ((section (".bar")));

void test(void)
{
    printf("Hello World\n");
}

int main()
{
    test();
    return 0;
}
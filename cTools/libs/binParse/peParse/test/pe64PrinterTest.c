#include "comdef.h"
#include "pe64Parse.h"
#include "pe64Printer.h"

void dummy(void) __attribute__ ((section (".MY_SECTION123")));
void dummy(void)
{

}

static void testPrintSections(const PE64File *pe)
{
    pe64PrintSections(pe);
}

int main(int argc, char *argv[])
{
    UNUSED(argc);
    PE64File *pe = pe64Parse(argv[0]);
    testPrintSections(pe);

    return 0;
}


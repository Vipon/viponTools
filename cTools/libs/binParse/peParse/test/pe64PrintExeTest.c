#include "comdef.h"
#include "pe64Parse.h"
#include "pe64Printer.h"

void dummy(void) __attribute__ ((section (".MY_SECTION123")));
void dummy(void)
{

}

int main(int argc, char *argv[])
{
    UNUSED(argc);
    PE64File *pe = pe64Parse(argv[0]);

    pe64PrintDosHeader(pe);
    pe64PrintNtHeader(pe);
    pe64PrintSections(pe);
    pe64PrintSymbols(pe);
    pe64PrintImports(pe);
    pe64PrintDelayImports(pe);

    pe64Free(pe);
    return 0;
}


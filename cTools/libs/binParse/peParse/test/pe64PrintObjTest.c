#include "test.h"
#include "pe64Parse.h"
#include "pe64Printer.h"

int main(int argc, char *argv[])
{
    if (argc < 2) {
        STDERROR_PRINT("There is no name of object file");
        exit(EXIT_FAILURE);
    }

    PE64File *pe = pe64Parse(argv[1]);
    EXPECT_VAL_NOT_EQ(pe, NULL, "Cannot parse pe64 file");
    EXPECT_VAL_NOT_EQ(pe->symtab, NULL, "Cannot parse pe64 symbol table");
    pe64PrintFileHeader(pe);
    pe64PrintSections(pe);
    pe64PrintSymbols(pe);

    pe64Free(pe);
    return 0;
}


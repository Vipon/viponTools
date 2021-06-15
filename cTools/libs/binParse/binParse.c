#include "binParse.h"
#include "elf64Parse.h"
#include "comdef.h"

BinParser binParser;

#define INIT_BIN_PARSER_FUNCS(type)                \
    binParser.binParse = (BinParse)&type ## Parse; \
    binParser.binFree = (BinFree)&type ## Free;    \
    binParser.binHook = (BinHook)&type ## Hook;

int initBinParser(const char *fn)
{
    void *binFile = NULL;
    if ((binFile = elf64Parse(fn)) != NULL) {
        binParser.type = ELF64;
        INIT_BIN_PARSER_FUNCS(elf64);
    } else {
        ERROR("Unknown FileType\n");
        return -1;
    }

    binParser.binFile = binFile;
    return 0;
}


void finiBinParser()
{
    binParser.binFree(binParser.binFile);
}


void *binHook(const char *func, const void *hand)
{
    return binParser.binHook(binParser.binFile, func, hand);
}


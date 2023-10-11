/***
 * MIT License
 *
 * Copyright (c) 2023 Konychev Valerii
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include "arch.h"
#include "args.h"
#include "comdef.h"
#include "string.h"
#include "binPrinter.h"

#include <stdlib.h>
#include <stdbool.h>

static Arch binParserArch = ARCH;

static const char doc[] =
    "Binary parser. Support binare format: mach-o/fat (64 bit), elf (64 bit), pe (64 bit)";
static const char argsDoc[] = "BIN_FILE";
static const char progVersion[] = "0.5.0";

typedef enum {
    HEADER = 0,
    SEGMENTS,
    SECTIONS,
    SYMBOLS,
    FAT_HEADER,
    FUNC_STARTS,
    LCOMS,
    CODE_SIGN,
    DOS_HEADER,
    FILE_HEADER,
    OPT_HEADER,
    IMPORTS,
    DELAY_IMPORTS,
    EXPORTS,
    RELOCATIONS,
    DYNAMIC,
    VERSION_INFO,
    NUM_FLAGS
} BinParserOpt;

static bool flags[NUM_FLAGS] = {};

static
void printHeader(const char *arg)
{
    UNUSED(arg);
    flags[HEADER] = true;
}

static
void printSymbols(const char *arg)
{
    UNUSED(arg);
    flags[SYMBOLS] = true;
}

static
void printSections(const char *arg)
{
    UNUSED(arg);
    flags[SECTIONS] = true;
}

static
void printSegments(const char *arg)
{
    UNUSED(arg);
    flags[SEGMENTS] = true;
}

static
void printFatHeader(const char *arg)
{
    UNUSED(arg);
    flags[FAT_HEADER] = true;
}

static
void printFuncStarts(const char *arg)
{
    UNUSED(arg);
    flags[FUNC_STARTS] = true;
}

static
void printLComs(const char *arg)
{
    UNUSED(arg);
    flags[LCOMS] = true;
}

static
void printCodeSign(const char *arg)
{
    UNUSED(arg);
    flags[CODE_SIGN] = true;
}

static
void printDosHeader(const char *arg)
{
    UNUSED(arg);
    flags[DOS_HEADER] = true;
}

static
void printFileHeader(const char *arg)
{
    UNUSED(arg);
    flags[FILE_HEADER] = true;
}

static
void printOptHeader(const char *arg)
{
    UNUSED(arg);
    flags[OPT_HEADER] = true;
}

static
void printImports(const char *arg)
{
    UNUSED(arg);
    flags[IMPORTS] = true;
}

static
void printDelayImports(const char *arg)
{
    UNUSED(arg);
    flags[DELAY_IMPORTS] = true;
}

static
void printExports(const char *arg)
{
    UNUSED(arg);
    flags[EXPORTS] = true;
}

static
void printRelocations(const char *arg)
{
    UNUSED(arg);
    flags[RELOCATIONS] = true;
}

static
void printDynamicSection(const char *arg)
{
    UNUSED(arg);
    flags[DYNAMIC] = true;
}

static
void printVersionInfo(const char *arg)
{
    UNUSED(arg);
    flags[VERSION_INFO] = true;
}

static
Arch getArchByName(const char* arch)
{
    if (strcmp(arch, "x86") == 0)
        return X86;
    if (strcmp(arch, "x86_64") == 0)
        return X86_64;
    if (strcmp(arch, "arm") == 0)
        return ARM;
    if (strcmp(arch, "aarch64") == 0)
        return AARCH64;

    return UNKNOWN_ARCH;
}

static
void setArch(const char *arg)
{
    binParserArch = getArchByName(arg);
}

static
void initParser(const char* fn, unsigned num)
{
    UNUSED(num);
    if (initBinPrinter(fn)) {
        LOG_ERROR("Unknown bin format");
        exit(EXIT_FAILURE);
    }
}

int main(int argc, char *argv[])
{
    ADD_DOC(doc);
    ADD_ARGS_DOC(argsDoc);
    ADD_VERSION(progVersion);
    SET_NUM_ARGS(1);
    SET_ARGS_HAND(initParser);

    ADD_ARG(printHeader, .name = "header"
                       , .key = 'h'
                       , .flags = OPTION_ARG_OPTIONAL
                       , .doc = "print all headers"
    );
    ADD_ARG(printSegments, .name = "segments"
                         , .key = 150
                         , .flags = OPTION_ARG_OPTIONAL
                         , .doc = "print all segments"
    );
    ADD_ARG(printSections, .name = "sections"
                         , .key = 'S'
                         , .flags = OPTION_ARG_OPTIONAL
                         , .doc = "print all section"
    );
    ADD_ARG(printSymbols, .name = "symbols"
                        , .key = 's'
                        , .flags = OPTION_ARG_OPTIONAL
                        , .doc = "print all symbols"
    );
    ADD_ARG(printRelocations, .name = "relocs"
                            , .key = 'r'
                            , .flags = OPTION_ARG_OPTIONAL
                            , .doc = "print relocations"
    );
    ADD_ARG(printFatHeader, .name = "fat-header"
                          , .key = 151
                          , .flags = OPTION_ARG_OPTIONAL
                          , .doc = "macho: print fat header information"
    );
    ADD_ARG(printFuncStarts, .name = "func-starts"
                           , .key = 152
                           , .flags = OPTION_ARG_OPTIONAL
                           , .doc = "macho: print information about func starts"
    );
    ADD_ARG(printLComs, .name = "lcom"
                      , .key = 'l'
                      , .flags = OPTION_ARG_OPTIONAL
                      , .doc = "macho: print load commands"
    );
    ADD_ARG(printCodeSign, .name = "code-sign"
                         , .key = 153
                         , .flags = OPTION_ARG_OPTIONAL
                         , .doc = "macho: print code sign"
    );
    ADD_ARG(setArch, .name = "mcpu"
                   , .key = 'm'
                   , .arg = "CPU_TYPE"
                   , .flags = OPTION_ARG_OPTIONAL
                   , .doc = "set up cpu type for parser"
    );
    ADD_ARG(printDosHeader, .name = "dos-header"
                          , .key = 154
                          , .flags = OPTION_ARG_OPTIONAL
                          , .doc = "pe: print dos header"
    );
    ADD_ARG(printFileHeader, .name = "file-header"
                           , .key = 155
                           , .flags = OPTION_ARG_OPTIONAL
                           , .doc = "pe: print file header"
    );
    ADD_ARG(printOptHeader, .name = "opt-header"
                          , .key = 156
                          , .flags = OPTION_ARG_OPTIONAL
                          , .doc = "pe: print opt header"
    );
    ADD_ARG(printImports, .name = "imports"
                          , .key = 'i'
                          , .flags = OPTION_ARG_OPTIONAL
                          , .doc = "pe: print imports"
    );
    ADD_ARG(printDelayImports, .name = "delay-imports"
                             , .key = 'd'
                             , .flags = OPTION_ARG_OPTIONAL
                             , .doc = "pe: print delay imports"
    );
    ADD_ARG(printExports, .name = "exports"
                        , .key = 'e'
                        , .flags = OPTION_ARG_OPTIONAL
                        , .doc = "pe: print exports"
    );
    ADD_ARG(printDynamicSection, .name = "dynamic"
                               , .key = 157
                               , .flags = OPTION_ARG_OPTIONAL
                               , .doc = "elf: print .dynamic section"
    );
    ADD_ARG(printVersionInfo, .name = "version-info"
                            , .key = 158
                            , .flags = OPTION_ARG_OPTIONAL
                            , .doc = "elf: print symbols version info from sections:"
                                     ".gnu.version, .gnu.version_r"
    );

    ARG_PARSE(argc, argv);
    setupBinPrinterArch(binParserArch);

    if (flags[HEADER])
        binPrinter.printHeader(binParser.bin);
    if (flags[SEGMENTS]) {
        binPrinter.printSegments(binParser.bin);
    }
    if (flags[SECTIONS]) {
        binPrinter.printSections(binParser.bin);
    }
    if (flags[SYMBOLS]) {
        binPrinter.printSymbols(binParser.bin);
    }
    if (flags[FAT_HEADER]) {
        binPrinter.fatMacho.printFatHeader(binParser.bin);
    }
    if (flags[FUNC_STARTS]) {
        binPrinter.macho.printFuncStarts(binParser.bin);
    }
    if (flags[LCOMS]) {
        binPrinter.macho.printLComs(binParser.bin);
    }
    if (flags[CODE_SIGN]) {
        binPrinter.macho.printCodeSign(binParser.bin);
    }
    if (flags[DOS_HEADER]) {
        binPrinter.pe.printDosHeader(binParser.bin);
    }
    if (flags[FILE_HEADER]) {
        binPrinter.pe.printFileHeader(binParser.bin);
    }
    if (flags[OPT_HEADER]) {
        binPrinter.pe.printOptHeader(binParser.bin);
    }
    if (flags[IMPORTS]) {
        binPrinter.pe.printImports(binParser.bin);
    }
    if (flags[DELAY_IMPORTS]) {
        binPrinter.pe.printDelayImports(binParser.bin);
    }
    if (flags[EXPORTS]) {
        binPrinter.pe.printExports(binParser.bin);
    }
    if (flags[RELOCATIONS]) {
        binPrinter.printRelocations(binParser.bin);
    }
    if (flags[DYNAMIC]) {
        binPrinter.elf.printDynamicSection(binParser.bin);
    }
    if (flags[VERSION_INFO]) {
        binPrinter.elf.printVersionInfo(binParser.bin);
    }

    finiBinPrinter();
}


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

#include "args.h"
#include "comdef.h"
#include "binPrinter.h"

#include <stdlib.h>
#include <stdbool.h>

static const char doc[] =
    "Binary parser. Support binare format: mach-o (64 bit), elf (32,64 bit), pe (64 bit)";
static const char argsDoc[] = "BIN_FILE";
static const char progVersion[] = "0.0.1";

typedef enum {
    HEADER = 0,
    SEGMENTS,
    SECTIONS,
    SYMBOLS,
    FUNC_STARTS,
    LCOMS,
    NUM_FLAGS
} BinParserOpt;

bool flags[NUM_FLAGS] = {};

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
void initParser(const char* fn, unsigned num)
{
    UNUSED(num);
    if (initBinPrinter(fn)) {
        ERROR("Unknown bin format");
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
    ADD_ARG(printFuncStarts, .name = "func-starts"
                           , .key = 'f'
                           , .flags = OPTION_ARG_OPTIONAL
                           , .doc = "macho: print information about func starts"
    );
    ADD_ARG(printLComs, .name = "lcom"
                      , .key = 'l'
                      , .flags = OPTION_ARG_OPTIONAL
                      , .doc = "macho: print load commands"
    );

    ARG_PARSE(argc, argv);

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
    if (flags[FUNC_STARTS]) {
        binPrinter.macho.printFuncStarts(binParser.bin);
    }
    if (flags[LCOMS]) {
        binPrinter.macho.printLComs(binParser.bin);
    }

    finiBinPrinter();
}


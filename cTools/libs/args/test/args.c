/***
 * MIT License
 *
 * Copyright (c) 2022 Konychev Valerii
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
#include "test.h"
#include "comdef.h"

#include <stdbool.h>

// Program documentation.
static char doc[] = "Test args library";

// A description of the arguments we accept.
static char argsDoc[] = "ARG0 ARG1";

bool isVerbose = false;

static void argVerbose(char *arg)
{
    UNUSED(arg);
    isVerbose = true;
}

int main(int argc, char **argv)
{
    ADD_DOC(doc);
    ADD_ARGS_DOC(argsDoc);
    ADD_ARG(argVerbose, "verbose", 'v', 0 , 0, "Produce verbose output");
    ARG_PARSE(argc, argv);

    EXPECT_BOOL_EQ(isVerbose, true);
    return 0;
}


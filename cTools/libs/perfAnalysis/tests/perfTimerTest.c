/***
 * MIT License
 *
 * Copyright (c) 2020-2021 Konychev Valera
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

// vipon headers
#include "os.h"
#include "perf.h"
#include "comdef.h"

// c standard headers
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

// os headers
#if defined(__UNIX__) || defined(__LINUX__) || defined(__MAC_OS_X__)
# include <unistd.h>
#elif defined(__WIN__)
# include <Windows.h>
# define getpid GetCurrentProcessId
#endif

int main(int argc, const char *argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    uint64_t curTsc = 0;
    uint64_t allTsc = 0;

    uint64_t i = 0;
    uint64_t numTest = 100;

    printf("#\tcycleTics\n");
    for (i = 0; i < numTest; ++i) {
        curTsc = getCPUCycles();
        getpid();
        curTsc = getCPUCycles() - curTsc;
        allTsc += curTsc;
        printf("%"PRIu64"\t%"PRIu64"\n", i, curTsc);
    }

    printf("\nAverage getpid() time: %"PRIu64"\n", allTsc / numTest);
    return 0;
}


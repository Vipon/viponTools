#include "perf.h"
#include "comdef.h"

#include <unistd.h>
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>


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


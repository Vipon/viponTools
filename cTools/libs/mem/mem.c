#include "comdef.h"
#include "mem.h"

// C standart headers
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

// OS standard headers
#if defined(unix) || defined(__unix) || defined(__unix__)
# include <unistd.h>
#else
# error "*** ERROR: Unknown OS. ***"
#endif


void Free(void *ptr)
{
    return free(ptr);
}


void *Malloc(size_t num)
{
    return malloc(num);
}


void *Calloc(size_t num, size_t elem_size)
{
    return calloc(num, elem_size);
}


void *Realloc(void *ptr, size_t size)
{
    return realloc(ptr, size);
}


void print_mem(uint8_t* mem, size_t size)
{
    size_t i = 0;
    for (i = 0; i < size; ++i) {
        printf("%x ", mem[i]);
    }

    putchar('\n');
}


long getPageSize(void)
{
    return sysconf(_SC_PAGESIZE);
}


size_t alignToPageSize(size_t val)
{
    long pageSize = getPageSize();
    if (pageSize == -1) {
        ERROR("getPageSize() fail");
        return (size_t)-1;
    }

    return val & ~((size_t)pageSize - 1);
}


size_t alignUpToPageSize(size_t val)
{
    long pageSize = getPageSize();
    if (pageSize == -1) {
        ERROR("getPageSize() fail");
        return (size_t)-1;
    }

    return (val + (size_t)pageSize) & ~((size_t)pageSize - 1);
}


uint8_t *directCopyBytes(const uint8_t *source, uint8_t *dest, size_t num)
{
    if (source == NULL || dest == NULL) {
        ERROR("Invalid arguments.");
        return NULL;
    }

    size_t i = 0;
    for (i = 0; i < num; ++i)
        dest[i] = source[i];

    return dest;
}


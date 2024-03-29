/***
 * MIT License
 *
 * Copyright (c) 2021-2024 Konychev Valera
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
#include "mem.h"
#include "comdef.h"

// C standart headers
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

void Free(void *ptr)
{
    free(ptr);
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

void printMem(const uint8_t* mem, size_t size)
{
    size_t i = 0;
    for (i = 0; i < size; ++i) {
        printf("%.2x ", mem[i]);
    }
}

// OS standard headers
#if defined(__UNIX__) || defined(__LINUX__) || defined(__MAC_OS_X__)
# include <unistd.h>

long getPageSize(void)
{
    return sysconf(_SC_PAGESIZE);
}

int vt_mprotect(void *addr, size_t len, int prot)
{
    return mprotect(addr, len, prot);
}

#elif defined(__WIN__)
# include <malloc.h>

long getPageSize(void)
{
    SYSTEM_INFO si;
    GetSystemInfo(&si);

    return (long)si.dwPageSize;
}

int vt_mprotect(void *addr, size_t len, int prot)
{
    DWORD oldProtect = 0;
    if (VirtualProtect(addr, len, (DWORD)prot, &oldProtect))
        return 0;
    else
        return -1;
}

#else
# error "*** ERROR: Unknown OS. ***"
#endif

size_t alignToPageSize(size_t val)
{
    long pageSize = getPageSize();
    if (pageSize == -1)
        return (size_t)-1;

    return val & ~((size_t)pageSize - 1);
}

size_t alignUpToPageSize(size_t val)
{
    long pageSize = getPageSize();
    if (pageSize == -1)
        return (size_t)-1;

    return (val + (size_t)pageSize) & ~((size_t)pageSize - 1);
}

uint8_t* directCopyBytes(const uint8_t *src, uint8_t *dst, size_t num)
{
    size_t i = 0;
    for (i = 0; i < num; ++i)
        dst[i] = src[i];

    return dst;
}

uint8_t* backwards_copy_bytes(const uint8_t *src, uint8_t *dst, size_t num)
{
    size_t i = num;
    while (i) {
        --i;
        dst[i] = src[i];
    }

    return dst;
}

uint8_t* copy_bytes(const uint8_t *src, uint8_t *dst, size_t num)
{
    /***
     * If destination address is bigger than source address, we should copy
     * from the last byte, because overlapping of address spaces is
     * possible.
     */
    if ((size_t)src >= (size_t)dst)
        return directCopyBytes(src, dst, num);
    else
        return backwards_copy_bytes(src, dst, num);
}

const char *getMProtStr(int prot)
{
    char *mprotStr[] = {
        ""      , // 000
        "r"     , // 001
        "w"     , // 010
        "wr"    , // 011
        "x"     , // 100
        "xr"    , // 101
        "xw"    , // 110
        "xwr"   , // 111
    };

    return mprotStr[prot];
}


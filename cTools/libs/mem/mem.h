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

#ifndef __MEM_H
#define __MEM_H

#include "os.h"

// C standard headers
#include <stdint.h>
#include <stddef.h>

EXPORT_FUNC
void Free(void *ptr);
EXPORT_FUNC
void *Malloc(size_t num);
EXPORT_FUNC
void *Calloc(size_t num, size_t elem_size);
EXPORT_FUNC
void *Realloc(void *ptr, size_t size);

EXPORT_FUNC
void print_mem(uint8_t* mem, size_t size);

/**
 * \brief Returns system page size
 * \return page size
 */
EXPORT_FUNC
long getPageSize(void);


/**
 * \brief Rounds to the nearest multiple page size value which is lower then val
 *
 * \param val value need to be aligned
 * \return aligned value
 */
EXPORT_FUNC
size_t alignToPageSize(size_t val);


/**
 * \brief Rounds to the nearest multiple page size value which is higher then val
 *
 * \param val value need to be aligned
 * \return aligned value
 */
EXPORT_FUNC
size_t alignUpToPageSize(size_t val);


/**
 * \brief Copies num bytes from the dest to the source buffer. Coping from a
 *        firs to a last byte.
 * \param source point to source buffer.
 * \param dest point to destination.
 * \param num number of bytes that need to copy.
 * \return Success: point to dest.
 *         Fail: NULL point.
 */
EXPORT_FUNC
uint8_t *directCopyBytes(const uint8_t *source, uint8_t *dest, size_t num);

/**
 * \brief Set memory protection.
 * \param addr point to memory should be align to page size.
 * \param len size of memory region.
 * \param prot protection flags.
 * \return Success: 0.
 *         Fail: error code.
 */
EXPORT_FUNC
int Mprotect(void *addr, size_t len, int prot);

EXPORT_FUNC
const char *getMProtStr(int prot);

#endif /* __MEM_H */


/***
 * MIT License
 *
 * Copyright (c) 2020 Konychev Valera
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

// C standard headers
#include <stdint.h>
#include <stddef.h>

void Free(void *ptr);
void *Malloc(size_t num);
void *Calloc(size_t num, size_t elem_size);
void *Realloc(void *ptr, size_t size);

void print_mem(uint8_t* mem, size_t size);

long getPageSize(void);


/**
 * \brief round to the nearest multiple page size value which is lower then val
 */
size_t alignToPageSize(size_t val);


/**
 * \brief round to the nearest multiple page size value which is higher then val
 */
size_t alignUpToPageSize(size_t val);


/**
 * Describe:
 *  Copy num bytes from the dest to the source buffer. Coping from a firs to a
 *  last byte.
 * Input:
 *  source - point to source buffer.
 *  dest - point to destination.
 *  num - number of bytes that need to copy.
 * Output:
 *  Success:
 *      point to dest.
 *  Fail:
 *      NULL point.
 */
uint8_t *directCopyBytes(const uint8_t *source, uint8_t *dest, size_t num);

#endif /* __MEM_H */


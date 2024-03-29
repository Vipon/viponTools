/***
 * MIT License
 *
 * Copyright (c) 2020-2024 Konychev Valera
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

#if defined(__UNIX__) || defined(__LINUX__) || defined(__MAC_OS_X__)
# include <sys/mman.h>

# define VT_PROT_READ      PROT_READ
# define VT_PROT_WRITE     PROT_WRITE
# define VT_PROT_EXEC      PROT_EXEC
#elif defined(__WIN__)
# include <Windows.h>

# define VT_PROT_READ      PAGE_READONLY
# define VT_PROT_WRITE     PAGE_READWRITE
# define VT_PROT_EXEC      PAGE_EXECUTE
#else
# error "*** ERROR: Unknown OS. ***"
#endif

EXPORT_FUNC
void Free(void *ptr);
EXPORT_FUNC
void *Malloc(size_t num);
EXPORT_FUNC
void *Calloc(size_t num, size_t elem_size);
EXPORT_FUNC
void *Realloc(void *ptr, size_t size);

EXPORT_FUNC
void printMem(const uint8_t* mem, size_t size);

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
 * \brief Copies num bytes from the \param dst to the \param src buffer. Coping
 * from a firs to a last byte.
 *
 * \param[in] src Point to source buffer.
 * \param[in] dst Point to destination.
 * \param[in] num Amount of bytes that need to copy.
 *
 * \return Point to \param dst.
 */
EXPORT_FUNC
uint8_t *directCopyBytes(const uint8_t *src, uint8_t *dst, size_t num);

/**
 * \brief Copy num bytes from the \param dst to the \param src buffer. Coping
 * from a last to a first byte.
 *
 * \param[in] src Point to source buffer.
 * \param[in] dst Point to destination.
 * \param[in] num Amount of bytes that need to copy.
 *
 * \return Point to \param dst.
 */
EXPORT_FUNC
uint8_t* backwards_copy_bytes(const uint8_t *src, uint8_t *dst, size_t num);

/**
 * \brief Copies \param num bytes from the \param src to the \param dst buffer.
 * Works in case of overlapping memory space.
 *
 * \param[in] src Point to source buffer.
 * \param[in] dst Point to destination.
 * \param[in] num Amount of bytes needed to copy.
 *
 * \return Point to \param dst
 */
EXPORT_FUNC
uint8_t* copy_bytes(const uint8_t *src, uint8_t *dst, size_t num);

/**
 * \brief Set memory protection.
 * \param[in] addr point to memory should be align to page size.
 * \param[in] len size of memory region.
 * \param[in] prot protection flags.
 * \return Success: 0.
 *         Fail: error code.
 */
EXPORT_FUNC
int vt_mprotect(void *addr, size_t len, int prot);

EXPORT_FUNC
const char *getMProtStr(int prot);

#endif /* __MEM_H */


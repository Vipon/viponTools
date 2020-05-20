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


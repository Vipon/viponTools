/***
 * MIT License
 *
 * Copyright (c) 2024 Konychev Valera
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

#ifndef __SORTED_VECTOR_H
#define __SORTED_VECTOR_H

#include "comdef.h"
#include "vector.h"

typedef struct {
    Vector v;
    int (*cmp)(const void *, const void *);
} Sorted_vector;

/**
 * \brief Initiates sorted vector.
 *
 * \param[in] sv Point to sorted vector needed to be initialized.
 * \param[in] capacity Capacity of initiated sorted vector.
 * \param[in] elem_size Size of type should be stored in the sorted vector.
 * \param[in] cmp Pointer to the comparison function, which should return 0,
 * negative or positive value if first argument equal, less ot greater than
 * second argument.
 *
 * \return 0 if success. -1 if fail.
 */
static inline int
sorted_vector_init(Sorted_vector *sv, size_t capacity, size_t elem_size,
    int (*cmp)(const void *, const void *))
{
    sv->cmp = cmp;
    return vector_init((Vector*)sv, capacity, elem_size);
}

/**
 * \brief Function finalizes sorted vector sotred at the pointer \param src.
 *
 * \param[in] sv Point to sorted vector needed to be finalized.
 */
static inline void
sorted_vector_fini(Sorted_vector *sv)
{
    vector_fini((Vector*)sv);
}

/**
 * \brief Change capacity of sorted vector.
 *
 * \param[in] sv Point to sorted vector.
 * \param[in] capacity New Capacity of sorted vector.
 *
 * \return 0 if success. -1 if fail.
 */
static inline int
sorted_vector_resize(Sorted_vector *sv, size_t capacity)
{
    return vector_resize((Vector*)sv, capacity);
}

EXPORT_FUNC void *
sorted_vector_find_elem(Sorted_vector *sv, const void* elem);

/**
 * \brief Insert new element in the sorted vector.
 *
 * \param[in] sv Point to sorted vector.
 * \param[in] elem New element of sorted vector.
 *
 * \return 0 if success. -1 if fail.
 */
EXPORT_FUNC int
sorted_vector_insert(Sorted_vector *sv, const void* elem);

#endif /* __SORTED_VECTOR_H */


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

#ifndef __VECTOR_H
#define __VECTOR_H

#include "os.h"
#include "comdef.h"
#include <stddef.h>
#include <stdint.h>

typedef struct {
    void *data;
    size_t  capacity;
    size_t  end;
    size_t  elem_size;
} Vector;

#define GET_PTR_VECTOR_ELEM(v, num) ((v)->data + (num) * (v)->elem_size)

/***
 * @brief
 * @param[in] v Pointer to the Vector.
 * @param[in] i Iteration pointer.
 * @param[in] code Body of code which is neded to execute.
 */
#define vector_for_each(v, i, code)                             \
    DEF_GUARD(                                                  \
        for ((i) = vector_begin(v); (void*)(i) < vector_end(v); \
            (i) = (void*)i + (v)->elem_size) {                  \
            code;                                               \
        }                                                       \
    );

#define vector_sort(v, cmp)                                  \
    DEF_GUARD(                                               \
        qsort((v)->data, (v)->end + 1, (v)->elem_size, cmp); \
    );

/**
 * \brief Initiates vector.
 *
 * \param[in] v Point to vector needed to be initialized.
 * \param[in] capacity Capacity of initiated vector.
 * \param[in] elem_size Size of type should be stored in the vector.
 *
 * \return 0 if success. -1 if fail.
 */
EXPORT_FUNC int
vector_init(Vector *v, size_t capacity, size_t elem_size);

/**
 * \brief Finalizes vector sotred at the pointer \param src.
 *
 * \param[in] v Point to vector needed to be finalized.
 */
EXPORT_FUNC void
vector_fini(Vector *v);

/**
 * \brief Returns pointer to the first element in vector.
 *
 * \param[in] v Point to vector.
 */
EXPORT_FUNC void *
vector_begin(Vector *v);

/**
 * \brief Returns pointer to the element after last.
 *
 * \param[in] v Point to vector.
 */
EXPORT_FUNC void *
vector_end(Vector *v);

/**
 * \brief Change capacity of vector.
 *
 * \param[in] v Point to vector.
 * \param[in] capacity New Capacity of vector.
 *
 * \return 0 if success. -1 if fail.
 */
EXPORT_FUNC int
vector_resize(Vector *v, size_t capacity);

/**
 * \brief Add new elem at the end of vector.
 *
 * \param[in] v Point to vector.
 * \param[in] elem Point to the elem needed to be added.
 *
 * \return 0 if success. -1 if fail.
 */
EXPORT_FUNC int
vector_push_back(Vector *v, const void* elem);

/**
 * \brief Return point to the last elem in the vector. Need to save element.
 *
 * \param[in] v Point to vector.
 *
 * \return NULL if fail.
 */
EXPORT_FUNC void *
vector_pop_back(Vector *v);

/**
 * \brief Add new elem at the end of vector.
 *
 * \param[in] v Point to vector.
 * \param[in] num Number of elem needed to change.
 * \param[in] elem Point to the elem needed to be added.
 *
 * \return 0 if success. -1 if fail.
 */
EXPORT_FUNC int
vector_set_elem(Vector *v, size_t num, const void *elem);

/**
 * \brief Return point to the last elem in the vector with number \param num.
 *
 * \param[in] v Point to vector.
 * \param[in] num Number of the elem need to return,
 *
 * \return NULL if fail.
 */
EXPORT_FUNC void *
vector_get_elem(Vector *v, size_t num);

EXPORT_FUNC
void *vector_find_elem(Vector *v, const void *elem,
    int (*cmp)(const void *, const void *));

#endif /* __VECTOR_H */


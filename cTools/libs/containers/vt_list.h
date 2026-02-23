/***
 * MIT License
 *
 * Copyright (c) 2024-2026 Konychev Valera
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

#ifndef __VT_LIST_H
#define __VT_LIST_H

#include <stdlib.h>

#include "comdef.h"

typedef struct vt_list_elem_t {
    void                  *data;
    struct vt_list_elem_t *next;
} vt_list_elem_t;

typedef struct vt_list_t {
    size_t         elem_size;
    vt_list_elem_t *first;
} vt_list_t ;

/***
 * @brief
 * @param[in] l Pointer to the vector.
 * @param[in] i Iterator pointer.
 */
#define VT_LIST_FOR_EACH(lp, i) \
    for (i = (lp)->first; i != NULL; i = i->next)

/**
 * \brief Initiates list.
 *
 * \param[in] l Point to list needed to be initialized.
 * \param[in] elem_size Size of type should be stored in the list.
 */
EXPORT_FUNC void
vt_list_init(vt_list_t *l, size_t elem_size);

/**
 * \brief Finalizes list at the pointer \param l.
 *
 * \param[in] l Point to list needed to be finalized.
 */
EXPORT_FUNC void
vt_list_fini(vt_list_t *l);

/**
 * \brief Add \param data at the begin of the list \param l.
 *
 * \param[in] l Point to list.
 * \param[in] data Point to data need to be added at the begin
 *
 * \return 0 if success. -1 if fail.
 */
EXPORT_FUNC int
vt_list_insert_at_begin(vt_list_t *l, const void *data);

/**
 * \brief Append \param data to the list \param l.
 *
 * \param[in] l Point to list.
 * \param[in] data Point to data need to be appended
 *
 * \return 0 if success. -1 if fail.
 */
EXPORT_FUNC int
vt_list_append(vt_list_t *l, const void *data);

/**
 * \brief Reverse list \param l.
 *
 * \param[in] l Point to the list.
 */
EXPORT_FUNC void
vt_list_reverse(vt_list_t *l);

#endif /* __VT_LIST_H */


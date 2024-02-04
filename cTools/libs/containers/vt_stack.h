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

#ifndef __VT_STACK_H
#define __VT_STACK_H

#include <stdbool.h>
#include "vt_vector.h"

typedef struct {
    vt_vector_t v;
} vt_stack_t;

/**
 * \brief Initiates stack.
 *
 * \param[in] sv Point to stack needed to be initialized.
 * \param[in] capacity Capacity of initiated stack.
 * \param[in] elem_size Size of type should be stored in the stack.
 *
 * \return 0 if success. -1 if fail.
 */
static inline int
vt_stack_init(vt_stack_t *s, size_t capacity, size_t elem_size)
{
    return vt_vector_init((vt_vector_t*)s, capacity, elem_size);
}

/**
 * \brief Function finalizes stack.
 *
 * \param[in] sv Point to stack needed to be finalized.
 */
static inline void
vt_stack_fini(vt_stack_t *s)
{
    vt_vector_fini((vt_vector_t*)s);
}

/**
 * \brief Change capacity of stack.
 *
 * \param[in] sv Point to stack.
 * \param[in] capacity New Capacity of stack.
 *
 * \return 0 if success. -1 if fail.
 */
static inline int
vt_stack_resize(vt_stack_t *s, size_t capacity)
{
    return vt_vector_resize((vt_vector_t*)s, capacity);
}

/**
 * \brief Push elem at the top of stack.
 *
 * \param[in] sv Point to stack.
 * \param[in] elem Elem needed to save at stack.
 *
 * \return 0 if success. -1 if fail.
 */
static inline int
vt_stack_push(vt_stack_t *s, const void* elem)
{
    return vt_vector_push_back((vt_vector_t*)s, elem);
}

/**
 * \brief Pop elem from the top of stack. Need to save element.
 *
 * \param[in] sv Point to stack.
 *
 * \return NULL if fail.
 */
static inline void *
vt_stack_pop(vt_stack_t *s)
{
    return vt_vector_pop_back((vt_vector_t*)s);
}

static inline bool
vt_stack_is_empty(const vt_stack_t *s)
{
    return (((const vt_vector_t*)(s))->end == 0);
}

#endif /* __VT_STACK_H */


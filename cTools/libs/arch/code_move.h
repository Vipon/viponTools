/***
 * MIT License
 *
 * Copyright (c) 2023 Konychev Valerii
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

#ifndef __CODE_MOVE_H
#define __CODE_MOVE_H

#include "comdef.h"
#include <stdint.h>

/* You could get description of code using the function get_strerr_code_move. */
#ifdef __WIN__
typedef enum : uint64_t {
#else
typedef enum {
#endif /* __WIN__ */
    CODE_MOVE_ERROR_OK = 0,
    CODE_MOVE_ERROR_BAD_ARG = (int64_t)0xFFFFFFFFFFFFFFF0,
    CODE_MOVE_ERROR_NO_MEM,
    CODE_MOVE_ERROR_BAD_DST,
    CODE_MOVE_ERROR_DISASM_INIT_ERROR,
    CODE_MOVE_ERROR_UNKNOWN_INSTR,
    CODE_MOVE_ERROR_UNKNOWN = (int64_t)0xFFFFFFFFFFFFFFFF
} CODE_MOVE_ERROR;

/***
 * @brief Function takes @p err and returns string that describe error.
 *        Always success.
 * @param[in] err Error number.
 * @return Pointer to a sring with description.
 */
EXPORT_FUNC
const char *get_code_move_err_str(CODE_MOVE_ERROR err);

#endif /* __CODE_MOVE_H */


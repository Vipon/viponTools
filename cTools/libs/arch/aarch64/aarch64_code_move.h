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

#ifndef __AARCH64_CODE_MOVE_H
#define __AARCH64_CODE_MOVE_H

#include "comdef.h"
#include "code_move.h"

#include <stdint.h>

/***
 * @brief Function takes aarch64 instruction from @p src, analyzes it with
 *        initial PC = @p old_pc, and moves to a new place (@p dst) with new
 *        PC = @p new_pc.
 * @param[in] src Pointer to an instruction needed to be moved.
 * @param[in] dst Destination pointer.
 * @param[in] old_pc source program counter.
 * @param[in] new_pc destination pragram counter.
 * @param[in] dst_size Size of destination buffer in bytes. If needs more
 *                     place, function will return CODE_MOVE_ERROR_BAD_DST.
 */
EXPORT_FUNC
CODE_MOVE_ERROR aarch64_instr_move( const uint8_t *src
                                  , uint8_t  *dst
                                  , uint64_t old_pc
                                  , uint64_t new_pc
                                  , uint64_t dst_size
                                  );

/***
 * @brief Function takes aarch64 binary code from @p src, analyzes it with
 *        initial PC = @p old_pc. After moves binary code to a new place
 *        (@p dst) with new PC = @p new_pc.
 * @param[in] src Pointer to a code needed to be moved.
 * @param[in] dst Destination pointer.
 * @param[in] old_pc source program counter.
 * @param[in] new_pc destination pragram counter.
 * @param[in] src_size Size of source binary code in bytes.
 * @param[in] dst_size Size of destination buffer in bytes. If needs more
 *                     place, function will return CODE_MOVE_ERROR_BAD_DST.
 */
EXPORT_FUNC
CODE_MOVE_ERROR aarch64_code_move( const uint8_t *src
                                 , uint8_t  *dst
                                 , uint64_t old_pc
                                 , uint64_t new_pc
                                 , uint64_t src_size
                                 , uint64_t dst_size
                                 );

/***
 * @brief Function takes aarch64 binary code from @p src, analyzes it with
 *        initial PC = @p old_pc. After return how many bytes will be needed to
 *        place new code.
 * @param[in] src Pointer to a code needed to be moved.
 * @param[in] old_pc source program counter.
 * @param[in] new_pc destination pragram counter. If equal (-1), function will
 *                   will return maximum estimation.
 * @param[in] src_size Size of source binary code in bytes.
 */
EXPORT_FUNC
CODE_MOVE_ERROR aarch64_estimate_space( const uint8_t *src
                                      , uint64_t old_pc
                                      , uint64_t new_pc
                                      , uint64_t src_size
                                      );

#endif /* __AARCH64_CODE_MOVE_H */


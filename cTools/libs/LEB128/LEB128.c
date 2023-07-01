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

#include "LEB128.h"
#include "comdef.h"

#include <stdint.h>
#include <stdlib.h>
#include <inttypes.h>


uint8_t* toULEB128(uint64_t v, uint8_t* buf, size_t size)
{
    if (buf == NULL) {
        LOG_ERROR("NULL pointer.");
        return NULL;
    }

    if (size == (size_t)-1) {
        LOG_ERROR("wrong size of buffer.");
        return NULL;
    }

    size_t i = 0;
    uint8_t bits7 = 0;

    do {
        if (i >= size) {
            LOG_ERROR("not enough space in buffer.");
            return NULL;
        }

        bits7 = (uint8_t)(v) & 0x7F;
        v >>= 7;

        if (v) {
            buf[i] = BIT_MASK(7) | bits7;
        } else {
            buf[i] = bits7;
        }

        LOG("%zu. 7BITS = 0x%.2"PRIX8
            " restVal = 0x%.16"PRIX64
            " buf[%zu] = 0x%.2"PRIX8, i, bits7, v, i, buf[i]);

        ++i;
    } while (v);

    return buf + i;
}


uint8_t *fromULEB128(uint8_t *p, uint64_t *res)
{
    if (p == NULL || res == NULL) {
        LOG_ERROR("NULL pointer.");
        return NULL;
    }

    size_t i = 0;
    uint8_t byte = 0;

    do {
        if (i >= sizeof(*res)) {
            LOG_ERROR("wrong ULEB128 record %"PRIu64".", *(uint64_t*)(void*)p);
            return NULL;
        }

        byte = p[i];
        *res |= (byte & (uint64_t)0x7F) << (i * 7);

        LOG("%zu. byte = 0x%.2"PRIX8" res = 0x%.16"PRIX64, i, byte, *res);

        ++i;
    } while (BIT(7, byte) != 0);

    return p + i;
}


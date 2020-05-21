#include "LEB128.h"
#include "comdef.h"

#include <stdint.h>
#include <stdlib.h>
#include <inttypes.h>


uint8_t* toULEB128(uint64_t v, uint8_t* buf, size_t size)
{
    if (buf == NULL) {
        ERROR("NULL pointer.");
        return NULL;
    }

    if (size == (size_t)-1) {
        ERROR("wrong size of buffer.");
        return NULL;
    }

    size_t i = 0;
    uint8_t bits7 = 0;

    do {
        if (i >= size) {
            ERROR("not enough space in buffer.");
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


uint8_t* fromULEB128(uint8_t* p, uint64_t* res)
{
    if (p == NULL || res == NULL) {
        ERROR("NULL pointer.");
        return NULL;
    }

    size_t i = 0;
    uint8_t byte = 0;

    do {
        if (i >= sizeof(*res)) {
            ERROR("wrong ULEB128 record %"PRIu64".", *(uint64_t*)p);
            return NULL;
        }

        byte = p[i];
        *res |= (byte & (size_t)0x7F) << (i * 7);

        LOG("%zu. byte = 0x%.2"PRIX8" res = 0x%.16"PRIX64, i, byte, *res);

        ++i;
    } while (BIT(7, byte) != 0);

    return p + i;
}

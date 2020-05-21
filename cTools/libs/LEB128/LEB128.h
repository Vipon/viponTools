#ifndef __LEB128_H
#define __LEB128_H

#include <stdint.h>
#include <stdlib.h>

uint8_t* toULEB128(uint64_t v, uint8_t* buf, size_t size);
uint8_t* fromULEB128(uint8_t* p, uint64_t* res);

#endif /* __LEB128_H */

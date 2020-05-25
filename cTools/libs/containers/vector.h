#ifndef __VECTOR_H
#define __VECTOR_H

#include <stddef.h>
#include <stdint.h>

typedef struct {
    uint8_t *data;
    size_t  capacity;
    size_t  end;
    size_t  elemSize;
} Vector;

#define forEachVector(v, func, ...)                                    \
    do {                                                               \
        uint8_t *p = NULL;                                             \
        for (p = beginVector(v); p < endVector(v); p += (v)->elemSize) { \
            func(p, __VA_ARGS__);                                      \
        }                                                              \
    } while(0);

int initVector(Vector *v, size_t capacity, size_t elemSize);

void freeVector(Vector *v);

uint8_t *beginVector(Vector *v);
uint8_t *endVector(Vector *v);

int resizeVector(Vector *v, size_t capacity);

int pushBackVector(Vector *v, const void* elem);

void *popBackVector(Vector *v);

int setElemVector(Vector *v, size_t num, const void *elem);

void *getElemVector(Vector *v, size_t num);


#endif /* __VECTOR_H */


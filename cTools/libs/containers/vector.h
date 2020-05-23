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

int vectorInit(Vector *v, size_t capacity, size_t elemSize);
void vectorFree(Vector *v);
int vectorPushBack(Vector *v, const void* elem);
void *vectorPopBack(Vector *v);
void *vectorGet(Vector *v, size_t num);

#endif /* __VECTOR_H */


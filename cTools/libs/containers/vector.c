#include "mem.h"
#include "bits.h"
#include "vector.h"
#include "comdef.h"

#include <stddef.h>

#define GET_PTR_VECTOR_ELEM(v, num) (v->data + num * v->elemSize)

int initVector(Vector *v, size_t capacity, size_t elemSize)
{
    if ((v->data = Malloc(capacity * elemSize)) == NULL) {
        ERROR("Cannot allocate memory");
        return -1;
    }

    v->capacity = capacity;
    v->end = 0;
    v->elemSize = elemSize;

    return 0;
}


void freeVector(Vector *v)
{
    Free(v->data);
    v->capacity = (size_t)-1;
    v->elemSize = (size_t)-1;
    v->end = (size_t)-1;
}


uint8_t *beginVector(Vector *v)
{
    return v->data;
}


uint8_t *endVector(Vector *v)
{
    return GET_PTR_VECTOR_ELEM(v, v->end);
}


int resizeVector(Vector *v, size_t capacity)
{
    v->capacity = capacity;
    void *data = Malloc(capacity * v->elemSize);
    if (data == NULL) {
        ERROR("Cannot allocate memory");
        return -1;
    }

    if (v->end > capacity) {
        v->end = capacity;
    }

    size_t num = v->end;
    directCopyBytes(v->data, data, num * v->elemSize);

    Free(v->data);
    v->data = data;

    return 0;
}


static int expandVector(Vector *v)
{
    v->capacity *= 2;
    void *data = Malloc(v->capacity * v->elemSize);
    if (data == NULL) {
        ERROR("Cannot allocate memory");
        return -1;
    }

    directCopyBytes(v->data, data, v->end * v->elemSize);
    Free(v->data);
    v->data = data;

    return 0;
}


int pushBackVector(Vector *v, const void *elem)
{
    if (v->capacity == v->end) {
        if (expandVector(v) == -1) {
            ERROR("Cannot expand Vector");
            return -1;
        }
    }

    directCopyBytes(elem, endVector(v), v->elemSize);
    ++v->end;

    return 0;
}


void *popBackVector(Vector *v)
{
    if (v->end == 0) {
        ERROR("Vector is empty");
        return NULL;
    }

    --v->end;
    return endVector(v);
}


int setElemVector(Vector *v, size_t num, const void *elem)
{
    if (num >= v->capacity) {
        if (resizeVector(v, ROUND_UP_2(num))) {
            ERROR("Cannot resize Vector");
            return -1;
        }
    }

    directCopyBytes(elem, v->data + (num * v->elemSize), v->elemSize);

    if (num > v->end) {
        v->end = num + 1;
    }

    return 0;
}


void *getElemVector(Vector *v, size_t num)
{
    if (num >= v->end) {
        return NULL;
    }

    return GET_PTR_VECTOR_ELEM(v, num);
}


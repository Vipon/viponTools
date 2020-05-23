#include "mem.h"
#include "vector.h"
#include "comdef.h"

#include <stddef.h>


int vectorInit(Vector *v, size_t capacity, size_t elemSize)
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


void vectorFree(Vector *v)
{
    Free(v->data);
    v->capacity = (size_t)-1;
    v->elemSize = (size_t)-1;
    v->end = (size_t)-1;
}


static int vectorExpand(Vector *v)
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


int vectorPushBack(Vector *v, const void *elem)
{
    if (v->capacity == v->end) {
        if (vectorExpand(v) == -1) {
            ERROR("Cannot expand Vector");
            return -1;
        }
    }

    directCopyBytes(elem, v->data + (v->end * v->elemSize), v->elemSize);
    ++v->end;

    return 0;
}


void *vectorPopBack(Vector *v)
{
    if (v->end == 0) {
        ERROR("Vector is empty");
        return NULL;
    }

    void *elem = v->data + (v->end * v->elemSize);
    --v->end;
    return elem;
}


void *vectorGet(Vector *v, size_t num)
{
    if (num >= v->end) {
        return NULL;
    }

    return v->data + (num * v->elemSize);
}



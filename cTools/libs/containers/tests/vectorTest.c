#include "comdef.h"
#include "vector.h"

#include <stddef.h>

int main(void)
{
    Vector v;

    const size_t capacity = 100;
    int def[capacity];

    vectorInit(&v, capacity, sizeof(int));

    size_t i = 0;
    for (i = 0; i < capacity; ++i) {
        def[i] = i;

        vectorPushBack(&v, &i);
    }


    for (i = 0; i < capacity; ++i) {
        int elem = *(int*)vectorGet(&v, i);
        if (def[i] != elem) {
            ERROR("Wrong value %zu: %d", i, elem);
            return -1;
        }
    }

    vectorFree(&v);
    return 0;
}


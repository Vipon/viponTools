#include "test.h"
#include "comdef.h"
#include "vector.h"

#include <stddef.h>


static int initVectorTest()
{
    Vector v;

    const size_t capacity = 100;
    const size_t elemSize = sizeof(int);
    if (initVector(&v, capacity, elemSize)) {
        return -1;
    }
    EXPECT_SIZET_EQ(v.capacity, capacity);
    EXPECT_SIZET_EQ(v.elemSize, elemSize);
    EXPECT_SIZET_EQ(v.end, 0);

    freeVector(&v);
    return 0;
}


static int pushBackVectorTest()
{
    Vector v;
    const size_t capacity = 100;
    initVector(&v, capacity, sizeof(int));

    size_t i = 0;
    for (i = 0; i < capacity; ++i) {
        pushBackVector(&v, &i);
    }

    unsigned *data = (unsigned*)v.data;
    for (i = 0; i < capacity; ++i) {
        EXPECT_INT_EQ(data[i], i);
    }

    freeVector(&v);
    return 0;
}


static int popBackVectorTest()
{
    Vector v;
    int i = 0;
    const int capacity = 100;

    initVector(&v, 1, sizeof(int));

    for (i = 0; i < capacity; ++i) {
        pushBackVector(&v, &i);
    }

    --i;
    for (; i >= 0  ; --i) {
        EXPECT_INT_EQ(*(int*) popBackVector(&v), i);
    }

    freeVector(&v);
    return 0;
}


static void addElemVector(void *p, int a)
{
    *(int *)p += a;
}


static int forEachVectorTest()
{
    Vector v;
    int i = 0;
    const int capacity = 100;

    initVector(&v, 1, sizeof(int));

    for (i = 0; i < capacity; ++i) {
        pushBackVector(&v, &i);
    }

    forEachVector(&v, addElemVector, 2);

    --i;
    for (; i >= 0  ; --i) {
        EXPECT_INT_EQ(*(int*) popBackVector(&v), i + 2);
    }

    freeVector(&v);
    return 0;
}


int main(void)
{
    EXPECT_FUNC_EQ(initVectorTest(), 0);
    EXPECT_FUNC_EQ(pushBackVectorTest(), 0);
    EXPECT_FUNC_EQ(popBackVectorTest(), 0);
    EXPECT_FUNC_EQ(forEachVectorTest(), 0);
    return 0;
}


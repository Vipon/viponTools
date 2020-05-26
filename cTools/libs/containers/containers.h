#ifndef __CONTEINERS_H
#define __CONTEINERS_H

#include "vector.h"
#include <stdint.h>

#if __STDC_VERSION__ >= 201112L
# define forEachContainer(c, func, ...) \
    _Generic((c),                       \
        Vector : forEachVector          \
    )(c, func, __VA_ARGS__)
#endif

#endif /* __CONTEINERS_H  */


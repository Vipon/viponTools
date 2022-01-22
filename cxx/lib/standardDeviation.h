/***
 * MIT License
 *
 * Copyright (c) 2022 Konychev Valera
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

#ifndef __STANDARD_DEVIATION_H
#define __STANDARD_DEVIATION_H

#include "setPoints.h"

#include <cmath>
#include <cstddef>
#include <algorithm>

template <typename Func, template<typename...> class Set, size_t N>
double standardDeviation(const Func& f, const Set<Point<N>>& set)
{
    double sum = 0;
    size_t num = set.size();
    for (const auto& elem: set) {
        Point<N-1> args;
        std::copy_n(elem.begin(), N-1, args.begin());
        double res = f(args) - elem[0];
        sum += res*res;
    }

    return sqrt(sum / num);
}

#endif /* __STANDARD_DEVIATION_H */


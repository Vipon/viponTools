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

#ifndef __LINEAR_FUNC_H
#define __LINEAR_FUNC_H

#include "point.h"

#include <cmath>
#include <cstddef>
#include <string>
#include <sstream>
#include <iostream>


class LinearFunc {
private:
    size_t dimension;
    Point k;

    LinearFunc();

    static double roundK(double k, unsigned precision)
    {
        double r = std::pow(10, precision);
        return std::round(k * r) / r;
    }

public:
    LinearFunc(const Point& k)
        : dimension(k.size())
        , k(k)
        {}

    double operator()(const Point& x) const
    {
        if (x.size() != dimension)
            throw "ERROR: misalign between point and function dimensions.";

        double sum = 0.0;
        for (size_t i = 0; i < dimension; ++i)
            sum += k[i]*x[i];

        return sum;
    }

    bool operator!=(const LinearFunc& f) const
    {
        return k != f.k;
    }

    friend std::ostream& operator<<(std::ostream& os, const LinearFunc& f)
    {
        bool allKZero = true;
        unsigned precision = os.precision();
        double k = roundK(f.k[0], precision);
        if (k != 0.0) {
            allKZero = false;
            os << k;
        }

        for (size_t i = 1; i < f.dimension; ++i) {
            k = roundK(f.k[i], precision);
            if (k != 0.0) {
                if (k > 0.0) {
                    if (!allKZero)
                        os << " + ";

                    if (k != 1)
                        os << k ;

                } else {
                    os << " - ";

                    if (k != 1)
                        os << std::abs(k);
                }

                allKZero = false;
                os << "x" << i;
            }
        }

        if (allKZero)
            os << 0.0;

        return os;
    }

    operator std::string() const
    {
        std::stringstream buffer;
        buffer << (*this);
        return buffer.str();
    }
};

#endif /* __LINEAR_FUNC_H */


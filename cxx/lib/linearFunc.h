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
#include <iostream>

class LinearFunc {
private:
    double x0;
    double k;

    LinearFunc();

public:
    LinearFunc(double x0, double k) : x0(x0), k(k) {}
    LinearFunc(const Point<2>& p) : x0(p[0]), k(p[1]) {}
    double operator()(double x) const { return x0 + k*x; }
    double operator()(const Point<1>& p) const { return x0 + k*p[0]; }

    friend std::ostream& operator<<(std::ostream& os, const LinearFunc& f)
    {
        double x0 = std::round(f.x0 * 1000) / 1000;
        double k = std::round(f.k * 1000) / 1000;
        os << x0;
        if (f.k > 0.0)
            os << " + " << k << "x";
        else if (f.k < 0.0)
            os << " - " << std::abs(k) << "x";
        return os;
    }
};

#endif /* __LINEAR_FUNC_H */


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

#ifndef __LINEAR_REGRESSION_H
#define __LINEAR_REGRESSION_H

#include "setPoints.h"
#include "linearFunc.h"

#include <cmath>
#include <cfloat>
#include <cstddef>
#include <iostream>
#include <algorithm>

namespace {

template<size_t N>
void normalizeSetPoints(SetPoints<N>& set)
{
    Point<N> max(set[0]);
    Point<N> min(set[0]);

    for (const auto& points: set) {
        size_t i = 0;
        for (const auto& point: points) {
            if (point > max[i])
                max[i] = point;

            if (point < min[i])
                min[i] = point;

            ++i;
        }
    }

    for (auto& points: set)
        for (size_t i = 0; i < N; ++i)
            points[i] = (points[i] - (max[i] + min[i])/2) / (max[i] - min[i]);
}

template<size_t N>
double calculateJ(const LinearFunc<N>& f, const SetPoints<N+1>& set)
{
    Point<N> x{};
    double J = 0;
    size_t numPoints = set.size();
    for (const auto& elem: set) {
        std::copy_n(elem.begin(), N, x.begin());
        double t_res = f(x) - elem[N];
        J += t_res * t_res;
    }

    J /= 2*numPoints;

    return J;
}

template<size_t N>
double derivativeJk0(const LinearFunc<N>& f, const SetPoints<N+1>& set)
{
    Point<N> x{};
    double dJ_dk0 = 0;
    size_t numPoints = set.size();
    for (const auto& elem: set) {
        std::copy_n(elem.begin(), N, x.begin());
        dJ_dk0 += f(x) - elem[N];
    }

    dJ_dk0 /= numPoints;

    return dJ_dk0;
}

template<size_t N>
double derivativeJkn(const LinearFunc<N>& f, const SetPoints<N+1>& set, size_t n)
{
    Point<N> x{};
    double dJ_dk = 0;
    size_t numPoints = set.size();
    for (const auto& elem: set) {
        std::copy_n(elem.begin(), N, x.begin());
        dJ_dk += (f(x) - elem[N]) * elem[n];
    }

    dJ_dk /= numPoints;

    return dJ_dk;
}

}

template<size_t N>
LinearFunc<N> linearRegression(const SetPoints<N+1>& set)
{
    // f(i) = x0 + k*x(i)
    // J = 1/2N * Sum((f(i) - f)^2)
    // x0(j) = x0(j) - a*(d(J) / dx0)
    // k(j) = k(j) - a*(d(J) / dk)

    SetPoints<N+1> normalSet(set);
    normalizeSetPoints(normalSet);

    Point<N+1> k{};
    double a = 0.01;
    double J = DBL_MAX;
    double J_old = J;
    LinearFunc<N> f(k);
    size_t i = 0;
    for (;;++i) {
        J = calculateJ(f, normalSet);
        k[0] -= a*derivativeJk0(f, normalSet);
        for (size_t i = 1; i < N+1; ++i)
            k[i] -= a*derivativeJkn(f, normalSet, i);

        if (J >= J_old)
            return LinearFunc<N>(k);

        f = LinearFunc<N>(k);
        J_old = J;
    }
}

#endif /* __LINEAR_REGRESSION */


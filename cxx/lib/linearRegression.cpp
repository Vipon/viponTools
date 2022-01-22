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

#include "linearFunc.h"
#include "linearRegression.h"

#include <cfloat>
#include <cstddef>
#include <iostream>

namespace {

double calculateJ(const LinearFunc& f, const SetPoints<2>& set)
{
    double J = 0;
    for (const auto& elem: set) {
        double t_res = f(elem[0]) - elem[1];
        J += t_res * t_res;
    }

    size_t num = set.size();
    J /= 2*num;

    return J;
}

double derivativeJx0(const LinearFunc& f, const SetPoints<2>& set)
{
    double dJ_dx0 = 0;
    for (const auto& elem: set) {
        dJ_dx0 += f(elem[0]) - elem[1];
    }

    size_t num = set.size();
    dJ_dx0 /= num;

    return dJ_dx0;
}

double derivativeJk(const LinearFunc& f, const SetPoints<2>& set)
{
    double dJ_dk = 0;
    for (const auto& elem: set) {
        dJ_dk += (f(elem[0]) - elem[1]) * elem[0];
    }

    size_t num = set.size();
    dJ_dk /= num;

    return dJ_dk;
}

}

LinearFunc linearRegression(const SetPoints<2>& set)
{
    // f(i) = x0 + k*x(i)
    // J = 1/2N * Sum((f(i) - f)^2)
    // x0(j) = x0(j) - a*(d(J) / dx0)
    // k(j) = k(j) - a*(d(J) / dk)

    double x0 = 0;
    double k = 0;
    double a = 0.01;
    double J = DBL_MAX;
    double J_old = J;
    LinearFunc f(x0, k);
    for (;;) {
        J = calculateJ(f, set);
        x0 = x0 - a*derivativeJx0(f, set);
        k = k - a*derivativeJk(f, set);

        if (J > J_old)
            return f;

        if (J == 0)
            return LinearFunc(x0, k);

        f = LinearFunc(x0, k);
        J_old = J;
    }
}


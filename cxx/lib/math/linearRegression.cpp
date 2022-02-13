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

#include "linearRegression.h"

#include <cmath>
#include <cfloat>
#include <cstddef>
#include <iostream>
#include <algorithm>

Matrix normalizeSetPoints(const Matrix& X)
{
    Matrix normlizeSet(X);
    size_t setNum = X.size().first;
    size_t dim = X.size().second;
    Point max(X[0]);
    Point min(X[0]);
    Point diff(X[0]);
    Point avrg(dim);

    for (const auto& points: X) {
        size_t i = 0;
        for (const auto& point: points) {
            if (point > max[i])
                max[i] = point;

            if (point < min[i])
                min[i] = point;

            avrg[i] += point;
            ++i;
        }
    }

    avrg /= setNum;
    diff = max - min;
    for (size_t i = 0; i < X.size().second; ++i) {
        if (diff[i] == 0) {
            diff[i] = 1;
            avrg[i] = 0;
        }
    }

    for (auto& point: normlizeSet) {
        point = (std::vector<double>)((Point(point) - avrg).to_all("/", diff));
    }

    return normlizeSet;
}

namespace {

double calculateJ(const Vector& theta, const Matrix& X, const Vector& y)
{
    Vector err = X * theta - y;
    double err2 = err.transpone() * err;

    size_t numPoints = X.size().first;
    double J = err2 / (2*numPoints);
    return J;
}

double derivativeJkn(const Vector& theta, const Matrix& X, const Vector& y, size_t n)
{
    Vector err = X * theta - y;
    size_t numPoints = X.size().first;
    return (err.transpone() * X.getColomn(n)) / numPoints;
}

}

LinearFunc linearRegression(const Matrix& X, const Vector& y)
{
    // f(i) = x0 + k*x(i)
    // J = 1/2N * Sum((f(i) - f)^2)
    // x0(j) = x0(j) - a*(d(J) / dx0)
    // k(j) = k(j) - a*(d(J) / dk)

    Vector theta(X.size().second);
    Vector dJ(X.size().second);
    /*
    Matrix normalX(normalizeSetPoints(X));
    Matrix normalY(normalizeSetPoints(y));
    */
    Matrix normalX(X);
    Vector normalY(y);

    double a = 0.01;
    double J = DBL_MAX;
    double J_old = J;

    size_t i = 0;
    for (;;++i) {
        J = calculateJ(theta, normalX, normalY);
        for (size_t i = 0; i < X.size().second; ++i)
            dJ[i] = derivativeJkn(theta, normalX, normalY, i);

        theta -= a * dJ;
        if (J >= J_old)
            return LinearFunc(theta.transpone());

        J_old = J;
    }
}


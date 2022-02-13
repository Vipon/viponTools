#include "linearRegression.h"

#include <cmath>
#include <cfloat>
#include <cstddef>
#include <iostream>
#include <algorithm>

namespace {

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

double calculateJ(const Matrix& theta, const Matrix& X, const Matrix& y)
{
    Matrix err = X * theta - y;
    Matrix err2 = err.transpone() * err;

    size_t numPoints = X.size().first;
    double J = err2[0][0] / (2*numPoints);
    return J;
}

double derivativeJkn(const Matrix& theta, const Matrix& X, const Matrix& y, size_t n)
{
    Matrix err = X * theta - y;
    size_t numPoints = X.size().first;
    return (err.transpone() * X.getColomn(n))[0][0] / numPoints;
}

}

LinearFunc linearRegression(const Matrix& X, const Matrix& y)
{
    // f(i) = x0 + k*x(i)
    // J = 1/2N * Sum((f(i) - f)^2)
    // x0(j) = x0(j) - a*(d(J) / dx0)
    // k(j) = k(j) - a*(d(J) / dk)

    Matrix theta(X.size().second, 1);
    Matrix dJ(X.size().second, 1);
    Matrix normalX(normalizeSetPoints(X));
    Matrix normalY(normalizeSetPoints(y));

    double a = 0.01;
    double J = DBL_MAX;
    double J_old = J;

    size_t i = 0;
    for (;;++i) {
        J = calculateJ(theta, normalX, normalY);
        for (size_t i = 0; i < X.size().second; ++i)
            dJ[i][0] = derivativeJkn(theta, normalX, normalY, i);

        theta -= a * dJ;
        if (J >= J_old)
            return LinearFunc(theta.transpone()[0]);

        J_old = J;
    }
}


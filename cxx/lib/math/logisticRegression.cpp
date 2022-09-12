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

#include "log.h"
#include "matrix.h"
#include "sigmoid.h"
#include "cxxVector.h"
#include "gradDescent.h"
#include "logisticRegression.h"

#include <cmath>
#include <cstddef>

namespace LogisticRegression
{

std::pair<double, Vector>
costFunc(const Matrix& X, const Vector& y, const Vector& theta, double lambda)
{
    size_t m = y.size();
    Matrix XTheta = X*theta;
    Matrix j = (y - 1).to_all("*", log(1 - sigmoid(XTheta)))
             - y.to_all("*", log(sigmoid(XTheta)));
    double J = j.sum() / (double)m;

    Vector grad(theta.size());
    Vector err = sigmoid(XTheta) - y;
    for (size_t i = 0; i < theta.size(); ++i) {
        double errXSum = err.transpone() * X.getColomn(i);
        grad[i] = errXSum / (double)m;
    }

    if (lambda > 0.0) {
        // Regularized
        Vector regTheta(theta);
        regTheta[0] = 0;
        J = J + (lambda/(double)(2*m)) * (regTheta.transpone() * regTheta);

        Vector gradReg = lambda/((double)m) * regTheta;
        grad = grad + gradReg;
    }

    return std::pair<double, Vector>(J, grad);
}

} // LogisticRegression

std::pair<double, Vector>
logisticRegression(
    const Matrix& X,
    const Vector& y,
    const Vector& initTheta,
    size_t numIter,
    double regParam,
    double alpha
)
{
    return gradDescent(
        LogisticRegression::costFunc,
        X,
        y,
        initTheta,
        numIter,
        regParam,
        alpha
    );
}


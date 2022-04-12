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

#include "test.h"
#include "round.h"
#include "matrix.h"
#include "cxxVector.h"
#include "logisticRegression.h"

int main()
{
    Matrix X(Matrix::load("exam_scores.txt"));
    //std::cout << X << std::endl;
    X = (Matrix::ones(X.size().first, 1)).concat(X);
    //std::cout << X << std::endl;
    Vector y(Matrix::load("pass_list.txt"));
    //std::cout << y << std::endl;

    // zero theta
    Vector theta(3);
    Vector expectGrad(
        std::vector<double>{ -0.1, -12.0092, -11.2628 }
    );

    std::pair<double, Vector> res = LogisticRegression::costFunc(X, y, theta);
    EXPECT_EQ(round(res.first, 6), 0.693147);
    EXPECT_EQ((std::string)res.second, (std::string)expectGrad);

    theta = std::vector<double>{-24, 0.2, 0.2};
    expectGrad = std::vector<double>{0.0429033, 2.56625, 2.64683};
    res = LogisticRegression::costFunc(X, y, theta);
    EXPECT_EQ(round(res.first, 3), 0.218);
    EXPECT_EQ((std::string)res.second, (std::string)expectGrad);

    theta = std::vector<double>{-25, 0.15, 0.15};
    Vector expectTheta(
        std::vector<double>{ -24.9993, 0.204935, 0.20016 }
    );
    res = logisticRegression(X, y, theta, 400, 0, 0.001);
    EXPECT_EQ(round(res.first, 4), 0.2035);
    EXPECT_EQ((std::string)res.second, (std::string)expectTheta);
    return 0;
}


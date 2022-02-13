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
#include "matrix.h"

#include <array>
#include <iostream>

using namespace std;

int main()
{
    Matrix A(
        {
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        }
    );
    Matrix trA (
        {
            {1, 4, 7},
            {2, 5, 8},
            {3, 6, 9}
        }
    );
    EXPECT_EQ(A.transpone(), trA);
    EXPECT_EQ(A.determinant(), 0);

    Matrix B(
        {
            {9, 8, 7},
            {6, 5, 4},
            {3, 2, 1}
        }
    );
    Matrix AplusB(
        {
            { 10, 10, 10 },
            { 10, 10, 10 },
            { 10, 10, 10 },
        }
    );
    Matrix AminusB(
        {
            { -8, -6, -4 },
            { -2,  0,  2 },
            {  4,  6,  8 },
        }
    );
    Matrix AmulB(
        {
            { 30, 24, 18 },
            { 84, 69, 54 },
            { 138, 114, 90 },
        }
    );
    EXPECT_EQ(A + B, AplusB);
    EXPECT_EQ(A - B, AminusB);
    EXPECT_EQ(A * B, AmulB);;

    Matrix C(
        {
            {1, 2, 0},
            {2, 1, 0},
            {0, 0, 1}
        }
    );
    Matrix invC(
        {
            { -1.0/3.0,  2.0/3.0, -0 },
            {  2.0/3.0, -1.0/3.0,  0 },
            {  0,        0,        1 }
        }
    );
    EXPECT_EQ(C.inv(), invC);

    Matrix D(
        {
            {1, 1, 1},
            {2, 2, 3},
        }
    );
    Matrix pInvD(
        {
            {1.5, -0.5},
            {1.5, -0.5},
            {-2, 1}
        }
    );
    EXPECT_EQ(D.pinv(), pInvD);

    return 0;
}


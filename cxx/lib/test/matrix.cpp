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

    Matrix B(
        {
            {9, 8, 7},
            {6, 5, 4},
            {3, 2, 1}
        }
    );

    cout << "Matrix A:\n" << A << "\n";
    cout << "Matrix B:\n" << B << "\n";
    cout << "Transpone A\n" << A.transpone() << "\n";
    cout << "Determinant A: " << A.determinant() << "\n";

    cout << "Matrix A + B:\n" << A + B << "\n";
    cout << "Matrix A - B:\n" << A - B << "\n";
    cout << "Matrix A * B:\n" << A * B << "\n";

    Matrix C(
        {
            {1, 2, 0},
            {2, 1, 0},
            {0, 0, 1}
        }
    );
    cout << "Matrix C:\n" << C << "\n";
    cout << "Inverse C\n" << C.inv() << "\n";

    Matrix D(
        {
            {1, 1, 1},
            {2, 2, 3},
        }
    );
    cout << "Matrix D:\n" << D << "\n";
    cout << "Pseudo Inverse D\n" << D.pinv() << "\n";
    return 0;
}


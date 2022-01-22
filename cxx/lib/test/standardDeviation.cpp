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

#include "setPoints.h"
#include "linearFunc.h"
#include "standardDeviation.h"

#include <iostream>

int main()
{
    SetPoints<2> testSet0 = {
        Point<2>({1.0, 1.0}),
        Point<2>({2.0, 2.0}),
        Point<2>({3.0, 3.0})
    };

    LinearFunc f0(0, 1);
    std::cout << "deviation:" << standardDeviation(f0, testSet0) << "\n";

    LinearFunc f1(0, 0.0);
    std::cout << "deviation:" << standardDeviation(f1, testSet0) << "\n";

    LinearFunc f2(0, 0.5);
    std::cout << "deviation:" << standardDeviation(f2, testSet0) << "\n";

    return 0;
}


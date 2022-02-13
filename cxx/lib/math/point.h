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

#ifndef __POINT_H
#define __POINT_H

#include "matrix.h"

#include <vector>
#include <cstddef>
#include <iostream>

class Point : public Matrix {
public:
    Point(size_t N) : Matrix(1, N) { }
    Point(const Point& B) : Matrix(B) { }
    Point(const Matrix& B)
    {
        if (B.size().first > 1)
            throw "Matrix cannot be turn into Point.";

        (*(Matrix*)this) = B;
    }
    Point(const std::vector<double>& v)
    {
        std::vector<std::vector<double>> m;
        m.push_back(v);
        *((Matrix*)this) = Matrix(m);
    }

    Point to_all(const std::string& op, const Point& B) const
    {
        return (*(Matrix*)this).to_all(op, B);
    }

    size_t size() const
    {
        return (*(Matrix*)this).size().second;
    }

    double& operator[](size_t n)
    {
        return (*(Matrix*)this)[0][n];
    }

    double operator[](size_t n) const
    {
        return (*(Matrix*)this)[0][n];
    }

    operator std::vector<double>() const
    {
        return (*((Matrix*)this))[0];
    }

    Point& operator=(Point&& B)
    {
        (*(Matrix*)this) = std::move(B);

        return *this;
    }

    Point& operator=(const Point& B)
    {
        (*(Matrix*)this) = B;

        return *this;
    }

    Point& operator+=(const Point& B)
    {
        if (size() != B.size())
            throw "ERROR: vectors sizes are different";

        (*(Matrix*)this) += B;

        return *this;
    }

    Point& operator+=(double k)
    {
        (*(Matrix*)this) += k;

        return *this;
    }

    Point operator+(const Point& B) const
    {
        Point A(*this);
        A += B;

        return A;
    }

    Point operator+(double k) const
    {
        Point A(*this);
        A += k;

        return A;
    }

    friend Point operator+(double k, const Point& M)
    {
        Point A(M);
        A += k;

        return A;
    }

    Point& operator-=(const Point& B)
    {
        if (size() != B.size())
            throw "ERROR: vectors sizes are different";

        (*(Matrix*)this) -= B;
        return *this;
    }

    Point& operator-=(double k)
    {
        (*(Matrix*)this) -= k;

        return *this;
    }

    Point operator-(const Point& B) const
    {
        Point A(*this);
        A -= B;

        return A;
    }

    Point operator-(double k) const
    {
        Point A(*this);
        A -= k;

        return A;
    }

    friend Point operator-(double k, const Point& M)
    {
        Point A(M);
        A -= k;
        A *= -1;

        return A;
    }

    Point& operator*=(double k)
    {
        (*(Matrix*)this) *= k;

        return *this;
    }

    Point operator*(double k) const
    {
        return Point((*(Matrix*)this) * k);
    }

    friend Point operator*(double k, const Point& M)
    {
        return Point((Matrix&)M * k);
    }

    Point& operator/=(double k)
    {
        (*(Matrix*)this) /= k;

        return (*this);
    }

    Point operator/(double k) const
    {
        return Point((*(Matrix*)this) / k);
    }

    bool operator!=(const Point& B) const
    {
        return (*(Matrix*)this) != B;
    }

};

#endif /* __POINT_H */


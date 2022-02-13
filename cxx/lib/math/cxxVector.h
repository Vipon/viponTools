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

#ifndef __CXX_VECTOR_H
#define __CXX_VECTOR_H

#include "matrix.h"
#include "point.h"

#include <vector>
#include <cstddef>
#include <iostream>

class Point;

class Vector : public Matrix {
public:
    Vector(size_t N) : Matrix(N, 1) { }
    Vector(const Vector& B) : Matrix(B) { }
    Vector(const Matrix& B)
    {
        if (B.size().second > 1)
            throw "Matrix cannot be turn into Vector.";

        (*(Matrix*)this) = B;
    }
    Vector(const std::vector<double>& v)
    {
        std::vector<std::vector<double>> m;
        for (const auto& elem: v) {
            std::vector<double> row;
            row.push_back(elem);
            m.push_back(row);
        }

        *((Matrix*)this) = Matrix(m);
    }

    Point transpone() const;

    Vector to_all(const std::string& op, const Vector& B) const
    {
        return (*(Matrix*)this).to_all(op, B);
    }

    size_t size() const
    {
        return (*(Matrix*)this).size().first;
    }

    double& operator[](size_t n)
    {
        return (*(Matrix*)this)[n][0];
    }

    double operator[](size_t n) const
    {
        return (*(Matrix*)this)[n][0];
    }

    Vector& operator=(Vector&& B)
    {
        (*(Matrix*)this) = std::move(B);

        return *this;
    }

    Vector& operator=(const Vector& B)
    {
        (*(Matrix*)this) = B;

        return *this;
    }

    Vector& operator+=(const Vector& B)
    {
        if (size() != B.size())
            throw "ERROR: vectors sizes are different";

        (*(Matrix*)this) += B;

        return *this;
    }

    Vector& operator+=(double k)
    {
        (*(Matrix*)this) += k;

        return *this;
    }

    Vector operator+(const Vector& B) const
    {
        Vector A(*this);
        A += B;

        return A;
    }

    Vector operator+(double k) const
    {
        Vector A(*this);
        A += k;

        return A;
    }

    friend Vector operator+(double k, const Vector& M)
    {
        Vector A(M);
        A += k;

        return A;
    }

    Vector& operator-=(const Vector& B)
    {
        if (size() != B.size())
            throw "ERROR: vectors sizes are different";

        (*(Matrix*)this) -= B;
        return *this;
    }

    Vector& operator-=(double k)
    {
        (*(Matrix*)this) -= k;

        return *this;
    }

    Vector operator-(const Vector& B) const
    {
        Vector A(*this);
        A -= B;

        return A;
    }

    Vector operator-(double k) const
    {
        Vector A(*this);
        A -= k;

        return A;
    }

    friend Vector operator-(double k, const Vector& M)
    {
        Vector A(M);
        A -= k;
        A *= -1;

        return A;
    }

    Vector& operator*=(double k)
    {
        (*(Matrix*)this) *= k;

        return *this;
    }

    Matrix operator*(const Point& p) const;

    Vector operator*(double k) const
    {
        return Vector((*(Matrix*)this) * k);
    }

    friend Vector operator*(double k, const Vector& M)
    {
        return Vector((Matrix&)M * k);
    }

    Vector& operator/=(double k)
    {
        (*(Matrix*)this) /= k;

        return (*this);
    }

    Vector operator/(double k) const
    {
        return Vector((*(Matrix*)this) / k);
    }

    bool operator!=(const Vector& B) const
    {
        return (*(Matrix*)this) != B;
    }

};

#endif // __CXX_VECTOR_H


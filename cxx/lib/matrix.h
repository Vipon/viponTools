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

#ifndef __MATRIX_H
#define __MATRIX_H

#include <cmath>
#include <vector>
#include <cstddef>
#include <ostream>

class Matrix {
private:
    size_t N;
    size_t M;
    std::vector<std::vector<double>> m;

public:
    Matrix() {} // Create Null-Matrix NxM
    Matrix(const std::vector<std::vector<double>>& m)
        : N(m.size())
        , M(m[0].size())
        , m(m)
        {}

    Matrix(size_t N, size_t M)
        : N(N), M(M)
    {
        m.resize(N);
        for (size_t i = 0; i < N; ++i)
            m[i].resize(M, 0);
    }

    Matrix(size_t N)
    {
        Matrix A(N, N);
        *this = std::move(A);
    }

    Matrix(const double** A, size_t N, size_t M)
        : N(N), M(M)
    {
        m.resize(N);
        for (size_t i = 0; i < N; ++i)
            for (size_t j = 0; j < M; ++j)
                m[i].push_back(A[i][j]);
    }

    Matrix(const Matrix& B)
        : N(B.N)
        , M(B.M)
        , m(B.m)
        {}

    Matrix(Matrix&& B)
        : N(B.N)
        , M(B.M)
        , m(std::move(B.m))
        {}

    Matrix transpone() const
    {
        Matrix A(M, N);
        for (size_t i = 0; i < M; ++i)
            for (size_t j = 0; j < N; ++j)
                A.m[i][j] = m[j][i];

        return A;
    }

    Matrix dropRow(size_t n) const
    {
        Matrix A(N-1, M);
        for (size_t i = 0, j = 0; i < N; ++i)
            if (i != n)
                A.m[j++] = m[i];

        return A;
    }

    Matrix dropColomn(size_t m) const
    {
        Matrix A(N, M-1);
        for (size_t i = 0; i < N; ++i)
            for (size_t j = 0, k = 0; j < M; ++j)
                if (j != m)
                    A.m[i][k++] = this->m[i][j];

        return A;
    }

    double minor(size_t i, size_t j) const
    {
        return dropColomn(j).dropRow(i).determinant();
    }

    double determinant() const
    {
        if (N != M)
            throw "ERROR: not rectangle matrix";

        if (N == 1)
            return m[0][0];

        double res = 0;
        for (size_t i = 0; i < N; ++i)
            res += std::pow(-1, i) * m[i][0] * minor(i, 0);

        return res;
    }

    Matrix& operator=(Matrix&& B)
    {
        N = B.N;
        M = B.M;
        m = std::move(B.m);

        return *this;
    }

    Matrix& operator+=(const Matrix& B)
    {
        if (N != B.N || M != B.M)
            throw "ERROR: not equal matrix dimensions";

        for (size_t i = 0; i < N; ++i)
            for (size_t j = 0; j < M; ++j)
                m[i][j] += B.m[i][j];

        return *this;
    }

    Matrix operator+(const Matrix& B) const
    {
        Matrix A(*this);
        A += B;

        return A;
    }

    Matrix& operator-=(const Matrix& B)
    {
        if (N != B.N || M != B.M)
            throw "ERROR: not equal matrix dimensions";

        for (size_t i = 0; i < N; ++i)
            for (size_t j = 0; j < M; ++j)
                m[i][j] -= B.m[i][j];

        return *this;
    }

    Matrix operator-(const Matrix& B) const
    {
        Matrix A(*this);
        A -= B;

        return A;
    }

    Matrix operator*(double k) const
    {
        Matrix A(*this);
        for (size_t i = 0; i < N; ++i)
            for (size_t j = 0; j < M; ++j)
                A.m[i][j] = k*m[i][j];

        return A;
    }

    Matrix operator/(double k) const
    {
        Matrix A(*this);
        for (size_t i = 0; i < N; ++i)
            for (size_t j = 0; j < M; ++j)
                A.m[i][j] = m[i][j] / k;

        return A;
    }

    friend Matrix operator*(double k, const Matrix& M)
    {
        Matrix A(M);
        for (size_t i = 0; i < M.N; ++i)
            for (size_t j = 0; j < M.M; ++j)
                A.m[i][j] = k*M.m[i][j];

        return A;
    }

    Matrix operator*(const Matrix& B) const
    {
        if (M != B.N)
            throw "ERROR: wrong matrix size";

        size_t L = B.M;
        Matrix C(N, L);

        for (size_t i = 0; i < N; ++i)
            for (size_t j = 0; j < L; ++j)
                for (size_t k = 0; k < M; ++k)
                    C.m[i][j] += m[i][k] * B.m[k][j];

        return C;
    }

    Matrix adjugate() const
    {
        if (N != M)
            throw "ERROR: wrong matrix size";

        Matrix C(this->N);
        for (size_t i = 0; i < N; ++i)
            for (size_t j = 0; j < N; ++j)
                C.m[i][j] = pow(-1, i+j) * minor(i, j);

        return C;
    }

    Matrix inv() const
    {
        if (N != M)
            throw "ERROR: wrong matrix size";

        double det = determinant();
        if (det == 0.0)
            throw "ERROR: singular matrix";

        return adjugate() / det;
    }

    Matrix pinv() const
    {
        try {
            return (transpone() * (*this)).inv() * transpone();
        } catch(...) {
        }

        return transpone() * ((*this) * transpone()).inv();
    }

    friend std::ostream& operator<<(std::ostream& os, const Matrix& A)
    {
        for (size_t i = 0; i < A.N; ++i) {
            os << "| ";
            for (size_t j = 0; j < A.M; ++j) {
                os << A.m[i][j] << " ";
            }
            os << "|" << std::endl;
        }

        return os;
    }
};

#endif // __MATRIX_H


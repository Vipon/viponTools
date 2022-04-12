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
#include <string>
#include <sstream>
#include <utility>
#include <cstddef>
#include <ostream>
#include <fstream>
#include <iterator>

class Point;
class Vector;

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

    Matrix(std::pair<size_t, size_t> size)
    {
        Matrix A(size.first, size.second);
        *this = std::move(A);
    }

    static
    Matrix ones(size_t N, size_t M)
    {
        Matrix A(N, M);
        for (size_t i = 0; i < N; ++i)
            for (size_t j = 0; j < M; ++j)
                A[i][j] = 1;

        return A;
    }

    static
    Matrix ones(std::pair<size_t, size_t> size)
    {
        return ones(size.first, size.second);
    }

    static
    Matrix load(const std::string& fn)
    {
        std::ifstream f(fn);
        double N, M;
        f >> N >> M;
        Matrix A(N, M);
        for (size_t i = 0; i < N; ++i) {
            for (size_t j = 0; j < M; ++j) {
                f >> A[i][j];
            }
        }

        return A;
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

    Matrix to_all(double (*f)(double)) const
    {
        Matrix A(this->N, this->M);
        for (size_t i = 0; i < N; ++i)
            for (size_t j = 0; j < M; ++j)
                A[i][j] = f((*this)[i][j]);

        return A;
    }

    Matrix to_all(const std::string& op, const Matrix& B) const
    {
        if (this->N != B.N || this->M != B.M)
            throw "Different dimensions of Matrix";

        auto add = [](double a, double b)
        {
            return a + b;
        };
        auto sub = [](double a, double b)
        {
            return a - b;
        };
        auto mul = [](double a, double b)
        {
            return a * b;
        };
        auto div = [](double a, double b)
        {
            return a / b;
        };

        double (*f)(double, double);
        if (op == std::string("+")) {
            f = add;
        } else if (op == std::string("-")) {
            f = sub;
        } else if (op == std::string("*")) {
            f = mul;
        } else if (op == std::string("/")) {
            f = div;
        } else {
            throw "Uknown Op";
        }

        Matrix A(this->N, this->M);
        for (size_t i = 0; i < N; ++i)
            for (size_t j = 0; j < M; ++j)
                A[i][j] = f((*this)[i][j], B[i][j]);

        return A;
    }

    Matrix concat(const Matrix& B) const
    {
        Matrix A(*this);
        if (A.N != B.N)
            throw "ERROR: wrong matrix dimensions";

        size_t newN = A.N;
        size_t newM = A.M + B.M;
        Matrix C(newN, newM);
        for (size_t i = 0; i < newN; ++i)
            for (size_t j = 0; j < newM; ++j)
                if (j < A.M)
                    C[i][j] = A[i][j];
                else
                    C[i][j] = B[i][j - A.M];

        return C;
    }

    double sum() const
    {
        double res = 0.0;
        for (size_t i = 0; i < N; ++i)
            for (size_t j = 0; j < M; ++j)
                res += (*this)[i][j];

        return res;
    }

    std::pair<size_t, size_t> size() const
    {
        return std::pair<size_t, size_t>(N, M);
    }

    std::vector<double>& operator[](size_t n)
    {
        return m[n];
    }

    const std::vector<double>& operator[](size_t n) const
    {
        return m[n];
    }

    Vector getColomn(size_t n) const;
    Point getRow(size_t n) const;

    Matrix transpone() const
    {
        Matrix A(M, N);
        for (size_t i = 0; i < M; ++i)
            for (size_t j = 0; j < N; ++j)
                A[i][j] = m[j][i];

        return A;
    }

    Matrix dropRow(size_t rowNum) const
    {
        Matrix A(N-1, M);
        for (size_t i = 0, j = 0; i < N; ++i)
            if (i != rowNum)
                A[j++] = m[i];

        return A;
    }

    Matrix dropColomn(size_t colNum) const
    {
        Matrix A(N, M-1);
        for (size_t i = 0; i < N; ++i)
            for (size_t j = 0, k = 0; j < M; ++j)
                if (j != colNum)
                    A[i][k++] = m[i][j];

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
        B.N = (size_t)-1;
        B.M = (size_t)-1;
        m = std::move(B.m);

        return *this;
    }

    Matrix& operator=(const Matrix& B)
    {
        N = B.N;
        M = B.M;
        m = B.m;

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

    Matrix& operator+=(double k)
    {
        for (size_t i = 0; i < N; ++i)
            for (size_t j = 0; j < M; ++j)
                m[i][j] += k;

        return *this;
    }

    Matrix operator+(const Matrix& B) const
    {
        Matrix A(*this);
        A += B;

        return A;
    }

    Matrix operator+(double k) const
    {
        Matrix A(*this);
        A += k;

        return A;
    }

    friend Matrix operator+(double k, const Matrix& M)
    {
        Matrix A(M);
        A += k;

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

    Matrix& operator-=(double k)
    {
        for (size_t i = 0; i < N; ++i)
            for (size_t j = 0; j < M; ++j)
                m[i][j] -= k;

        return *this;
    }

    Matrix operator-(const Matrix& B) const
    {
        Matrix A(*this);
        A -= B;

        return A;
    }

    Matrix operator-(double k) const
    {
        Matrix A(*this);
        A -= k;

        return A;
    }

    friend Matrix operator-(double k, const Matrix& M)
    {
        Matrix A(M);
        A -= k;
        A *= -1;

        return A;
    }

    Matrix& operator*=(const Matrix& B)
    {
        (*this) = (*this) * B;
        return (*this);
    }

    Matrix& operator*=(double k)
    {
        for (size_t i = 0; i < N; ++i)
            for (size_t j = 0; j < M; ++j)
                m[i][j] *= k;

        return *this;
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

    Matrix operator*(double k) const
    {
        Matrix A(*this);
        A *= k;

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

    Matrix& operator/=(const Matrix& B)
    {
        (*this) = (*this) * B.pinv();
        return (*this);
    }

    Matrix& operator/=(double k)
    {
        for (size_t i = 0; i < N; ++i)
            for (size_t j = 0; j < M; ++j)
                m[i][j] /= k;

        return *this;
    }

    Matrix operator/(const Matrix& B) const
    {
        return (*this) * B.pinv();
    }

    Matrix operator/(double k) const
    {
        Matrix A(*this);
        A /= k;

        return A;
    }

    friend Matrix operator/(double k, const Matrix& M)
    {
        return k * M.pinv();
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

    friend std::istream& operator>>(std::istream& is, Matrix& A)
    {
        for (size_t i = 0; i < A.N; ++i) {
            for (size_t j = 0; j < A.M; ++j) {
                is >> A.m[i][j];
            }
        }

        return is;
    }

    bool operator!=(const Matrix& B) const
    {
        if (N != B.N || M != B.M)
            return true;

        for (size_t i = 0; i < N; ++i)
            for (size_t j = 0; j < M; ++j)
                if (m[i][j] != B[i][j])
                    return true;

        return false;
    }

    class iterator :
        public std::iterator<
            std::input_iterator_tag, // iterator_category
            std::vector<double>      // value_type
        >
    {
    private:
        friend class Matrix;
        using MatrixIt = std::vector<std::vector<double>>::iterator;
        MatrixIt it;

    public:
        iterator(MatrixIt m) : it(m) {}
        reference operator*() const { return *it; }
        pointer operator->() { return &(*it); }
        iterator& operator++() { ++it; return (*this); }
        iterator operator++(int)
        {
            iterator newIt = (*this);
            ++(*this);
            return newIt;
        }
        bool operator==(iterator other) const { return (it == other.it); }
        bool operator!=(iterator other) const { return (it != other.it); }
    };

    class const_iterator :
        public std::iterator<
            std::output_iterator_tag,  // iterator_category
            const std::vector<double> // value_type
        >
    {
    private:
        friend class Matrix;
        using ConstMatrixIt = std::vector<std::vector<double>>::const_iterator;
        ConstMatrixIt it;

    public:
        const_iterator(ConstMatrixIt m) : it(m) {}
        reference operator*() const { return *it; }
        pointer operator->() { return &(*it); }
        const_iterator& operator++() { ++it; return (*this); }
        const_iterator operator++(int)
        {
            const_iterator newIt = (*this);
            ++(*this);
            return newIt;
        }
        bool operator==(const_iterator other) const { return (it == other.it); }
        bool operator!=(const_iterator other) const { return (it != other.it); }
    };

    iterator begin() { return iterator(m.begin()); }
    const_iterator begin() const  { return const_iterator(m.begin()); }
    iterator end()   { return iterator(m.end()); }
    const_iterator end() const  { return const_iterator(m.end()); }

    operator std::string() const
    {
        std::stringstream buffer;
        buffer << (*this);
        return buffer.str();
    }
};

#endif // __MATRIX_H


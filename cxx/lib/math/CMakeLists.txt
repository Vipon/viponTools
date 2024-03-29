# MIT License
#
# Copyright (c) 2020-2021 Konychev Valerii
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

set(MATH_HEADERS
  log.h
  #bfgs.h
  round.h
  point.h
  matrix.h
  sigmoid.h
  cxxVector.h
  linearFunc.h
  gradDescent.h
  #quasiNewton.h
  normalEquation.h
  linearRegression.h
  standardDeviation.h
  logisticRegression.h
)

set(MATH_SOURCES
  #bfgs.cpp
  point.cpp
  matrix.cpp
  cxxVector.cpp
  gradDescent.cpp
  #quasiNewton.cpp
  normalEquation.cpp
  linearRegression.cpp
  logisticRegression.cpp
)

add_vipon_library(
  NAME math
  TYPE STATIC
  HEADERS ${MATH_HEADERS}
  SOURCES ${MATH_SOURCES}
)

add_subdirectory(test)


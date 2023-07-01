# MIT License
#
# Copyright (c) 2020-2023 Konychev Valerii
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

cmake_minimum_required(VERSION 3.15 FATAL_ERROR)
project(viponTools
  LANGUAGES C CXX
  VERSION 1.0
)

include(${CMAKE_CURRENT_LIST_DIR}/cmake/toolchain/cflags/cflags.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/cmake/common.cmake)
include(CTest)

if (NOT VIPON_TOOLS_LIB_TYPE)
  set(VIPON_TOOLS_LIB_TYPE SHARED)
  if (WIN32)
    set(VIPON_TOOLS_LIB_TYPE STATIC)
  endif (WIN32)
endif (NOT VIPON_TOOLS_LIB_TYPE)

if (NOT VIPON_TOOLS_EXTERNAL_DIR)
  set(VIPON_TOOLS_EXTERNAL_DIR "${CMAKE_CURRENT_LIST_DIR}/external")
  set(VIPON_TOOLS_EXTERNAL_LIB_DIR "${VIPON_TOOLS_EXTERNAL_DIR}/lib")
  set(VIPON_TOOLS_EXTERNAL_INCLUDE_DIR "${VIPON_TOOLS_EXTERNAL_DIR}/include")
endif (NOT VIPON_TOOLS_EXTERNAL_DIR)

include_directories(AFTER "${VIPON_TOOLS_EXTERNAL_INCLUDE_DIR}")
link_directories(AFTER "${VIPON_TOOLS_EXTERNAL_LIB_DIR}")

add_subdirectory(${VIPON_TOOLS_EXTERNAL_DIR})
add_subdirectory(cTools)
add_subdirectory(cxx)


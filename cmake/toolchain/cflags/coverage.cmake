# MIT License
#
# Copyright (c) 2021 Konychev Valerii
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

include(${CMAKE_CURRENT_LIST_DIR}/cflags.cmake)

set(COVERAGE_CFLAGS)

# Need to specify libs for link, because cmake checking with linking.
set(CMAKE_REQUIRED_LIBRARIES --coverage)
if("${CMAKE_C_COMPILER_ID}" STREQUAL "GNU")
  append_cflags(COVERAGE_CFLAGS --coverage -fprofile-abs-path)
elseif("${CMAKE_C_COMPILER_ID}" STREQUAL "Clang")
  if (NOT WIN32)
    append_cflags(COVERAGE_CFLAGS "-fprofile-instr-generate -fcoverage-mapping")
  endif ()
endif()

append_cflags(COVERAGE_CFLAGS -fno-omit-frame-pointer)
unset(CMAKE_REQUIRED_LIBRARIES)

string(APPEND C_FLAGS_DEBUG   " ${COVERAGE_CFLAGS}")
string(APPEND CXX_FLAGS_DEBUG " ${COVERAGE_CFLAGS}")


# MIT License
#
# Copyright (c) 2021-2023 Konychev Valerii
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

set(WARNING_FLAGS)
append_cflags(WARNING_FLAGS -Werror -Wall -Wextra -Wcast-qual -Wnewline-eof -Wparentheses -Wsign-conversion -fdiagnostics-color=always)

# only C warning flags
set(WARNING_CFLAGS)
append_cflags(WARNING_CFLAGS -Wmissing-prototypes -Wstrict-prototypes)

# only CXX warning flags
set(WARNING_CXXFLAGS)

if("${CMAKE_C_COMPILER_ID}" STREQUAL "GNU")
  # clang doesn't support these flags (this causes too many warnings)
  append_cflags(WARNING_FLAGS -Wduplicated-branches -Wduplicated-cond)
endif()

if (MSVC)
  append_cflags(WARNING_FLAGS
    -Wno-cast-align
    -Wno-switch-enum
    -Wno-unknown-argument
    -Wno-reserved-id-macro
    -Wno-extra-semi-stmt
    -Wno-documentation-unknown-command
    -Wno-microsoft-fixed-enum
    -Wno-unused-command-line-argument
    -Wno-c++98-compat
    -Wno-covered-switch-default
    -Wno-gnu
    -Wno-empty-translation-unit
    -Wno-bad-function-cast
    -Wno-shadow-field-in-constructor
    -Wno-declaration-after-statement
    -Wno-shadow
    -Wno-old-style-cast
    -Wno-float-equal
    -Wno-c++98-compat-pedantic
    -Wno-compound-token-split-by-space
  )

  message(STATUS "msvc_version: ${MSVC_VERSION}")
  if (MSVC_VERSION GREATER_EQUAL "1930") # VS17
    append_cflags(WARNING_FLAGS
      -Wno-reserved-identifier
      -Wno-cast-function-type
      -Wno-documentation
      -Wno-microsoft-enum-value
      -Wno-duplicate-enum
    )
  endif ()
  if (MSVC_VERSION GREATER_EQUAL "1934") # VS17.7.2
    append_cflags(WARNING_FLAGS
      -Wno-unsafe-buffer-usage
    )
  endif ()
endif (MSVC)

if (WIN32)
  append_cflags(WARNING_FLAGS
    -Wno-deprecated-declarations
    -Wno-ignored-attributes
  )
endif (WIN32)

string(APPEND C_FLAGS   " ${WARNING_FLAGS}")
string(APPEND C_FLAGS   " ${WARNING_CFLAGS}")

string(APPEND CXX_FLAGS " ${WARNING_FLAGS}")
string(APPEND CXX_FLAGS " ${WARNING_CXXFLAGS}")


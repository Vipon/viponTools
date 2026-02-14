# MIT License
#
# Copyright (c) 2020-2024 Konychev Valerii
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

add_subdirectory(peParse)
add_subdirectory(elfParse)
add_subdirectory(machoParse)

set(BIN_PARSE_LIB_LIST
  pe64Parse
  elf64Parse
  elf32Parse
  macho64Parse
  fatMacho64Parse
)

add_vipon_library(
  NAME binParse
  TYPE ${VIPON_TOOLS_LIB_TYPE}
  HEADERS binParse.h
  SOURCES binParse.c
  LINK_LIBS ${BIN_PARSE_LIB_LIST}
  INSTALL ON
)

if (WIN32)
  target_compile_options(binParse PRIVATE "-Wno-cast-function-type-mismatch")
  if (VIPON_TOOLS_LIB_TYPE STREQUAL "SHARED")
    target_compile_definitions(binParse PRIVATE BIN_PARSER_SHARED_LIB)
  elseif (VIPON_TOOLS_LIB_TYPE STREQUAL "STATIC")
    target_compile_definitions(binParse PRIVATE BIN_PARSER_STATIC_LIB)
  endif ()
endif (WIN32)


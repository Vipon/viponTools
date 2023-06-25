# MIT License
#
# Copyright (c) 2023 Konychev Valerii
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

set(BIN_DYN_MOD_SOURCES binDynMod.c)
set(BIN_DYN_MOD_HEADERS binDynMod.h)
set(BIN_DYN_MOD_LIB_LIST binParse)

if ("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
  add_subdirectory(peDynMod)
  list(APPEND BIN_DYN_MOD_LIB_LIST pe64DynMod)
endif ()

if ("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
  add_subdirectory(elfDynMod)
  list(APPEND BIN_DYN_MOD_LIB_LIST elf64DynMod elf32DynMod)
endif ()

if ("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
  add_subdirectory(machoDynMod)
  list(APPEND BIN_DYN_MOD_LIB_LIST macho64DynMod fatMacho64DynMod)
endif ()

add_vipon_library(
  NAME binDynMod
  TYPE STATIC
  HEADERS ${BIN_DYN_MOD_HEADERS}
  SOURCES ${BIN_DYN_MOD_SOURCES}
  LINK_LIBS ${BIN_DYN_MOD_LIB_LIST}
  INSTALL ON
)

add_subdirectory(test)


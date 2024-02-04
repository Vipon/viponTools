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

set(AARCH64_LIB_SOURCES
  aarch64.c
)

set(AARCH64_LIB_HEADERS
  aarch64.h
)

add_vipon_library(
  NAME aarch64
  TYPE ${VIPON_TOOLS_LIB_TYPE}
  HEADERS ${AARCH64_LIB_HEADERS}
  SOURCES ${AARCH64_LIB_SOURCES}
  LINK_LIBS string
  INSTALL ON
)

set(AARCH64_CODE_MOVE_LIB_SOURCES
  aarch64_code_move.c
)

set(AARCH64_CODE_MOVE_LIB_HEADERS
  aarch64_code_move.h
)

add_vipon_library(
  NAME aarch64_code_move
  TYPE ${VIPON_TOOLS_LIB_TYPE}
  HEADERS ${AARCH64_CODE_MOVE_LIB_HEADERS}
  SOURCES ${AARCH64_CODE_MOVE_LIB_SOURCES}
  LINK_LIBS aarch64 code_move mem vt_containers
  INSTALL ON
)

set(AARCH64_DISASM_WRAP_LIB_SOURCES
  aarch64_capstone_wrap.c
)

set(AARCH64_DISASM_WRAP_LIB_HEADERS
  aarch64_disasm_wrap.h
)

add_vipon_library(
  NAME aarch64_disasm_wrap
  TYPE ${VIPON_TOOLS_LIB_TYPE}
  HEADERS ${AARCH64_DISASM_WRAP_LIB_HEADERS}
  SOURCES ${AARCH64_DISASM_WRAP_LIB_SOURCES}
  LINK_LIBS capstone
  DEFINES -DCAPSTONE_DISASM
  INSTALL ON
)

set(AARCH64_ASM_WRAP_LIB_SOURCES
  aarch64_keystone_wrap.c
)

set(AARCH64_ASM_WRAP_LIB_HEADERS
  aarch64_asm_wrap.h
)

set(AARCH64_ASM_WRAP_LIB_LINKS keystone)
if (NOT WIN32)
  list(APPEND AARCH64_ASM_WRAP_LIB_LINKS stdc++)
endif (NOT WIN32)

add_vipon_library(
  NAME aarch64_asm_wrap
  TYPE ${VIPON_TOOLS_LIB_TYPE}
  HEADERS ${AARCH64_ASM_WRAP_LIB_HEADERS}
  SOURCES ${AARCH64_ASM_WRAP_LIB_SOURCES}
  LINK_LIBS ${AARCH64_ASM_WRAP_LIB_LINKS}
  DEFINES -DKEYSTONE_ASM
  INSTALL ON
)

add_subdirectory(test)

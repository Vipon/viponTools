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

set(X86_64_HEADERS
  x86_64.h
  x86_64_DisassemblerWrap.h
)

set(X86_64_SOURCES
  x86_64.c
  x86_64_CapstoneWrap.c
)

add_vipon_library(
  NAME asmWrap
  TYPE ${VIPON_TOOLS_LIB_TYPE}
  HEADERS ${X86_64_HEADERS}
  SOURCES ${X86_64_SOURCES}
  LINK_LIBS capstone mem
  INSTALL ON
)

target_compile_definitions(asmWrap PUBLIC -DCAPSTONE_DISASSEMBLER)


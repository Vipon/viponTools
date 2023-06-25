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

set(ELF64_DYN_MOD_SOURCES
  elf64DynMod.c
)

set(ELF64_DYN_MOD_HEADERS
  elf64DynMod.h
)

# Dynamic modification library must be static
add_vipon_library(
  NAME elf64DynMod
  TYPE STATIC
  HEADERS ${ELF64_DYN_MOD_HEADERS}
  SOURCES ${ELF64_DYN_MOD_SOURCES}
  LINK_LIBS file mem string elf64Parse
  INSTALL ON
)

set(ELF32_DYN_MOD_SOURCES
  elf32DynMod.c
)

set(ELF32_DYN_MOD_HEADERS
  elf32DynMod.h
)

# Dynamic modification library must be static
add_vipon_library(
  NAME elf32DynMod
  TYPE STATIC
  HEADERS ${ELF32_DYN_MOD_HEADERS}
  SOURCES ${ELF32_DYN_MOD_SOURCES}
  LINK_LIBS file mem string elf32Parse
  INSTALL ON
)


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

set(MACHO64_DYN_MOD_SOURCES
  macho64DynMod.c
)

set(MACHO64_DYN_MOD_HEADERS
  macho64DynMod.h
)

# Dynamic modification library must be static
add_vipon_library(
  NAME macho64DynMod
  TYPE STATIC
  HEADERS ${MACHO64_DYN_MOD_HEADERS}
  SOURCES ${MACHO64_DYN_MOD_SOURCES}
  LINK_LIBS file mem string macho64Parse
  INSTALL ON
)

set(FAT_MACHO64_DYN_MOD_SOURCES
  fatMacho64DynMod.c
)

set(FAT_MACHO64_DYN_MOD_HEADERS
  fatMacho64DynMod.h
)

# Dynamic modification library must be static
add_vipon_library(
  NAME fatMacho64DynMod
  TYPE STATIC
  HEADERS ${FAT_MACHO64_DYN_MOD_HEADERS}
  SOURCES ${FAT_MACHO64_DYN_MOD_SOURCES}
  LINK_LIBS file mem string fatMacho64Parse
  INSTALL ON
)


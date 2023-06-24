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

add_vipon_library(
  NAME macho64Parse
  TYPE SHARED
  HEADERS macho64Parse.h macho.h macho64.h
  SOURCES macho64Parse.c
  LINK_LIBS file mem string
  INSTALL ON
)

add_vipon_library(
  NAME fatMacho64Parse
  TYPE SHARED
  HEADERS fatMacho64Parse.h
  SOURCES fatMacho64Parse.c
  LINK_LIBS macho64Parse file mem string
  INSTALL ON
)

# Dynamic modification library must be static
add_vipon_library(
  NAME macho64DynMod
  TYPE STATIC
  HEADERS macho64DynMod.h
  SOURCES macho64DynMod.c
  LINK_LIBS file mem string
  INSTALL ON
)

# Dynamic modification library must be static
add_vipon_library(
  NAME fatMacho64DynMod
  TYPE STATIC
  HEADERS fatMacho64DynMod.h
  SOURCES fatMacho64DynMod.c
  LINK_LIBS file mem string
  INSTALL ON
)

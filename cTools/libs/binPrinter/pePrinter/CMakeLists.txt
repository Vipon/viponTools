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

set(PE64_PRINTER_SOURSES
  pe64PrinterHeaders.c
  pe64PrinterSections.c
  pe64PrinterSymbols.c
  pe64PrinterImports.c
  pe64PrinterDelayImports.c
  pe64PrinterExports.c
)

set(PE64_PRINTER_HEADERS
  pe64Printer.h
)

add_vipon_library(
  NAME pe64Printer
  TYPE ${VIPON_TOOLS_LIB_TYPE}
  SOURCES ${PE64_PRINTER_SOURSES}
  HEADERS ${PE64_PRINTER_HEADERS}
  LINK_LIBS pe64Parse
  INSTALL ON
)

add_subdirectory(test)


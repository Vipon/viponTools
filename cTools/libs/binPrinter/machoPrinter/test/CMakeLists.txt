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

add_vipon_test(
  NAME macho64PrintExeTest
  SOURCES macho64PrintExeTest.c
  LINK_LIBS macho64Printer macho64Parse file string LEB128 comdef mem
  CMD_LINE macho64PrintExeTest_ref macho64PrintExeTest_ref.txt
  TEST_FILES macho64PrintExeTest_ref macho64PrintExeTest_ref.txt
)

add_vipon_test(
  NAME macho64PrintObjTest
  SOURCES macho64PrintObjTest.c
  LINK_LIBS macho64Printer macho64Parse file string LEB128 comdef mem
  CMD_LINE macho64PrintObjTest_ref macho64PrintObjTest_ref.txt
  TEST_FILES macho64PrintObjTest_ref macho64PrintObjTest_ref.txt
)


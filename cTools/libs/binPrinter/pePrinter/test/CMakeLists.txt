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

# Need for testing print of delay load libs
if (MSVC)
  set(TEST_LINK_FLAGS /DELAYLOAD:delayLib.dll)
else ()
  set(TEST_LINK_FLAGS -Wl,-delayload:delayLib.dll)
endif ()

add_vipon_library(
  NAME delayLib
  TYPE SHARED
  SOURCES delayLib.c delayLib.h
)

add_vipon_test(
  NAME pe64PrintExeTest
  SOURCES pe64PrintExeTest.c
  # Delay load doesn't work without delayimp.lib
  LINK_LIBS pe64Parse pe64Printer delayLib file mem string comdef delayimp.lib
  LINK_FLAGS ${TEST_LINK_FLAGS}
)

add_custom_target(testObjFile
  COMMAND ${CMAKE_C_COMPILER} -c -g3 -ggdb -Wno-unknown-argument "${CMAKE_CURRENT_LIST_DIR}/testObjFile.c"
  DEPENDS testObjFile.c
)

if (MSVC)
  set(TEST_FILE "${CMAKE_CURRENT_BINARY_DIR}/testObjFile.obj")
else ()
  set(TEST_FILE "${CMAKE_CURRENT_BINARY_DIR}/testObjFile.o")
endif ()

add_vipon_test(
  NAME pe64PrintObjTest
  SOURCES pe64PrintObjTest.c
  LINK_LIBS pe64Parse pe64Printer file mem string comdef
  CMD_LINE "${TEST_FILE}"
  DEPENDS testObjFile
)


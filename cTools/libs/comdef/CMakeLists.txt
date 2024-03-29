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

set(COMDEF_HEADERS comdef.h)
set(COMDEF_SOURCES comdef.c)

add_library(comdef ${VIPON_TOOLS_LIB_TYPE}
  ${COMDEF_SOURCES}
  ${COMDEF_HEADERS}
)

if (WIN32)
  if (VIPON_TOOLS_LIB_TYPE STREQUAL "SHARED")
    install(
      TARGETS comdef
      DESTINATION ${INSTALL_BIN_DIR}
    )
  else ()
    install(
      TARGETS comdef
      DESTINATION ${INSTALL_LIB_DIR}
    )
  endif ()
else (WIN32)
  install(
    TARGETS comdef
    DESTINATION ${INSTALL_LIB_DIR}
  )
endif (WIN32)

installHeaders("${COMDEF_HEADERS}")

target_include_directories(comdef INTERFACE
  ${CMAKE_CURRENT_LIST_DIR}
)

if (WIN32)
  if (VIPON_TOOLS_LIB_TYPE STREQUAL "SHARED")
    target_compile_definitions(comdef PRIVATE COMDEF_SHARED_LIB)
  else ()
    target_compile_definitions(comdef PRIVATE STATIC_LIB)
  endif ()
endif (WIN32)


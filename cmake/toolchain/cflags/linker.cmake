# MIT License
#
# Copyright (c) 2026 Konychev Valerii
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

if (APPLE)
  if ("${CMAKE_HOST_SYSTEM_PROCESSOR}" STREQUAL "arm64")
    if ("${CMAKE_C_COMPILER_ID}" STREQUAL "Clang" OR "${CMAKE_C_COMPILER_ID}" STREQUAL "AppleClang")
      if (CMAKE_C_COMPILER_VERSION VERSION_LESS 17.0)
        # Main MacOsX linker has problem with linking MOD_CODE macro
        string(APPEND _LINKER_FLAGS " -ld_classic")
      endif ()
    endif ()
  endif ()
endif (APPLE)

if ("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
  if ("${CMAKE_SYSTEM_PROCESSOR}" STREQUAL "i386")
    string(APPEND _LINKER_FLAGS " -Wl,-m,elf_i386")
  endif()
endif()

# MIT License
#
# Copyright (c) 2021-2024 Konychev Valerii
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

include_guard(GLOBAL)
include(CheckCCompilerFlag)
include(CheckCXXCompilerFlag)

macro(append_cflags var)
  foreach(flag ${ARGN})
    set(check "Flag: ${flag}")
    check_c_compiler_flag("${flag} -w" ${check})
    if(${check})
      string(APPEND ${var} " ${flag}")
    endif()
  endforeach()
endmacro()

macro(append_cxxflags var)
  foreach(flag ${ARGN})
    set(check "Flag: ${flag}")
    check_cxx_compiler_flag("${flag} -w" ${check})
    if(${check})
      string(APPEND ${var} " ${flag}")
    endif()
  endforeach()
endmacro()

include(${CMAKE_CURRENT_LIST_DIR}/common.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/debug.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/release.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/coverage.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/sanitizers.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/warnings.cmake)

set(CMAKE_C_FLAGS ${C_FLAGS})
set(CMAKE_C_FLAGS_DEBUG ${C_FLAGS_DEBUG})
set(CMAKE_C_FLAGS_RELEASE ${C_FLAGS_RELEASE})

set(CMAKE_CXX_FLAGS ${CXX_FLAGS})
set(CMAKE_CXX_FLAGS_DEBUG ${CXX_FLAGS_DEBUG})
set(CMAKE_CXX_FLAGS_RELEASE ${CXX_FLAGS_RELEASE})

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

if (NOT "${_LINKER_FLAGS}" STREQUAL "")
  string(REPLACE " " ";" LINKER_FLAGS "${_LINKER_FLAGS}")
  add_link_options(${LINKER_FLAGS})
endif ()


# MIT License
#
# Copyright (c) 2021-2023 Konychev Valerii
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

include(${CMAKE_CURRENT_LIST_DIR}/cflags.cmake)

set(SANITIZERS_FLAGS)

# Need to specify libs for link, because cmake checking with linking.
if (WIN32)
  set(CMAKE_REQUIRED_LIBRARIES "-lclang_rt.asan-x86_64")
  if (CMAKE_BUILD_TYPE STREQUAL "Release")
    # Sanitizers doesn't work with debug informations
    append_cflags(SANITIZERS_FLAGS -fsanitize=address -fsanitize=leak -fsanitize=undefined)
  endif ()
elseif (APPLE)
  set(CMAKE_REQUIRED_LINK_OPTIONS "-fsanitize=address")
  append_cflags(SANITIZERS_FLAGS -fsanitize=address -fsanitize=leak -fsanitize=undefined)
  unset(CMAKE_REQUIRED_LINK_OPTIONS)
  string(APPEND _LINKER_FLAGS " ${SANITIZERS_FLAGS}")
else ()
  set(CMAKE_REQUIRED_LIBRARIES "-lasan -lubsan")
  append_cflags(SANITIZERS_FLAGS -fsanitize=address -fsanitize=leak -fsanitize=undefined)
endif ()

unset(CMAKE_REQUIRED_LIBRARIES)

string(APPEND C_FLAGS_DEBUG   " ${SANITIZERS_FLAGS}")
string(APPEND CXX_FLAGS_DEBUG " ${SANITIZERS_FLAGS}")


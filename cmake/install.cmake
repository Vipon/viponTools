# MIT License
#
# Copyright (c) 2022 Konychev Valerii
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

if (MSVC)
  set(CMAKE_VS_INCLUDE_INSTALL_TO_DEFAULT_BUILD ON)
endif ()

set(INSTALL_LIB_DIR "${CMAKE_INSTALL_PREFIX}/lib")
set(INSTALL_BIN_DIR "${CMAKE_INSTALL_PREFIX}/bin")
set(INSTALL_INCLUDE_DIR "${CMAKE_INSTALL_PREFIX}/include")
if (APPLE)
  list(APPEND CMAKE_INSTALL_RPATH "@loader_path/../lib")
elseif (UNIX AND NOT APPLE)
  list(APPEND CMAKE_INSTALL_RPATH "$ORIGIN/../lib")
endif ()


function(installFiles dir)
  foreach(file ${ARGN})
    install(
      FILES "${file}"
      DESTINATION ${dir}
    )
  endforeach(file)
endfunction(installFiles)

function(installHeaders)
  installFiles(${INSTALL_INCLUDE_DIR} ${ARGN})
endfunction(installHeaders)


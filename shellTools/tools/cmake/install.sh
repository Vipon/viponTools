#!/bin/bash
#
# MIT License
#
# Copyright (c) 2021 Konychev Valera
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

SCRIPT_DIR="$(realpath $(dirname "${BASH_SOURCE[0]}"))"
CMAKE_ROOT="${SCRIPT_DIR}/cmake"
CMAKE_BUILD_DIR="${CMAKE_ROOT}/build"
CMAKE_VERSION="3.20.3"
CMAKE_INSTALL_PATH="${HOME}/.local"
CMAKE_BUILD_TYPE="Release"

downloadCmakeSrcCode()
{
    wget "https://github.com/Kitware/CMake/archive/refs/tags/v${CMAKE_VERSION}.tar.gz" \
        -O cmake.tar.gz
    mkdir ${CMAKE_ROOT}
    tar -xzvf cmake.tar.gz -C "${CMAKE_ROOT}" --strip-components 1
}

buildCmake()
{
    mkdir "${CMAKE_BUILD_DIR}"
    cd "${CMAKE_BUILD_DIR}"
    cmake -DCMAKE_BUILD_TYPE="${CMAKE_BUILD_TYPE}"       \
          -DCMAKE_INSTALL_PREFIX="${CMAKE_INSTALL_PATH}" \
          ..
    make -j8
}

installCmake()
{
    cd "${CMAKE_BUILD_DIR}"
    make install
}

main()
{
    cd "${SCRIPT_DIR}"
    downloadCmakeSrcCode
    buildCmake
    installCmake
}

main "$@"


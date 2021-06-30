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

ROOT="$(realpath $(dirname "${BASH_SOURCE[0]}"))"
CAPSTONE_ROOT="${ROOT}/capstone"
CAPSTONE_BUILD_DIR="${CAPSTONE_ROOT}/build"
INSTALL_PATH="${HOME}/.local"
BUILD_ARCH="AArch64;ARM;Mips;PowerPC;X86"
BUILD_TYPE="Release"
SHARED_LIBS="ON"

downloadSrcCode()
{
    wget "https://github.com/aquynh/capstone/archive/3.0.5.tar.gz" \
         -O capstone.tar.gz
    mkdir "${CAPSTONE_ROOT}"
    tar -xzvf capstone.tar.gz -C "${CAPSTONE_ROOT}" --strip-components 1
}

buildCapstone()
{
    mkdir "${CAPSTONE_BUILD_DIR}"
    cd "${CAPSTONE_BUILD_DIR}"
    cmake -DCMAKE_BUILD_TYPE="${BUILD_TYPE}"       \
          -DCAPSTONE_X86_ATT_DISABLE="ON"          \
          -DCMAKE_INSTALL_PREFIX="${INSTALL_PATH}" \
          ..
    make -j8
}

installCapstone()
{
    cd "${CAPSTONE_BUILD_DIR}"
    make install
}

main()
{
    cd "${ROOT}"
    downloadSrcCode
    buildCapstone
    installCapstone
}

main "$@"

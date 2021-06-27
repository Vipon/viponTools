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
CCACHE_ROOT="${SCRIPT_DIR}/ccache"
CCACHE_BUILD_DIR="${CCACHE_ROOT}/build"
CCACHE_VERSION="4.3"
CCACHE_INSTALL_PATH="${HOME}/.local"
CCACHE_BUILD_TYPE="Release"

downloadCCacheSrcCode()
{
    wget "https://github.com/ccache/ccache/archive/refs/tags/v${CCACHE_VERSION}.tar.gz" \
        -O ccache.tar.gz
    mkdir ${CCACHE_ROOT}
    tar -xzvf ccache.tar.gz -C "${CCACHE_ROOT}" --strip-components 1
}

buildCCache()
{
    mkdir "${CCACHE_BUILD_DIR}"
    cd "${CCACHE_BUILD_DIR}"
    cmake -DCMAKE_BUILD_TYPE="${CCACHE_BUILD_TYPE}"       \
          -DCMAKE_INSTALL_PREFIX="${CCACHE_INSTALL_PATH}" \
          ..
    make -j8
}

installCCache()
{
    cd "${CCACHE_BUILD_DIR}"
    make install
}

main()
{
    cd "${SCRIPT_DIR}"
    downloadCCacheSrcCode
    buildCCache
    installCCache
}

main "$@"


#!/bin/bash
#
# MIT License
#
# Copyright (c) 2020 Konychev Valera
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

SCRIPT_DIR="$(dirname "${BASH_SOURCE[0]}")"
source "${SCRIPT_DIR}/../libCommon.sh"

NUM_THREADS=`nproc`

SYSROOT="/"
LLVM_PATH="${PWD}/llvm"
BUILD_PATH="${PWD}/build"
INSTALL_PATH="${PWD}/install"
ENABLE_CCACHE="OFF"
BUILD_TYPE="Release"
CROSS_COMPILE="False"
LLVM_TARGETS_TO_BUILD="X86"

SCRIPT_NAME=`basename "$0"`
printUsage()
{
    echo "${SCRIPT_NAME} ..."
}


printOptions()
{
    echo -e "Available parameters:"
    echo -e "\t[--build-path]\t\t where whould be built."
    echo -e "\t[-c|--cross-compile]\t build compiler for cross-compiling."
    echo -e "\t[-d|--debug]\t\t make debug build."
    echo -e "\t[--enable-ccahe]\t enable using ccache during llvm build."
    echo -e "\t[-h|--help]\t\t print this man."
    echo -e "\t[--install-path]\t where whould be installed."
    echo -e "\t[-j|--jobs NUM_THREADS]\t set number of threads for make."
    echo -e "\t[--llvm-path]\t\t path to llvm source code."
    echo -e "\t[-s|--sysroot]\t\t path to sysroot, which should be used."
}


printHelp()
{
    printUsage
    echo
    printOptions
}


makeAbsolutePath()
{
    realpath "$1"
}


parseArgs()
{
    opts=$(getopt -o cdhj:s: \
                  --long build-path: \
                  --long cross-compile \
                  --long debug \
                  --long enable-ccache \
                  --long help \
                  --long install-path: \
                  --long jobs: \
                  --long llvm-path: \
                  --long sysroot: \
                  -- "$@")
    if [ $? -ne 0 ]; then
        echo "ERROR: Incorrect options provided."
        printOptions
        exit -1
    fi

    eval set -- "${opts}"
    while true; do
        case "$1" in
        --build-path)
            BUILD_PATH="$(makeAbsolutePath "$2")"
            shift 2
            ;;

        -c|--cross-compile)
            CROSS_COMPILE="True"
            LLVM_TARGETS_TO_BUILD="X86;PowerPC;ARM;AArch64;Mips"
            shift 1
            ;;

        -d|--debug)
            BUILD_TYPE="Debug"
            shift 1
            ;;

        --enable-ccache)
            ENABLE_CCACHE="ON"
            shift 1
            ;;

        -h|--help)
            printHelp
            exit 0
            ;;

        --install-path)
            INSTALL_PATH="$(makeAbsolutePath "$2")"
            shift 2
            ;;

        -j|--jobs)
            NUM_THREADS="$2"
            shift 2
            ;;

        --llvm-path)
            LLVM_PATH="$(makeAbsolutePath "$2")"
            shift 2
            ;;

        -s|--sysroot)
            SYSROOT="$2"
            shift 2
            ;;

        --)
            shift
            break
            ;;
        esac
    done
}


build()
{
    # Clean LDFLAGS, CFLAGS and CXXFLAGS, beacuse they must be empty.
    OLD_LDFLAGS="${LDFLAGS}"
    echo "OLDLDFLAGS: ${LDFLAGS}"
    export LDFLAGS=""
    echo "NEWLDFLAGS: ${LDFLAGS}"

    OLD_CFLAGS="${CFLAGS}"
    echo "OLDCFLAGS: ${CFLAGS}"
    export CFLAGS="--sysroot=${SYSROOT} -I${SYSROOT}/include/ -I/usr/include"
    echo "NEWCFLAGS: ${CFLAGS}"

    OLD_CXXFLAGS="${CXXFLAGS}"
    echo "OLDCXXFLAGS: ${CXXFLAGS}"
    export CXXFLAGS="${CFLAGS}"
    echo "NEWCXXFLAGS: ${CXXFLAGS}"

    mkdir -p "${BUILD_PATH}"
    cd "${BUILD_PATH}"

    cmake -DLLVM_ENABLE_PEDANTIC=OFF \
          -DCMAKE_C_FLAGS="${CFLAGS}" \
          -DCMAKE_CXX_FLAGS="${CXXFLAGS}" \
          -DCMAKE_BUILD_TYPE="${BUILD_TYPE}" \
          -DLLVM_TARGETS_TO_BUILD="${LLVM_TARGETS_TO_BUILD}" \
          -DLLVM_TOOL_LLD_BUILD=ON \
          -DLLVM_BUILD_TOOLS=ON \
          -DLLVM_BUILD_UTILS=OFF \
          -DLLVM_ENABLE_BINDINGS=OFF \
          -DLLVM_BUILD_RUNTIME=ON \
          -DLLVM_BUILD_RUNTIMES=ON \
          -DLLVM_APPEND_VC_REV=OFF \
          -DCLANG_INSTALL_SCANBUILD=OFF \
          -DCLANG_INSTALL_SCANVIEW=OFF \
          -DCLANG_PLUGIN_SUPPORT=OFF \
          -DLLVM_CCACHE_BUILD="${ENABLE_CCACHE}" \
          -DLLVM_ENABLE_TERMINFO=OFF \
          -DCMAKE_INSTALL_PREFIX="${INSTALL_PATH}" "${LLVM_PATH}"
    if [ "$?" != "0" ]; then
        pError "llvm cmake fail."
        exit -1
    fi

    make -j"${NUM_THREADS}"
    if [ "$?" != "0" ]; then
        pError "llvm build fail."
        exit -1
    fi

    # Restore LDFLAGS, CFLAGS and CXXFLAGS.
    export LDFLAGS="${OLD_LDLAGS}"
    echo "RESTORE LDFLAGS: ${LDFLAGS}"
    export CFLAGS="${OLD_CFLAGS}"
    echo "RESTORE CFLAGS: ${CFLAGS}"
    export CXXFLAGS="${OLD_CXXFLAGS}"
    echo "RESTORE CXXFLAGS: ${CXXFLAGS}"
}


main()
{
    parseArgs "$@"
    build
}

main "$@"


#!/bin/bash

TRUE="true"
FALSE="false"

NUM_THREADS=`nproc`

VERSION="9.0.1"
LLVM_PATH=""
BUILD_PATH=""
INSTALL_PATH=""
DOWNLOAD_PATH="${PWD}/download"
BASE_URL=""
PUBLIC_KEY=""

TEST="${FALSE}"
SYSROOT="/"
LLD_BUILD="${FALSE}"
LIBCXX_BUILD="${FALSE}"
COMPILER_RT_BUILD="${FALSE}"
BUILD_TYPE="Release"
CROSS_COMPILE="${FALSE}"
LLVM_TARGETS_TO_BUILD="X86"
ENABLE_CCACHE="${FALSE}"

checkUtils()
{
    # Check exesting of wget
    if [ `which wget` == "" ]; then
        echo "ERROR: There is no wget."
        exit -1
    fi

    # Check exesting of tar
    if [ `which tar` == "" ]; then
        echo "ERROR: There is no tar."
        exit -1
    fi

    # Check exesting of xz
    if [ `which xz` == "" ]; then
        echo "ERROR: There is no xz."
        exit -1
    fi

    # Check exesting of gpg
    if [ `which gpg` == "" ]; then
        echo "ERROR: There is no gpg."
        exit -1
    fi

    # Check exesting of cmake
    if [ `which cmake` == "" ]; then
        echo "ERROR: There is no cmake."
        exit -1
    fi

    # Check exesting of make
    if [ `which make` == "" ]; then
        echo "ERROR: There is no make."
        exit -1
    fi

    return 0
}


SCRIPT_NAME=`basename "$0"`
printUsage()
{
    echo "${SCRIPT_NAME} ..."
}


printOptions()
{
    echo -e "Available parameters:"
    echo -e "\t[--build-path]\t\t where whould be built."
    echo -e "\t[--compiler-rt]\t\t build compiler-rt."
    echo -e "\t[-c|--cross-compile]\t build compiler for cross-compiling."
    echo -e "\t[-d|--debug]\t\t make debug build."
    echo -e "\t[--enable-ccahe]\t enable using ccache during llvm build."
    echo -e "\t[-f|--full]\t\t build everything."
    echo -e "\t[-h|--help]\t\t print this man."
    echo -e "\t[--install-path]\t where whould be installed."
    echo -e "\t[-j|--jobs NUM_THREADS]\t set number of threads for make."
    echo -e "\t[--version]\t specify version of LLVM. Default: 9.0.1."
    echo -e "\t[--libcxx]\t\t build libcxx."
    echo -e "\t[--lld]\t\t\t build lld."
    echo -e "\t[--llvm-path]\t\t path to llvm source code."
    echo -e "\t[-s|--sysroot]\t\t path to sysroot, which should be used."
    echo -e "\t[-t|--test]\t\t test clang."
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


printBuildInfo()
{
    echo "LLVM version: ${VERSION}"
    echo "NUM_THREADS: ${NUM_THREADS}"
    echo "BUILD_TYPE: ${BUILD_TYPE}"
    echo "CROSS_COMPILE: ${CROSS_COMPILE}"
    echo "LLVM_PATH: ${LLVM_PATH}"
    echo "BUILD_PATH: ${LLVM_PATH}"
    echo "INSTALL_PATH: ${LLVM_PATH}"
    echo "LLD_BUILD: ${LLD_BUILD}"
    echo "LIBCXX_BUILD: ${LIBCXX_BUILD}"
    echo "COMPILER_RT_BUILD: ${COMPILER_RT_BUILD}"
    echo "LLVM_TARGETS_TO_BUILD: ${LLVM_TARGETS_TO_BUILD}"
    echo "TEST: ${TEST}"
}


parseArgs()
{
    opts=$(getopt -o cdfhj:s:t         \
                  --long build-path:   \
                  --long compiler-rt   \
                  --long cross-compile \
                  --long debug         \
                  --long enable-ccache \
                  --long full          \
                  --long help          \
                  --long install-path: \
                  --long jobs:         \
                  --long version:      \
                  --long libcxx        \
                  --long lld           \
                  --long llvm-path:    \
                  --long sysroot:      \
                  --long test          \
                  -- "$@")
    if [ $? -ne 0 ]; then
        echo "Incorrect option provided"
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

            --compiler-rt)
                COMPILER_RT_BUILD="${TRUE}"
                shift 1
                ;;

            -c|--cross-compile)
                CROSS_COMPILE="${TRUE}"
                LLVM_TARGETS_TO_BUILD="X86;PowerPC;ARM;AArch64;Mips"
                shift 1
                ;;

            -d|--debug)
                BUILD_TYPE="Debug"
                shift 1
                ;;

            --enable-ccache)
                ENABLE_CCACHE="${TRUE}"
                shift 1
                ;;

            -f|--full)
                LLD_BUILD="${TRUE}"
                LIBCXX_BUILD="${TRUE}"
                COMPILER_RT_BUILD="${TRUE}"
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

            --version)
                VERSION="$2"
                shift 2
                ;;

            --libcxx)
                LIBCXX_BUILD="${TRUE}"
                shift 1
                ;;

            --lld)
                LLD_BUILD="${TRUE}"
                shift 1
                ;;

            --llvm-path)
                LLVM_PATH="$(makeAbsolutePath "$2")"
                shift 2
                ;;

            -s|--sysroot)
                SYSROOT="$2"
                shift 2
                ;;

            -t|--test)
                TEST="${TRUE}"
                shift 1
                ;;

            --)
                shift 1
                break
                ;;
        esac
     done

    if [ "${LLVM_PATH}" == "" ]; then
        LLVM_PATH="${PWD}/llvm-${VERSION}"
    fi
    if [ "${BUILD_PATH}" == "" ]; then
        BUILD_PATH="${PWD}/buildLLVM-${VERSION}"
    fi
    if [ "${INSTALL_PATH}" == "" ]; then
        INSTALL_PATH="${PWD}/installLLVM-${VERSION}"
    fi

    # new URL
    BASE_URL="https://github.com/llvm/llvm-project/releases/download/llvmorg-${VERSION}"
    if [[ "${VERSION}" < "7.0.1" ]]; then
        # old URL
        BASE_URL="https://releases.llvm.org/${VERSION}"
    fi

    printBuildInfo
    return 0
}


downloadPublicKey()
{
    if [ -f "${DOWNLOAD_PATH}/.download_public_key_${VERSION}_Success" ]; then
        return 0
    fi

    # Could be 2 different name of public key:
    #   tstellar-gpg-key.asc
    #   hans-gpg-key-asc
    # It depends on who published release.
    PUBLIC_KEY="tstellar-gpg-key.asc"
    wget "${BASE_URL}/${PUBLIC_KEY}" -P "${DOWNLOAD_PATH}"
    if [ "$?" != "0" ]; then
        PUBLIC_KEY="hans-gpg-key.asc"
        wget "${BASE_URL}/${PUBLIC_KEY}" -P "${DOWNLOAD_PATH}"
        if [ "$?" != "0" ]; then
            echo "ERROR: cann't download public key."
            exit -1
        fi
    fi

    gpg --import "${DOWNLOAD_PATH}/${PUBLIC_KEY}"
    if [ "$?" != "0" ]; then
        echo "ERROR: cann't import public key."
        exit -1
    fi

    touch "${DOWNLOAD_PATH}/.download_public_key_${VERSION}_Success"
    return 0
}


downloadLLVMTool()
{
    TOOL="$1"
    if [ -f "${DOWNLOAD_PATH}/.download_${TOOL}_${VERSION}_success" ]; then
        return 0
    fi

    wget "${BASE_URL}/${TOOL}-${VERSION}.src.tar.xz" -P "${DOWNLOAD_PATH}"
    if [ "$?" != "0" ]; then
        echo "ERROR: cannot download ${TOOL}."
        exit -1
    fi

    wget "${BASE_URL}/${TOOL}-${VERSION}.src.tar.xz.sig" -P "${DOWNLOAD_PATH}"
    if [ "$?" != "0" ]; then
        echo "ERROR: cann't download signature for ${TOOL}."
        exit -1
    fi

    gpg --verify "${DOWNLOAD_PATH}/${TOOL}-${VERSION}.src.tar.xz.sig" \
                 "${DOWNLOAD_PATH}/${TOOL}-${VERSION}.src.tar.xz"
    if [ "$?" != "0" ]; then
        echo "ERROR: cann't verify ${TOOL}."
        exit -1
    fi

    touch "${DOWNLOAD_PATH}/.download_${TOOL}_${VERSION}_success"
    return 0
}


downloadSourceCode()
{
    mkdir -p "${DOWNLOAD_PATH}"

    downloadLLVMTool llvm
    # Name of clang package depends on version
    if [[ "${VERSION}" < "7.0.1" ]]; then
    downloadLLVMTool cfe
    else
    downloadLLVMTool clang
    fi
    if [ "${COMPILER_RT_BUILD}" == "${TRUE}" ]; then
    downloadLLVMTool compiler-rt
    fi
    if [ "${LLD_BUILD}" == "${TRUE}" ]; then
    downloadLLVMTool lld
    fi
    if [ "${LIBCXX_BUILD}" == "${TRUE}" ]; then
    downloadLLVMTool libcxx
    downloadLLVMTool libcxxabi
    fi

    return 0
}


extractToDir()
{
    TOOL="$1"
    DIR="$2"

    if [ ! -f "${DIR}/.extract_${TOOL}_success" ]; then
        tar -xJvf "${DOWNLOAD_PATH}/${TOOL}-${VERSION}.src.tar.xz"
        if [ -d "${DIR}" ]; then
            rm -rf "${DIR}"
        fi
        mv "${TOOL}-${VERSION}.src" "${DIR}"
        touch "${DIR}/.extract_${TOOL}_success"
    fi

    return 0
}


extractSourceCode()
{
    extractToDir llvm "${LLVM_PATH}"
    # Name of clang package depends on version
    if [[ "${VERSION}" < "7.0.1" ]]; then
    extractToDir cfe "${LLVM_PATH}/tools/clang"
    else
    extractToDir clang "${LLVM_PATH}/tools/clang"
    fi
    if [ "${COMPILER_RT_BUILD}" == "${TRUE}" ]; then
    extractToDir compiler-rt "${LLVM_PATH}/projects/compiler-rt"
    fi
    if [ "${LLD_BUILD}" == "${TRUE}" ]; then
    extractToDir lld "${LLVM_PATH}/tools/lld"
    fi
    if [ "${LIBCXX_BUILD}" == "${TRUE}" ]; then
    extractToDir libcxx "${LLVM_PATH}/projects/libcxx"
    extractToDir libcxxabi "${LLVM_PATH}/projects/libcxxabi"
    fi

    return 0
}


buildCode()
{
    CCACHE_PARAM=""
    if [ "${ENABLE_CCACHE}" == "${TRUE}" ]; then
        CCACHE_PARAM="--enable-ccache"
    fi
    CROSS_COMPILE_PARAM=""
    if [ "${CROSS_COMPILE}" == "${TRUE}" ]; then
        CROSS_COMPILE_PARAM="--cross-compile"
    fi
    DEBUG_PARAM=""
    if [ "${BUILD_TYPE}" == "debug" ]; then
        DEBUG_PARAM="--debug"
    fi

    ./buildLLVM.sh --build-path="${BUILD_PATH}" \
                   "${CCACHE_PARAM}" \
                   "${CROSS_COMPILE_PARAM}" \
                   "${DEBUG_PARAM}" \
                   --install-path="${INSTALL_PATH}" \
                   --jobs="${NUM_THREADS}" \
                   --llvm-path="${LLVM_PATH}" \
                   --sysroot="${SYSROOT}"
}


runTest()
{
    cd "${BUILD_PATH}"
    make -j"${NUM_THREADS}" check-clang
    return "$?"
}


main()
{
    checkUtils
    parseArgs "$@"
    downloadPublicKey
    downloadSourceCode
    extractSourceCode
    buildCode

    if [ "${TEST}" == "${TRUE}" ]; then
        runTest
    fi

    exit "$?"
}

main "$@"


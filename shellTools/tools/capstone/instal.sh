#!/bin/bash

CAPSTONE_ROOT="capstone"
CAPSTONE_BUILD_DIR="${CAPSTONE_ROOT}/build"
INSTALL_PATH="${HOME}/.local"
BUILD_ARCH="AArch64;ARM;Mips;PowerPC;X86"
BUILD_TYPE="Release"
SHARED_LIBS="ON"

downloadSrcCode()
{
    git clone "https://github.com/aquynh/capstone"
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
    downloadSrcCode
    buildCapstone
    installCapstone
}

main "$@"


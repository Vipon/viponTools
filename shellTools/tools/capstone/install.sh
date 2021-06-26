#!/bin/bash

CAPSTONE_ROOT="capstone"
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
    downloadSrcCode
    buildCapstone
    installCapstone
}

main "$@"


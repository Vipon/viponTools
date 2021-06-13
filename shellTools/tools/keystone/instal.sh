#!/bin/bash

KEYSTONE_ROOT="keystone"
KEYSTONE_BUILD_DIR="${KEYSTONE_ROOT}/build"
INSTALL_PATH="${HOME}/.local"
BUILD_ARCH="AArch64;ARM;Mips;PowerPC;X86"
BUILD_TYPE="Release"
SHARED_LIBS="ON"

downloadSrcCode()
{
    git clone "https://github.com/keystone-engine/keystone"
}

buildKeystone()
{
    mkdir "${KEYSTONE_BUILD_DIR}"
    cd "${KEYSTONE_BUILD_DIR}"
    cmake -DCMAKE_BUILD_TYPE="${BUILD_TYPE}"       \
          -DBUILD_SHARED_LIBS="${SHARED_LIBS}"     \
          -DLLVM_TARGETS_TO_BUILD="${BUILD_ARCH}"  \
          -DCMAKE_INSTALL_PREFIX="${INSTALL_PATH}" \
          -G "Unix Makefiles"                      \
          ..
    make -j8
}

installKeystone()
{
    cd "${KEYSTONE_BUILD_DIR}"
    make install
}

main()
{
    downloadSrcCode
    buildKeystone
    installKeystone
}

main "$@"


#!/bin/bash

SCRIPT_DIR="$(dirname "${BASH_SOURCE[0]}")"
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
    downloadCmakeSrcCode
    buildCmake
    installCmake
}

main "$@"


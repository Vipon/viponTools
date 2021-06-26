#!/bin/bash

SCRIPT_DIR="$(dirname "${BASH_SOURCE[0]}")"
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
    downloadCCacheSrcCode
    buildCCache
    installCCache
}

main "$@"


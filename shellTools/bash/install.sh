#!/bin/bash

SCRIPT_DIR="$(dirname "${BASH_SOURCE[0]}")"
source "${SCRIPT_DIR}/../libCommon.sh"

installBashrc()
{
    cd "${SCRIPT_DIR}"
    cat ".bashrc" >> "${HOME}/.bashrc"
}

main()
{
    installBashrc
}

main "$@"


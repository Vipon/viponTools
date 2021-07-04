#!/bin/bash

source "$(dirname "${BASH_SOURCE[0]}")/libosRelease.sh"

OS=`uname`

getOsName()
{
    if [ "${OS_RELEASE}" != "" ]; then
        echo "$(getOsNameFromOsRelease)"
    else
        echo ""
    fi
}

getOsVersion()
{
    if [ "${OS_RELEASE}" != "" ]; then
        echo "$(getVersionFromOsRelease)"
    else
        echo ""
    fi
}

execIfMacOsX()
{
    if [[ "$OS" == "Darwin" ]]; then
        "$1"
    fi
}

execIfLinux()
{
    if [[ "$OS" == "Linux" ]]; then
        "$1"
    fi
}


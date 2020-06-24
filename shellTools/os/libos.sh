#!/bin/bash

source "$(dirname "${BASH_SOURCE[0]}")/libosRelease.sh"

getOsName()
{
    if [ "${OS_RELEASE}" != "" ]; then
        echo "$(getOsNameFromOsRelease)"
    fi

    echo ""
}

getOsVersion()
{
    if [ "${OS_RELEASE}" != "" ]; then
        echo "$(getVersionFromOsRelease)"
    fi

    echo ""
}


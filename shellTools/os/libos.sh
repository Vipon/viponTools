#!/bin/bash

source "$(dirname "${BASH_SOURCE[0]}")/libosRelease.sh"

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


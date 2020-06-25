#!/bin/bash

getOsReleaseFile()
{
    if [ -f "/etc/os-release" ]; then
        echo "/etc/os-release"
    elif [ -f "/usr/lib/os-release" ]; then
        echo "/usr/lib/os-release"
    else
        echo ""
    fi
}

OS_RELEASE="$(getOsReleaseFile)"

getOsNameFromOsRelease()
{
    cat "${OS_RELEASE}" | grep -oP "^NAME=\"\K(\S[^\"]*)"
}

getVersionFromOsRelease()
{
    cat "${OS_RELEASE}" | grep -oP "^VERSION_ID=\"\K(\S[^\"]*)"
}


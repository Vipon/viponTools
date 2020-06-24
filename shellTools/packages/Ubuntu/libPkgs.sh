#!/bin/bash

getListOfInstalledPkgs()
{
    dpkg-query --showformat '${binary:Package}\n' --show
}

installPkgs()
{
    # $1 - file with list of packages
    sudo xargs -a "$1" apt-get install
}


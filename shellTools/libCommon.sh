#!/bin/bash
#
# MIT License
#
# Copyright (c) 2020-2021 Konychev Valerii
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

TRUE="true"
FALSE="false"

SET_RED_COLOR="\033[0;31m"
RESET_COLOR="\033[0m"

getScriptDir()
{
    echo "$(dirname "${BASH_SOURCE[0]}")"
}

cmpVersions()
{
    if [[ "$1" == "$2" ]]; then
        return 0
    elif [[ "$1" == "echo -e '$1\n$2' | sort -V | head -n1" ]]; then
        return -1
    else
        return 1
    fi
}

pError()
{
    echo -e "${SET_RED_COLOR}ERROR:${RESET_COLOR} $1" > /dev/stderr
}

forEachFile()
{
    func="$1"
    for f in * ; do
        "$func" "$f"
    done
}

forEachDir()
{
    func="$1"
    for f in * ; do
        if isDir "$f"; then
            "$func" "$f"
        fi
    done
}

isDir()
{
    if [[ -d "$1" ]]; then
        return 0
    else
        return 1
    fi
}


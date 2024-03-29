#!/bin/bash
#
# MIT License
#
# Copyright (c) 2020-2021 Konychev Valera
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

SCRIPT_DIR="$(realpath $(dirname "${BASH_SOURCE[0]}"))"
source "${SCRIPT_DIR}/libCommon.sh"

OS=`uname`
TOOLS_DIR="${SCRIPT_DIR}/tools"

macOsXPreinstall()
{
    brew install coreutils
    brew install npm
}

preinstall()
{
    if [[ "$OS" == "Darwin" ]]; then
        macOsXPreinstall
    elif  [[ "$OS" == "Linux" ]]; then
        return
    else
        exit 1
    fi
}

installTool()
{
    "$1/install.sh"
}

installTools()
{
    cd "${TOOLS_DIR}"
    forEachDir installTool
}

main()
{
    preinstall()
    installTools
}

main "$@"


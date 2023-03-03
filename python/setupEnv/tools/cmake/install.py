#!/usr/bin/env python3
#
# MIT License
#
# Copyright (c) 2022 Konychev Valera
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

import subprocess

from os.path import dirname, realpath
import sys
curDir = dirname(realpath(__file__))
vpyDir = dirname(dirname(dirname(curDir)))
sys.path.append(vpyDir)

from vpy.net import downloadFile
from vpy.os import execForOs, getForOs
from vpy.installArgs import parseInstallArgs
import vpy.installArgs as vpy

vpy.INSTALL_VERSION = '3.24.1'

WIN_URL = None
WIN_FN = None

MAC_URL = None
MAC_FN = None

LINUX_URL = None
LINUX_FN = None

def parseArgs():
    args = parseInstallArgs('Install cmake.')

    global WIN_URL
    WIN_URL = f'https://github.com/Kitware/CMake/releases/download/v{vpy.INSTALL_VERSION}/cmake-{vpy.INSTALL_VERSION}-windows-x86_64.msi'
    global WIN_FN
    WIN_FN = f'cmake-{vpy.INSTALL_VERSION}-windows-x86_64.msi'

    return args

def downloadCmake():
    url = getForOs(
            linux = LINUX_URL,
            mac = MAC_URL,
            win = WIN_URL
          )
    fn = getForOs(
            linux = LINUX_FN,
            mac = MAC_FN,
            win = WIN_FN
         )

    downloadFile(url, fn)

def installCmakeForLinux():
    # !TODO: Add linux installation
    return

def installCmakeForMac():
    # !TODO: Add mac os installation
    return

def installCmakeForWin(args):
    com = [ 'MsiExec'
          , '/i'
          , WIN_FN
          , 'ADD_CMAKE_TO_PATH=User'
          , '/passive'
    ]

    if args.install_prefix is not None:
        com += [f'INSTALL_ROOT={vpy.INSTALL_PREFIX}']

    subprocess.check_call(com)

def installCmake(args):
    execForOs(
        linux = installCmakeForLinux,
        mac = installCmakeForMac,
        win = installCmakeForWin,
        wargs = args
    )

def main():
    args = parseArgs()
    downloadCmake()
    installCmake(args)

if __name__ == '__main__':
    main()


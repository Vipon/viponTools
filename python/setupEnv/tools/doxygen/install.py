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
from vpy.os import execForOs
from vpy.installArgs import installArgs

VERSION = '1.9.5'
BASE_URL = 'https://sourceforge.net/projects/doxygen/files/rel-'
INSTALL_DIR = None

WIN_URL = None
WIN_BIN = 'doxygen-setup.exe'

LINUX_URL = None
LINUX_ARCHIVE = None

MAC_URL = None
MAC_BIN = 'Doxygen.dmg'

def parseArgs():
    args = installArgs('Install doxygen.')

    global VERSION
    if args.install_version == None:
        args.install_version = VERSION
    else:
        VERSION = args.install_version

    global WIN_URL
    WIN_URL = f'{BASE_URL}{VERSION}/doxygen-{VERSION}-setup.exe'
    global MAC_URL
    MAC_URL = f'{BASE_URL}{VERSION}/Doxygen-{VERSION}.dmg'

    global INSTALL_DIR
    if args.install_dir != None:
        INSTALL_DIR = args.install_dir

    return args

def downloadDoxygenForLinux():
    return

def downloadDoxygenForWin():
    url = WIN_URL
    fn = WIN_BIN
    print(WIN_URL)
    downloadFile(url, fn)

def downloadDoxygenForMac():
    url = MAC_URL
    fn = MAC_BIN
    downloadFile(url, fn)

def downloadDoxygen():
    execForOs(
        linux = downloadDoxygenForLinux,
        mac = downloadDoxygenForMac,
        win = downloadDoxygenForWin
    )

def installDoxygenLinux():
    # !TODO: Add linux installation
    return

def installDoxygenMac():
    # !TODO: Add mac os installation
    return

def installDoxygenWin():
    args = [
          WIN_BIN
        , f'{"/DIR=" + INSTALL_DIR if INSTALL_DIR is not None else ""}'
        , '/SILENT'
    ]

    subprocess.check_call(args)

def installDoxygen():
    execForOs(
        linux = installDoxygenLinux,
        mac = installDoxygenMac,
        win = installDoxygenWin
    )

def main():
    parseArgs()
    downloadDoxygen()
    installDoxygen()

if __name__ == '__main__':
    main()


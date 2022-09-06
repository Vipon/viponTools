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
libDir = dirname(dirname(curDir))
sys.path.append(libDir)

from lib.net import downloadFile
from lib.os import execForOs, getForOs


WIN_URL = 'https://code.visualstudio.com/sha/download?build=stable&os=win32-x64-user'
WIN_FN = 'VSCodeUserSetup-x64.exe'

MAC_URL = ''
MAC_FN = ''

LINUX_URL = ''
LINUX_FN = ''

def downloadVSCode():
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

def installVSCodeForLinux():
    return 

def installVSCodeForMac():
    return 

def installVSCodeForWin():
    subprocess.check_call(
        [ WIN_FN
        , '/SILENT'
        ]
    )

def installVSCode():
    execForOs(
        linux = installVSCodeForLinux,
        mac = installVSCodeForMac,
        win = installVSCodeForWin
    )

def main():
    downloadVSCode()
    installVSCode()

if __name__ == '__main__':
    main()
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
from lib.installArgs import installArgs

WIN_URL = 'https://github.com/msys2/msys2-installer/releases/download/2022-09-04/msys2-x86_64-20220904.exe'
WIN_FN = 'msys2.exe'

MAC_URL = ''
MAC_FN = ''

LINUX_URL = ''
LINUX_FN = ''

def downloadMSYS2():
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

def installMSYS2ForLinux():
    return

def installMSYS2ForMac():
    return

def installMSYS2ForWin():
    args = [ WIN_FN
           , '--default-answer'
           , '--confirm-command'
           , '--accept-licenses'
           , '--root=C:\msys64'
           , 'install'
           ]

    print(args)
    subprocess.check_call(args)

def installMSYS2():
    execForOs(
        linux = installMSYS2ForLinux,
        mac = installMSYS2ForMac,
        win = installMSYS2ForWin
    )

def main():
    downloadMSYS2()
    installMSYS2()

if __name__ == '__main__':
    main()


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

from os.path import dirname, realpath
import sys
curDir = dirname(realpath(__file__))
vpyDir = dirname(dirname(dirname(curDir)))
sys.path.append(vpyDir)

from vpy.cmd import execCmd, CalledProcessError
from vpy.net import downloadFile
from vpy.os import execForOs, getForOs
from vpy.installArgs import parseInstallArgs
import vpy.installArgs as vpy

WIN_URL = 'https://github.com/msys2/msys2-installer/releases/download/2022-09-04/msys2-x86_64-20220904.exe'
WIN_FN = 'msys2.exe'
WIN_NOT_FAULT_ERROR = 'The directory you selected already exists and contains an installation.'
vpy.DEFAULT_WIN_INSTALL_PREFIX = 'C:\msys64'

MAC_URL = ''
MAC_FN = ''

LINUX_URL = ''
LINUX_FN = ''

def parseArgs():
    args = parseInstallArgs('Install MSYS2.')
    return args

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
           , f'--root={vpy.INSTALL_PREFIX}'
           , 'install'
           ]

    try:
        execCmd(args, captureOut=True)
    except CalledProcessError as execError:
        print(execError.output.decode("utf-8"), flush=True)
        if WIN_NOT_FAULT_ERROR in execError.output.decode("utf-8"):
            return
        else:
            exit(-1)


def installMSYS2():
    execForOs(
        linux = installMSYS2ForLinux,
        mac = installMSYS2ForMac,
        win = installMSYS2ForWin
    )

def main():
    parseArgs()
    downloadMSYS2()
    installMSYS2()

if __name__ == '__main__':
    main()


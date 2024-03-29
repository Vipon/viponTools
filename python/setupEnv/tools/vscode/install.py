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

from vpy.cmd import execCmd
from vpy.net import downloadFile
from vpy.os import execForOs, getForOs
from vpy.installArgs import parseInstallArgs
import vpy.installArgs as vpy

WIN_URL = 'https://code.visualstudio.com/sha/download?build=stable&os=win32-x64-user'
WIN_FN = 'VSCodeUserSetup-x64.exe'

MAC_URL = ''
MAC_FN = ''

LINUX_URL = ''
LINUX_FN = ''

def parseArgs():
    args = parseInstallArgs('Install vscode.')
    return args

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

def installVSCodeForWin(args):
    com = [ WIN_FN
          , '/SILENT'
    ]

    if args.install_prefix is not None:
        com += [f'/DIR={vpy.INSTALL_PREFIX}']

    execCmd(com)

def installVSCode(args):
    execForOs(
        linux = installVSCodeForLinux,
        mac = installVSCodeForMac,
        win = installVSCodeForWin,
        wargs = args
    )

def main():
    args = parseArgs()
    downloadVSCode()
    installVSCode(args)

if __name__ == '__main__':
    main()


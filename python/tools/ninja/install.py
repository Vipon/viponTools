#!/usr/bin/env python3
#
# MIT License
#
# Copyright (c) 2021 Konychev Valera
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
libDir = dirname(dirname(curDir))
sys.path.append(libDir)

from lib.net import downloadFile
from lib.os import execForOs, getForOs, appendPath
from lib.file import extractFile, mvFile
from lib.dir import createDir

VERSION = '1.10.2'
BASE_URL = 'https://github.com/ninja-build/ninja/releases/download'

WIN_ARCHIVE = 'ninja-win.zip'
WIN_EXEC = 'ninja.exe'
WIN_INSTALL_PATH = 'C:\\Ninja'

LINUX_ARCHIVE = 'ninja-linux.zip'
LINUX_EXEC = 'ninja'
LINUX_INSTALL_PATH = '~/.local/bin'

MAC_ARCHIVE = 'ninja-mac.zip'
MAC_EXEC = 'ninja'
MAC_INSTALL_PATH = '~/.local/bin'

def downloadNinjaForLinux():
    url = f'{BASE_URL}/v{VERSION}/{LINUX_ARCHIVE}'
    fn = LINUX_ARCHIVE
    downloadFile(url, fn)

def downloadNinjaForWin():
    url = f'{BASE_URL}/v{VERSION}/{WIN_ARCHIVE}'
    fn = WIN_ARCHIVE
    downloadFile(url, fn)

def downloadNinjaForMac():
    url = f'{BASE_URL}/v{VERSION}/{MAC_ARCHIVE}'
    fn = MAC_ARCHIVE
    downloadFile(url, fn)

def downloadNinja():
    execForOs(
        linux = downloadNinjaForLinux,
        mac = downloadNinjaForMac,
        win = downloadNinjaForWin
    )

def extractNinja():
    arch =  getForOs(
                linux = LINUX_ARCHIVE,
                mac = MAC_ARCHIVE,
                win = WIN_ARCHIVE
            )
    extractFile(arch)

def installNinja():
    path =  getForOs(
                linux = LINUX_INSTALL_PATH,
                mac = MAC_INSTALL_PATH,
                win = WIN_INSTALL_PATH
            )
    execFile =  getForOs(
                linux = LINUX_EXEC,
                mac = MAC_EXEC,
                win = WIN_EXEC
            )
    createDir(path)
    mvFile(execFile, path)
    appendPath(path)

def main():
    downloadNinja()
    extractNinja()
    installNinja()

if __name__ == '__main__':
    main()


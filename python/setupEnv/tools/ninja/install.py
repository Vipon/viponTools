#!/usr/bin/env python3
#
# MIT License
#
# Copyright (c) 2021-2026 Konychev Valera
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

from packaging.version import Version
from os.path import dirname, realpath
import sys
curDir = dirname(realpath(__file__))
vpyDir = dirname(dirname(dirname(curDir)))
sys.path.append(vpyDir)

from vpy.net import downloadFile
from vpy.os import execForOs, getForOs, appendPath, isMacOsX, installPkg
from vpy.file import extractFile, mvFile
from vpy.dir import createDir
from vpy.installArgs import parseInstallArgs
import vpy.ninja as ninja
import vpy.installArgs as vpy

vpy.INSTALL_VERSION = '1.10.2'
BASE_URL = 'https://github.com/ninja-build/ninja/releases/download'

WIN_ARCHIVE = 'ninja-win.zip'
WIN_EXEC = 'ninja.exe'

LINUX_ARCHIVE = 'ninja-linux.zip'
LINUX_EXEC = 'ninja'

MAC_ARCHIVE = 'ninja-mac.zip'
MAC_EXEC = 'ninja'

def parseArgs():
    args = parseInstallArgs('Install ninja.')
    return args

def downloadNinjaForLinux():
    url = f'{BASE_URL}/v{vpy.INSTALL_VERSION}/{LINUX_ARCHIVE}'
    fn = LINUX_ARCHIVE
    downloadFile(url, fn)

def downloadNinjaForWin():
    url = f'{BASE_URL}/v{vpy.INSTALL_VERSION}/{WIN_ARCHIVE}'
    fn = WIN_ARCHIVE
    downloadFile(url, fn)

def downloadNinjaForMac():
    url = f'{BASE_URL}/v{vpy.INSTALL_VERSION}/{MAC_ARCHIVE}'
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
    path =  vpy.INSTALL_BIN_DIR
    execFile =  getForOs(
                linux = LINUX_EXEC,
                mac = MAC_EXEC,
                win = WIN_EXEC
            )
    createDir(path)
    mvFile(execFile, path)
    if isLinux():
        os.chmod(path, 0o744)
    appendPath(path)

def main():
    args = parseArgs()
    cur_version = Version(ninja.get_installed_version())
    new_version = Version(vpy.INSTALL_VERSION)
    if cur_version >= new_version:
        return

    if isMacOsX() and args.default:
        installPkg('ninja')
    else:
        downloadNinja()
        extractNinja()
        installNinja()

if __name__ == '__main__':
    main()


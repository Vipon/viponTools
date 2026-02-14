#!/usr/bin/env python3
#
# MIT License
#
# Copyright (c) 2022-2026 Konychev Valera
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

import sys
from packaging.version import Version
from os.path import dirname, realpath, join
curDir = dirname(realpath(__file__))
vpyDir = dirname(dirname(dirname(curDir)))
sys.path.append(vpyDir)

from vpy.os import execForOs, getForOs, appendPath, isWin, installPkg
from vpy.dir import createDir, mvDir
from vpy.net import downloadFile
from vpy.file import extractFile
from vpy.installArgs import parseInstallArgs
import vpy.ccache as ccache
import vpy.installArgs as vpy

import vpy.brew as brew

vpy.INSTALL_VERSION = '4.6.3'

WIN_URL = None
WIN_ARCHIVE = None
WIN_CCACHE_DIR = None
WIN_INSTALL_PATH = None

MAC_URL = ''
MAC_ARCHIVE = ''
MAC_INSTALL_PATH = '~/.local/bin'

LINUX_URL = ''
LINUX_ARCHIVE = ''
LINUX_INSTALL_PATH = '~/.local/bin'

def parseArgs():
    args = parseInstallArgs('Install ccache.')

    global WIN_URL
    WIN_URL = f'https://github.com/ccache/ccache/releases/download/v{vpy.INSTALL_VERSION}/ccache-{vpy.INSTALL_VERSION}-windows-x86_64.zip'
    global WIN_ARCHIVE
    WIN_ARCHIVE = f'ccache-{vpy.INSTALL_VERSION}-windows-x86_64.zip'
    global WIN_CCACHE_DIR
    WIN_CCACHE_DIR = f'ccache-{vpy.INSTALL_VERSION}-windows-x86_64'

    global WIN_INSTALL_PATH
    WIN_INSTALL_PATH = join(vpy.INSTALL_BIN_DIR, 'ccache')

    return args

def downloadCCache():
    url = getForOs(
            linux = LINUX_URL,
            mac = MAC_URL,
            win = WIN_URL
          )
    arch = getForOs(
            linux = LINUX_ARCHIVE,
            mac = MAC_ARCHIVE,
            win = WIN_ARCHIVE
         )

    downloadFile(url, arch)

def extractCCache():
    arch =  getForOs(
                linux = LINUX_ARCHIVE,
                mac = MAC_ARCHIVE,
                win = WIN_ARCHIVE
            )
    extractFile(arch)

def createBinDir():
    createDir(vpy.INSTALL_BIN_DIR)

def installCCacheForLinux():
    return

def installCCacheForMac():
    return

def installCCacheForWin():
    mvDir(WIN_CCACHE_DIR, WIN_INSTALL_PATH)

def installCCache():
    createBinDir()

    path = getForOs(
            linux = LINUX_INSTALL_PATH,
            mac = MAC_INSTALL_PATH,
            win = WIN_INSTALL_PATH
           )

    execForOs(
        linux = installCCacheForLinux,
        mac = installCCacheForMac,
        win = installCCacheForWin
    )

    appendPath(path)

def main():
    args = parseArgs()
    cur_version = Version(ccache.get_installed_version())
    new_version = Version(vpy.INSTALL_VERSION)
    if cur_version >= new_version:
        return

    if isWin() or not args.default:
        downloadCCache()
        extractCCache()
        installCCache()
    else:
        installPkg('ccache')

if __name__ == '__main__':
    main()


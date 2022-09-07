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

import sys
from os.path import dirname, realpath
curDir = dirname(realpath(__file__))
libDir = dirname(dirname(curDir))
sys.path.append(libDir)

from lib.os import execForOs, getForOs, appendPath
from lib.dir import createDir, mvDir
from lib.net import downloadFile
from lib.file import extractFile

VERSION = '4.6.3'

WIN_URL = f'https://github.com/ccache/ccache/releases/download/v{VERSION}/ccache-{VERSION}-windows-x86_64.zip'
WIN_ARCHIVE = f'ccache-{VERSION}-windows-x86_64.zip'
WIN_BIN_DIR = 'C:\\bin'
WIN_CCACHE_DIR = f'ccache-{VERSION}-windows-x86_64'
WIN_INSTALL_PATH = f'{WIN_BIN_DIR}\\ccache'

MAC_URL = ''
MAC_ARCHIVE = ''
MAC_BIN_DIR = ''
MAC_INSTALL_PATH = ''

LINUX_URL = ''
LINUX_ARCHIVE = ''
LINUX_BIN_DIR = ''
LINUX_INSTALL_PATH = ''

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
    binDir = getForOs(
                linux = LINUX_BIN_DIR,
                mac = MAC_BIN_DIR,
                win = WIN_BIN_DIR
             )
    createDir(binDir)

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
    downloadCCache()
    extractCCache()
    installCCache()

if __name__ == '__main__':
    main()


#!/usr/bin/env python3
#
# MIT License
#
# Copyright (c) 2023 Konychev Valera
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
import os
from os.path import dirname, realpath, join
curDir = dirname(realpath(__file__))
vpyDir = dirname(dirname(dirname(curDir)))
sys.path.append(vpyDir)

from vpy.os import installPkg
from vpy.dir import createDir
from vpy.net import downloadFile
from vpy.file import extractFile
from vpy.cmd import execCmd
from vpy.installArgs import parseInstallArgs
import vpy.installArgs as vpy

vpy.INSTALL_VERSION = '3.0.5'

CAPSTONE_SRC_URL = None
CAPSTONE_SRC_ROOT = None
CAPSTONE_BUILD_DIR = None
CAPSTONE_SRC_ARCHIVE = None

def parseArgs():
    args = parseInstallArgs('Install capstone.')

    global CAPSTONE_SRC_URL
    CAPSTONE_SRC_URL = f'https://github.com/capstone-engine/capstone/archive/refs/tags/{vpy.INSTALL_VERSION}.zip'

    global CAPSTONE_SRC_ARCHIVE
    CAPSTONE_SRC_ARCHIVE = join(dirname(__file__), f'capstone-{vpy.INSTALL_VERSION}.zip')

    global CAPSTONE_SRC_ROOT
    CAPSTONE_SRC_ROOT = join(dirname(__file__), f'capstone-{vpy.INSTALL_VERSION}')

    global CAPSTONE_BUILD_DIR
    CAPSTONE_BUILD_DIR = join(CAPSTONE_SRC_ROOT, 'build')

    return args

def downloadSrcCapstone():
    downloadFile(CAPSTONE_SRC_URL, CAPSTONE_SRC_ARCHIVE)

def extractSrcCapstone():
    extractFile(CAPSTONE_SRC_ARCHIVE, dirname(__file__))

def buildAndInstallCapstone():
    cwd = os.getcwd()

    createDir(CAPSTONE_BUILD_DIR)
    os.chdir(CAPSTONE_BUILD_DIR)
    cmd = [ 'cmake'
          , '-DCMAKE_BUILD_TYPE=Release'
          , '-DCAPSTONE_X86_ATT_DISABLE=ON'
          , f'-DCMAKE_INSTALL_PREFIX={vpy.INSTALL_PREFIX}'
          , '..'
          ]
    execCmd(cmd)

    cmd = [ 'make', '-j' ]
    execCmd(cmd)

    cmd = [ 'make', 'install' ]
    execCmd(cmd)

    os.chdir(cwd)

def main():
    args = parseArgs()
    if not args.default:
        downloadSrcCapstone()
        extractSrcCapstone()
        buildAndInstallCapstone()
    else:
        installPkg('capstone')

if __name__ == '__main__':
    main()


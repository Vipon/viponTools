#!/usr/bin/env python3
#
# MIT License
#
# Copyright (c) 2023-2026 Konychev Valera
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
from packaging.version import Version
from os.path import dirname, realpath, join
curDir = dirname(realpath(__file__))
vpyDir = dirname(dirname(dirname(curDir)))
sys.path.append(vpyDir)

from vpy.os import installPkg, isWin
from vpy.dir import createDir, is_dir_exist, rm_dir
from vpy.net import downloadFile
from vpy.file import extractFile, cpFile
from vpy.cmd import execCmd
from vpy.installArgs import parseInstallArgs
import vpy.cmake as cmake
import vpy.git as git
import vpy.installArgs as vpy

vpy.INSTALL_VERSION = '0.9.2'

KEYSTONE_SRC_URL = None
KEYSTONE_SRC_ROOT = None
KEYSTONE_BUILD_DIR = None
KEYSTONE_SRC_ARCHIVE = None

def parseArgs():
    args = parseInstallArgs('Install keystone.')

    global KEYSTONE_SRC_URL
    KEYSTONE_SRC_URL = f'https://github.com/keystone-engine/keystone/archive/refs/tags/{vpy.INSTALL_VERSION}.zip'

    global KEYSTONE_SRC_ARCHIVE
    KEYSTONE_SRC_ARCHIVE = join(dirname(__file__), f'keystone-{vpy.INSTALL_VERSION}.zip')

    global KEYSTONE_SRC_ROOT
    KEYSTONE_SRC_ROOT = join(dirname(__file__), f'keystone-{vpy.INSTALL_VERSION}')

    global KEYSTONE_BUILD_DIR
    KEYSTONE_BUILD_DIR = join(KEYSTONE_SRC_ROOT, 'build')

    return args

def downloadSrcKeystone():
    downloadFile(KEYSTONE_SRC_URL, KEYSTONE_SRC_ARCHIVE)

def extractSrcKeystone():
    if is_dir_exist(KEYSTONE_SRC_ROOT):
        rm_dir(KEYSTONE_SRC_ROOT)
    extractFile(KEYSTONE_SRC_ARCHIVE, dirname(__file__))

def applyBuiltPatch():
    ''' Because keystone project haven't being update since 2020 CMake files
    are too out-dated.
    '''
    prev_cwd = os.getcwd()
    os.chdir(KEYSTONE_SRC_ROOT)
    # git apply works only in the git repo
    git.init()
    patch_file = join(KEYSTONE_SRC_ROOT, '..', 'fix_build.diff')
    git.apply(patch_file)
    os.chdir(prev_cwd)

def buildAndInstallKeystone():
    cwd = os.getcwd()

    createDir(KEYSTONE_BUILD_DIR)
    os.chdir(KEYSTONE_BUILD_DIR)

    cmake_version = Version(cmake.get_installed_version())
    if cmake_version >= Version('4.0'):
        applyBuiltPatch()

    cmd = [ 'cmake' ]
    if cmake_version >= Version('3.5'):
        cmd += [ '-DCMAKE_POLICY_VERSION_MINIMUM=3.5' ]
    cmd += [ '-DCMAKE_BUILD_TYPE=Release'
           , '-DBUILD_SHARED_LIBS=OFF'
           , '-DKEYSTONE_X86_ATT_DISABLE=ON'
           , '-DLLVM_TARGETS_TO_BUILD=AArch64;X86'
           , f'-DCMAKE_INSTALL_PREFIX={vpy.INSTALL_PREFIX}'
           , '..'
           ]
    execCmd(cmd)

    if isWin():
        cmd = [ 'cmake', '--build', '.', '--config', 'Release']
    else:
        cmd = [ 'make', '-j' ]
    execCmd(cmd)

    if isWin():
        cmd = [ 'cmake', '--install', '.', '--config', 'Release']
    else:
        cmd = [ 'make', 'install' ]
    execCmd(cmd)

    os.chdir(cwd)

def main():
    args = parseArgs()
    if not args.default:
        downloadSrcKeystone()
        extractSrcKeystone()
        buildAndInstallKeystone()
    else:
        installPkg('keystone')

if __name__ == '__main__':
    main()


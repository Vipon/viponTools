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

from os import chdir, getcwd
from os.path import dirname, realpath, join
import sys
curDir = dirname(realpath(__file__))
vpyDir = dirname(dirname(dirname(curDir)))
sys.path.append(vpyDir)

from vpy.cmd import execCmd
from vpy.os import execForOs, isWin, isMacOsX
from vpy.dir import createDir
from vpy.installArgs import parseInstallArgs
import vpy.brew as brew

import vpy.git as git
import vpy.installArgs as vpy

ARGP_GIT_URL = 'https://github.com/Vipon/win-argp.git'
ARGP_SRC_PATH = realpath(join(dirname(__file__), './win-argp'))
ARGP_BUILD_PATH = join(ARGP_SRC_PATH, 'build')
ARGP_INSTALL_PREFIX = realpath(f'{dirname(__file__)}/../../../../external')

def parseArgs():
    args = parseInstallArgs('Install gnulib.')

    global ARGP_INSTALL_PREFIX
    if args.install_prefix is not None:
        ARGP_INSTALL_PREFIX = vpy.INSTALL_PREFIX

    return args

def downloadArgpSrcCode():
    def downloadArgpSrcCodeWin():
        git.clone([ARGP_GIT_URL, ARGP_SRC_PATH])

    def downloadArgpSrcCodeLinux():
        # It's on Linux by default
        return

    def downloadArgpSrcCodeMac():
        # Dont need, install from brew.
        return

    execForOs(
        linux = downloadArgpSrcCodeLinux,
        mac = downloadArgpSrcCodeMac,
        win = downloadArgpSrcCodeWin
    )

def buildArgp():
    def buildArgpLinux():
        # It's on Linux by default
        return

    def buildArgpMac():
        # Don't need to build. Should be installed from brew
        return

    def buildArgpWin():
        cwd = getcwd()
        createDir(ARGP_BUILD_PATH)
        chdir(ARGP_BUILD_PATH)

        cmd = [ 'cmake'
              , f'-DWIN_ARGP_INSTALL_PREFIX={ARGP_INSTALL_PREFIX}'
              , '..'
              ]
        execCmd(cmd)

        cmd = [ 'cmake', '--build', '.', '--config Release']
        execCmd(cmd)

        chdir(cwd)

    execForOs(
        linux = buildArgpLinux,
        mac = buildArgpMac,
        win = buildArgpWin
    )

def installArgp():
    def installArgpLinux():
        # It's on Linux by default
        return

    def installArgpMac():
        brew.install('argp-standalone')
        return

    def installArgpWin():
        cwd = getcwd()
        chdir(ARGP_BUILD_PATH)

        cmd = [ 'cmake', '--install', '.', '--config Release']
        execCmd(cmd)

        chdir(cwd)

    execForOs(
        linux = installArgpLinux,
        mac = installArgpMac,
        win = installArgpWin
    )

def main():
    args = parseArgs()
    if isWin() or not args.default:
        downloadArgpSrcCode()
        buildArgp()
        installArgp()
    elif isMacOsX():
        brew.install('argp-standalone')

if __name__ == '__main__':
    main()


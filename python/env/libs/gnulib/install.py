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
vpyDir = dirname(dirname(dirname(curDir)))
sys.path.append(vpyDir)

from vpy.os import execForOs, getForOs

import vpy.git as git
import vpy.pacman as pacman

GNULIB_GIT_URL = 'https://git.savannah.gnu.org/git/gnulib.git'
GNULIB_SRC_PATH = realpath('./gnulib')
GNULIB_INSTALL_PATH = realpath('./../../../../external/gnulib')

GNULIB_MSYS_DEPS = [ 'gcc'
                   , 'make'
                   , 'm4'
                   , 'autoconf'
                   , 'automake'
                   , 'bash'
                   , 'coreutils'
                   , 'diffutils'
                   , 'patch'
                   , 'grep'
                   , 'gawk'
                   , 'gettext'
                   , 'bison'
                   , 'gperf'
                   , 'texinfo'
                   , 'sed'
                   , 'libtool'
                   , 'tar'
                   , 'flex'
                   , 'python3'
                   , 'unzip'
                   , 'rsync'
                   , 'mingw64/mingw-w64-x86_64-emacs'
                   , 'mingw-w64-x86_64-toolchain'
                   , 'mingw64/mingw-w64-x86_64-python'
                   , 'openssh'
                   ]

def installGnuLibDepsForWin():
    pacman.install(GNULIB_MSYS_DEPS)

def installGnuLibDepsForLinux():
    # It's on Linux by default
    return

def installGnuLibDepsForMac():
    # !TODO: add install via brew
    return

def installGnuLibDeps():
    execForOs(
        linux = installGnuLibDepsForLinux,
        mac = installGnuLibDepsForMac,
        win = installGnuLibDepsForWin
    )

def downloadGnuLibSrcCode():
    git.clone([GNULIB_GIT_URL, GNULIB_SRC_PATH])

def buildGnuLib():
    shell = pacman.PACMAN_SHELL

    args = [ shell
           , '-lc'
           , ' '.join(
                    [ 'cd'
                    , f'"{GNULIB_SRC_PATH}"'
                    , '&&'
                    , './gnulib-tool'
                    , '--create-megatestdir'
                    , '--with-tests'
                    , '--dir'
                    , f'"{GNULIB_INSTALL_PATH}"'
                    , 'argp'
                    ]
                )
           ]

    subprocess.check_call(args)

    args = [ shell
           , '-lc'
           , ' '.join(
                    [ 'cd'
                    , f'"{GNULIB_INSTALL_PATH}"'
                    , '&&'
                    , './do-autobuild'
                    ]
                )
           ]

    subprocess.check_call(args)

def main():
    installGnuLibDeps()
    downloadGnuLibSrcCode()
    buildGnuLib()

if __name__ == '__main__':
    main()


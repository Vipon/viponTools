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

from os import listdir
from os.path import dirname, realpath, join, splitext
import sys
curDir = dirname(realpath(__file__))
vpyDir = dirname(dirname(dirname(curDir)))
sys.path.append(vpyDir)

from vpy.os import execForOs, getForOs
from vpy.dir import createDir
from vpy.file import cpFile

import vpy.git as git
import vpy.pacman as pacman

GNULIB_GIT_URL = 'https://git.savannah.gnu.org/git/gnulib.git'
GNULIB_SRC_PATH = realpath('./gnulib')
GNULIB_BUILD_PATH = realpath('./../../../../external/gnulib')
GNULIB_AFTER_BUILT_PATH = join(GNULIB_BUILD_PATH, 'ALL', 'gllib')
GNULIB_INSTALL_PREFIX = realpath('./../../../../external')
GNULIB_INSTALL_LIB_PATH = join(GNULIB_INSTALL_PREFIX, 'lib')
GNULIB_INSTALL_INCLUDE_PATH = join(GNULIB_INSTALL_PREFIX, 'include')

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

def installGnuLibDeps():
    def installGnuLibDepsForWin():
        pacman.install(GNULIB_MSYS_DEPS)

    def installGnuLibDepsForLinux():
        # It's on Linux by default
        return

    def installGnuLibDepsForMac():
        # Dont need, install from brew.
        return

    execForOs(
        linux = installGnuLibDepsForLinux,
        mac = installGnuLibDepsForMac,
        win = installGnuLibDepsForWin
    )

def downloadGnuLibSrcCode():
    def downloadGnuLibSrcCodeWin():
        git.clone([GNULIB_GIT_URL, GNULIB_SRC_PATH])

    def downloadGnuLibSrcCodeLinux():
        # It's on Linux by default
        return

    def downloadGnuLibSrcCodeMac():
        # Dont need, install from brew.
        return

    execForOs(
        linux = downloadGnuLibSrcCodeLinux,
        mac = downloadGnuLibSrcCodeMac,
        win = downloadGnuLibSrcCodeWin
    )

def buildGnuLib():
    def buildGnuLibLinux():
        # It's on Linux by default
        return

    def buildGnuLibMac():
        # Don't need to build. Should be installed from brew
        return

    def buildGnuLibWin():
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
                        , f'"{GNULIB_BUILD_PATH}"'
                        , 'argp'
                        ]
                    )
            ]

        subprocess.check_call(args)

        args = [ shell
            , '-lc'
            , ' '.join(
                        [ 'cd'
                        , f'"{GNULIB_BUILD_PATH}"'
                        , '&&'
                        , './do-autobuild'
                        ]
                    )
            ]

        subprocess.check_call(args)

    execForOs(
        linux = buildGnuLibLinux,
        mac = buildGnuLibMac,
        win = buildGnuLibWin
    )

def installGnuLib():
    def installGnuLibLinux():
        # It's on Linux by default
        return

    def installGnuLibMac():
        # !TODO: add install via brew
        return

    def installGnuLibWin():
        createDir(GNULIB_INSTALL_LIB_PATH)
        createDir(GNULIB_INSTALL_INCLUDE_PATH)
        for f in listdir(GNULIB_AFTER_BUILT_PATH):
            (_, ext) = splitext(f)
            if ext == '.h':
                cpFile( join(GNULIB_AFTER_BUILT_PATH, f)
                      , join(GNULIB_INSTALL_INCLUDE_PATH, f)
                )
            if ext == '.a' or ext == '.so':
                cpFile( join(GNULIB_AFTER_BUILT_PATH, f)
                      , join(GNULIB_INSTALL_LIB_PATH, f)
                )

    execForOs(
        linux = installGnuLibLinux,
        mac = installGnuLibMac,
        win = installGnuLibWin
    )


def main():
    installGnuLibDeps()
    downloadGnuLibSrcCode()
    buildGnuLib()
    installGnuLib()

if __name__ == '__main__':
    main()


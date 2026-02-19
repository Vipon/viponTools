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

import argparse
from os.path import join, realpath, expanduser
from vpy.os import getForOs

INSTALL_PREFIX = None
INSTALL_VERSION = None
INSTALL_BIN_DIR = None
INSTALL_LIB_DIR = None
INSTALL_INCLUDE_DIR = None
DEFAULT_WIN_INSTALL_PREFIX = 'C:\\'
DEFAULT_MAC_INSTALL_PREFIX = expanduser('~/.local')
DEFAULT_LINUX_INSTALL_PREFIX = expanduser('~/.local')

def installArgs(descr: str):
    parser = argparse.ArgumentParser(description = descr)
    parser.add_argument('--install-version', metavar = 'VERSION', nargs = '?',
            default = None,
            help = 'number of version should be installed')
    parser.add_argument('--install-prefix', metavar = 'DIR_PATH', nargs = '?',
            default = None,
            help = 'path to install directories: bin, lib, include')
    parser.add_argument('--default', action = 'store_true',
            default = False,
            help = 'if flag is set, produce default install: brew, apt etc.')
    parser.add_argument('--plaform', metavar = 'TRIPLET', nargs = '?',
            default = 'host',
            help = 'specify target system triplet')

    return parser.parse_args()

def parseInstallArgs(descr: str):
    args = installArgs(descr)

    global INSTALL_VERSION
    if args.install_version is not None:
        INSTALL_VERSION = args.install_version
    else:
        args.default = True

    global INSTALL_PREFIX
    if args.install_prefix == None:
        INSTALL_PREFIX = getForOs(
                linux = DEFAULT_LINUX_INSTALL_PREFIX,
                mac = DEFAULT_MAC_INSTALL_PREFIX,
                win = DEFAULT_WIN_INSTALL_PREFIX
        )
    else:
        INSTALL_PREFIX = args.install_prefix

    INSTALL_PREFIX = realpath(INSTALL_PREFIX)
    global INSTALL_BIN_DIR
    INSTALL_BIN_DIR = join(INSTALL_PREFIX, 'bin')
    global INSTALL_LIB_DIR
    INSTALL_LIB_DIR = join(INSTALL_PREFIX, 'lib')
    global INSTALL_INCLUDE_DIR
    INSTALL_INCLUDE_DIR = join(INSTALL_PREFIX, 'include')

    return args


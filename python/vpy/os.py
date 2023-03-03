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

import os
import platform

OS = platform.system()

def isLinux():
    return OS == 'Linux'

def isMacOsX():
    return OS == 'Darwin'

def isWin():
    return OS == 'Windows'

def execForOs( linux = None
             , mac   = None
             , win   = None
             , largs = None
             , margs = None
             , wargs = None
             ):
    if isLinux():
        if linux is None:
            raise 'Where is no Linux hadler'
        if largs == None:
            return linux()
        else:
            return linux(largs)
    elif isMacOsX():
        if mac is None:
            raise 'Where is no Mac OS X hadler'
        if margs == None:
            return mac()
        else:
            return mac(margs)
    elif isWin():
        if win is None:
            raise 'Where is no Win hadler'
        if wargs == None:
            return win()
        else:
            return win(wargs)
    else:
        raise 'Unknown OS'

def getForOs(linux=None, mac=None, win=None):
    if isLinux():
        return linux
    elif isMacOsX():
        return mac
    elif isWin():
        return win
    else:
        raise 'Unknown OS'

def appendPath(newPath):
    def appendNixPath():
        return
    def appendWinPath():
        os.system(f'setx path "%path%;{newPath}"')

    execForOs(
        linux = appendNixPath,
        mac = appendNixPath,
        win = appendWinPath
    )

def cd(path: str) -> None:
    os.chdir(path)


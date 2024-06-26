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
if os.name == 'nt':
    import winreg

from . import apt as apt
from . import brew as brew

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

def get_user_path() -> str:
    def get_win_user_path() -> str:
        # Get the current user registry
        root = winreg.ConnectRegistry(None, winreg.HKEY_CURRENT_USER)
        # Go to the environment key
        key = winreg.OpenKey(root, 'Environment', 0, winreg.KEY_ALL_ACCESS)
        # Grab the current path value
        return winreg.QueryValueEx(key, 'PATH')[0]
    def get_nix_user_path() -> str:
        return ''

    return execForOs( linux = get_nix_user_path
                    , mac = get_nix_user_path
                    , win = get_win_user_path
                    )

def appendPath(newPath):
    def appendNixPath():
        return
    def appendWinPath():
        # Get the current user registry
        root = winreg.ConnectRegistry(None, winreg.HKEY_CURRENT_USER)
        # Go to the environment key
        key = winreg.OpenKey(root, 'Environment', 0, winreg.KEY_ALL_ACCESS)
        # Grab the current path value
        path = winreg.QueryValueEx(key, 'PATH')[0]
        if newPath in path:
            return
        # Takes the current path value and appends the new program path
        new_path = path + newPath + ';'
        # Updated the path with the updated path
        winreg.SetValueEx(key, 'PATH', 0, winreg.REG_EXPAND_SZ, new_path)

    execForOs(
        linux = appendNixPath,
        mac = appendNixPath,
        win = appendWinPath
    )

def cd(path: str) -> None:
    os.chdir(path)

def linuxInstallPkg(name: str) -> None:
    apt.install(name)

def macInstallPkg(name: str) -> None:
    brew.install(name)

def winInstallPkg(name: str) -> None:
    return

def installPkg(name: str) -> None:
    execForOs(
        linux = linuxInstallPkg,
        largs = name,
        mac = macInstallPkg,
        margs = name,
        win = winInstallPkg,
        wargs = name
    )


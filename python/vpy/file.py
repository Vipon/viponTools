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

from os import remove
from shutil import move, copyfile
from zipfile import is_zipfile, ZipFile
from tarfile import is_tarfile, TarFile

def extractTar(arch, out='.'):
    with TarFile(arch, 'r') as t:
        t.extractall(path=out)

def extractZip(arch, out=None):
    with ZipFile(arch, 'r') as z:
        z.extractall(path=out)

def extractFile(arch, out='.'):
    if is_zipfile(arch):
        extractZip(arch, out)
    elif is_tarfile(arch):
        extractTar(arch, out)

def mvFile(src, dst):
    move(src, dst)

def rmFile(fn: str) -> None:
    remove(fn)

def cpFile(src: str, dst: str) -> None:
    copyfile(src, dst)


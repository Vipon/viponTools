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

from typing import List
from vpy.string import getStartWhiteSpaces

## Read lines with the same indent
# @brief Return list of all strings from current, which have the same indent
#   from the beginning of the line. File pointer will point to the first string
#   has different indentation.
# @param f File pointer
def readWithSameIndent(f) -> List[str]:
    lines = list()

    firstline = f.readline()
    startWhiteSpaces = getStartWhiteSpaces(firstline)
    pos = f.tell()
    lines.append(firstline)
    for l in f:
        if getStartWhiteSpaces(l) == startWhiteSpaces:
            pos = f.tell()
            lines.append(l)
        else:
            f.seek(pos)
            break

    return lines

## Seek to a specific line
# @brief Move a file pointer from current position to exect after a seeking line
# @param f File pointer
# @param line Line for seeking
def seekLine(f, line: str) -> None:
    while True:
        l = f.readline()
        if (l.rstrip('\n') == line) or (l == ''):
            return


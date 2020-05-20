#!/usr/bin/env python3

import os
import re
import argparse
from enum import Enum
from typing import List


class FileHeaderType(Enum):
    BIN = 0
    TEXT = 1
    REGEX = 2


class FileHeader:
    def __init__(self, t: FileHeaderType, h: str):
        self.header = h
        self.hType = t


    def __compareBinFileHeader(self, fn: str) -> bool:
        fo = open(fn, 'rb')
        header = fo.read(len(self.header))
        return header == self.header


    @staticmethod
    def __getFirstLine(fn: str) -> str:
        fo = open(fn, 'r')
        # !TODO: need to determine max path length
        # MAX_LEN is needed to prevent creation too big string
        MAX_LEN = 1024
        try:
            header = fo.readline(MAX_LEN).strip()
        except UnicodeDecodeError:
            # No unicode file, so
            return ''

        return header


    def __compareTextFileHeader(self, fn: str) -> bool:
        header = FileHeader.__getFirstLine(fn)
        if header == self.header:
            return True
        else:
            return False


    def __compareRegExFileHeader(self, fn: str) -> bool:
        header = FileHeader.__getFirstLine(fn)
        p = re.compile(self.header)
        if p.match(header) is not None:
            return True
        else:
            return False


    def compareFileHeader(self, fn: str) -> bool:
        if self.hType == FileHeaderType.BIN:
            return self.__compareBinFileHeader(fn)
        elif self.hType == FileHeaderType.REGEX:
            return self.__compareRegExFileHeader(fn)
        elif self.hType == FileHeaderType.TEXT:
            return self.__compareTextFileHeader(fn)
        else:
            raise Exception('Unknown FileHeaderType')


class FileType(Enum):
    ELF = 0
    PYTHON = 1
    SHELL = 2
    BASH = 3

    @staticmethod
    def fromStrToFileType(typeName: str):
        typeName = typeName.lower()
        if typeName == 'elf':
            return FileType.ELF
        elif typeName == 'python':
            return FileType.PYTHON
        elif typeName == 'shell':
            return FileType.SHELL
        elif typeName == 'bash':
            return FileType.BASH
        else:
            raise Exception('Unknown FileType', typeName)


def printFileTypes():
    for t in FileType:
        print(t.name.lower())


class FileFilter:
    _table = {
        # FileType : (FileHeaders, extensions)
        FileType.ELF : ((FileHeader(FileHeaderType.BIN, b'\x7f\x45\x4c\x46'),), ('')),
        FileType.PYTHON: ((FileHeader(FileHeaderType.REGEX, '^#!(.*[\s/]+|\s*)python.$'),), ('py', '')),
        FileType.SHELL: ((FileHeader(FileHeaderType.REGEX, '^#!(.*[\s/]+|\s*)bash$'),
                          FileHeader(FileHeaderType.REGEX, '^#!(.*[\s/]+|\s*)dash$'),
                          FileHeader(FileHeaderType.REGEX, '^#!(.*[\s/]+|\s*)sh$'),), ('sh', '')),
        FileType.BASH: ((FileHeader(FileHeaderType.REGEX, '^#!(.*[\s/]+|\s*)bash$'),), ('sh', '')),
    }

    @staticmethod
    def __checkExt(fn: str, t: FileType):
        (headers, extensions) = FileFilter._table[t]
        (name, ext) = os.path.splitext(fn)
        ext = ext.strip('.')
        if ext is '':
            return False

        if ext in extensions:
            return True

        return False


    @staticmethod
    def __checkHeaders(fn: str, t: FileType):
        (headers, extensions) = FileFilter._table[t]
        for h in headers:
            if h.compareFileHeader(fn):
                return True

        return False


    @staticmethod
    def __checkFiles(filesNameList: List[str], t: FileType):
        resList = list()
        for fn in filesNameList:
            if FileFilter.__checkExt(fn, t) or FileFilter.__checkHeaders(fn, t):
                resList.append(fn)

        return resList


    @staticmethod
    def filter(filesNameList: List[str], fileTypes: List[FileType]):
        resList = list()
        for t in fileTypes:
            resList += FileFilter.__checkFiles(filesNameList, t)

        return resList


def parseArgs():
    argParser = argparse.ArgumentParser(description=
        'Filter file by type. Get list of file names and return only files,'
        'which types are specified.'
        )
    argParser.add_argument('--type', '-t', metavar='FILE_TYPE', nargs='+',
        help='type of filtered files')
    argParser.add_argument('--file', '-f', metavar='FILE_PATH', nargs='+',
        help='path to filtered files')
    argParser.add_argument('--print-types-list', action='store_true',
        help='print list of supported file types')

    return argParser.parse_args()


def main():
    args = parseArgs()

    if args.print_types_list:
        printFileTypes()
        exit(0)

    filesNameList = args.file
    types = args.type

    fileTypes = list(map(FileType.fromStrToFileType, types))
    for fn in FileFilter.filter(filesNameList, fileTypes):
        print(fn)


if __name__ == '__main__':
    main()


@echo off

set VERSION=3.9.5
set URL=https://www.python.org/ftp/python/%VERSION%/python-%VERSION%-amd64.exe
set INSTALLER=%cd%\python-%VERSION%-amd64.exe

curl %URL% -o %INSTALLER%
%INSTALLER% /quiet                      ^
            /passive                    ^
            InstallAllUsers=0           ^
            PrependPath=1               ^
            Include_pip=1               ^
            Include_doc=1               ^
            Include_dev=1               ^
            Include_exe=1               ^
            Include_launcher=1          ^
            Include_lib=1               ^
            Include_tcltk=1             ^
            Include_test=1              ^
            Include_tools=1

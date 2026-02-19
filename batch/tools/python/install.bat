@echo off

set NEW_VERSION=3.14.1

for /f "tokens=1" %%i in ('get_version.bat 2^>^&1') do set CUR_VERSION=%%i

call .\..\cmp_version.bat %NEW_VERSION% %CUR_VERSION%
if %ERRORLEVEL% LEQ 0 (
    REM Installed version is more modern
    exit /b 0
)

set URL=https://www.python.org/ftp/python/%NEW_VERSION%/python-%NEW_VERSION%-amd64.exe
set INSTALLER=%cd%\python-%NEW_VERSION%-amd64.exe

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


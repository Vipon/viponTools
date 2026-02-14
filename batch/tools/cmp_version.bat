@echo off
setlocal EnableDelayedExpansion

if "%~2"=="" (
    echo Using: cmp_version.bat version1 version2
    echo Example: cmp_version.bat 1.2.10 1.3.5
    exit /b 2
)

call :CompareVersions "%~1" "%~2"
exit /b %errorlevel%


:CompareVersions
set "v1=%~1"
set "v2=%~2"

for /f "tokens=1-4 delims=." %%a in ("%v1%") do (
    set a1=%%a
    set a2=%%b
    set a3=%%c
    set a4=%%d
)

for /f "tokens=1-4 delims=." %%a in ("%v2%") do (
    set b1=%%a
    set b2=%%b
    set b3=%%c
    set b4=%%d
)

for %%i in (1 2 3 4) do (
    if "!a%%i!"=="" set a%%i=0
    if "!b%%i!"=="" set b%%i=0

    if !a%%i! GTR !b%%i! exit /b 1
    if !a%%i! LSS !b%%i! exit /b -1
)

exit /b 0

@echo off

where python >nul 2>nul
if errorlevel 1 (
    REM There is no Python in Path
    echo 0
    exit /b -1
)

python --version >nul 2>&1
if errorlevel 1 (
    REM Python doesn't work
    echo 0
    exit /b -1
)

for /f "tokens=2" %%i in ('python --version 2^>^&1') do set PYVER=%%i

echo %PYVER%
exit /b 0

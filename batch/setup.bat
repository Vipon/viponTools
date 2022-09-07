@echo off

set ROOT_DIR=%cd%\..
set BATCH_TOOLS_DIR=%cd%\tools
set PYTHON_TOOLS_DIR=%ROOT_DIR%\python\tools

for /d  %%f in (%BATCH_TOOLS_DIR%\*) do (
    cd %%f
    call install.bat
)

echo %PYTHON_TOOLS_DIR%
for /d  %%f in (%PYTHON_TOOLS_DIR%\*) do (
    cd %%f
    python install.py
)


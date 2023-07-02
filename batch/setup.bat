@echo off

set BATCH_TOOLS_DIR=%cd%\tools

for /d %%f in (%BATCH_TOOLS_DIR%\*) do (
    cd %%f
    call install.bat
)


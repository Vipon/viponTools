@echo off

set VIPON_TOOLS_ROOT=%cd%

cd batch
call setup.bat

cd %VIPON_TOOLS_ROOT%
pip3 install -r python\vpy\requirements.txt
python .\python\setupEnv\setupEnv -f python\setupEnv\depsTests.txt


# viponTools

## Description

If you want to contribute or use my tools/libs, you are welcome!

## Clone
```
git clone -c core.symlinks=true https://github.com/Vipon/viponTools
```

## Setup environment
### Windows
Scripts bellow will automatically download and install at least:
```
setup.bat
```

Need to add to PATH:
* {INSTALL_DIR}\VC\Tools\Llvm\x64\bin
* {INSTALL_DIR}\MSBuild\Current\Bin

### Linux
```
./setup.sh
```

### MacOS
```
./setup.sh
```

## Configure, Build and Testing
### GnuMake
Default for Linux: configure could be run without params.
```
./configure --gmake
cd output/
make -j
make test
```

### Ninja
Alternative build system for all operation systems.
```
./configure --ninja
cd output/
ninja
ninja test
```

### Visual studio
Default for Windows: configure could be run without params.
Currently works only with "Visual studio 16 2019"
```
./configure --vs
cd output/
cmake --build .
ctest -C Debug
```

Default build is Debug. If you want to build Release with Visual Studo use the
folowed code:
```
cmake --build . --config Release
```

### XCode
Default for MacOsX: configure could be run without params.
```
./configure --xcode
cd output/
xcodebuild -scheme ALL_BUILD
xcodebuild -scheme RUN_TESTS
```

## Donate

Bitcoin: bc1qdy94rqqye8ez64qy59dl6e5cz3pks4mxku85vk <br>
USDT TRC20: TYR4YCLV5crWB7htBDAZoDve6sRB9AxJJQ <br>

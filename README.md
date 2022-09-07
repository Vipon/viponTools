# viponTools

## Description

If you want to contribute or use my libs, you are welcome!

## Setup environment
Scripts bellow will automatically download and install at least:
* python3
* ninja
* vscode
* cmake
* ccache

### Windows
```
cd batch
setup.bat
```

### Linux
### MacOS

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

### XCode
Default for MacOsX: configure could be run without params.
```
./configure --xcode
cd output/
xcodebuild -scheme ALL_BUILD
xcodebuild -scheme RUN_TESTS
```


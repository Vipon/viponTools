# MIT License
#
# Copyright (c) 2020-2024 Konychev Valerii
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

add_subdirectory(comdef)
add_subdirectory(mem)
add_subdirectory(file)
add_subdirectory(string)
add_subdirectory(LEB128)
add_subdirectory(containers)
add_subdirectory(args)
add_subdirectory(binParse)
add_subdirectory(binPrinter)
add_subdirectory(binDynMod)
if (NOT APPLE)
add_subdirectory(perfAnalysis)
endif (NOT APPLE)
add_subdirectory(test)
add_subdirectory(x86_64)
add_subdirectory(arch)
add_subdirectory(mod)


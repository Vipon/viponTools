# MIT License
#
# Copyright (c) 2026 Konychev Valerii
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

include_guard()

set(COMMON_HEADERS
  os.h
  arch.h
  bits.h
)

set(LINUX_NATIVE_HEADERS
  elf.h
  endian.h
  linux/list.h
  linux/types.h
  linux/container_of.h
  sys/elf_common.h
  sys/elf32.h
  sys/elf64.h
)

# To install WIN32_NATIVE_HEADERS
add_subdirectory(Windows)

set(APPLE_NATIVE_HEADERS
  Kernel/kern/cs_blobs.h
  mach-o/cpu.h
  mach-o/fat.h
  mach-o/fixup-chains.h
  mach-o/loader.h
  mach-o/nlist.h
  mach-o/reloc.h
  mach-o/stab.h
  mach-o/vm_prot.h
  mach-o/arm/reloc.h
  mach-o/arm64/reloc.h
  mach-o/x86_64/reloc.h
)

set(LINUX_HEADERS
  ${APPLE_NATIVE_HEADERS}
)

set(WIN32_HEADERS
  ${APPLE_NATIVE_HEADERS}
  ${LINUX_NATIVE_HEADERS}
)

set(APPLE_HEADERS
  ${LINUX_NATIVE_HEADERS}
)

if (WIN32)
  list(APPEND COMMON_HEADERS ${WIN32_HEADERS})
elseif (APPLE)
  list(APPEND COMMON_HEADERS ${APPLE_HEADERS})
elseif (UNIX AND NOT APPLE)
  list(APPEND COMMON_HEADERS ${LINUX_HEADERS})
endif ()

installHeaders("${COMMON_HEADERS}")

add_subdirectory(linux)

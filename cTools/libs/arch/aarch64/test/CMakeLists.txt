# MIT License
#
# Copyright (c) 2023 Konychev Valerii
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

add_vipon_test(
  NAME aarch64_asm_disasm
  SOURCES aarch64_asm_disasm.c
  LINK_LIBS aarch64_asm_wrap aarch64_disasm_wrap mem
  DEFINES -DKEYSTONE_ASM -DCAPSTONE_DISASM
)

add_vipon_test(
  NAME aarch64_pc_rel
  SOURCES aarch64_pc_rel.c
  LINK_LIBS aarch64_asm_wrap aarch64 string mem
  DEFINES -DKEYSTONE_ASM
)

add_vipon_test(
  NAME aarch64_code_move_test
  SOURCES aarch64_code_move.c
  LINK_LIBS aarch64_code_move aarch64 code_move binParse elf32Parse elf64Parse fatMacho64Parse
            macho64Parse pe64Parse comdef file mem string
)


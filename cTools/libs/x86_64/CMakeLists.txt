set(X86_64_HEADERS
  x86_64.h
  x86_64_DisassemblerWrap.h
)

set(X86_64_SOURCES
  x86_64.c
  x86_64_CapstoneWrap.c
)

add_vipon_library(
  NAME asmWrap
  TYPE SHARED
  HEADERS ${X86_64_HEADERS}
  SOURCES ${X86_64_SOURCES}
  LINK_LIBS capstone keystone mem
  INSTALL ON
)

target_compile_definitions(asmWrap PUBLIC -DCAPSTONE_DISASSEMBLER)


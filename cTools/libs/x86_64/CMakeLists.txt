set(X86_64_HEADERS
  x86_64.h
  x86_64_DisassemblerWrap.h
  )

set(X86_64_SOURCES
  x86_64.c
  x86_64_CapstoneWrap.c
  )

add_library(asmWrap SHARED
  ${X86_64_SOURCES}
  ${X86_64_HEADERS}
  )

target_link_libraries(asmWrap capstone keystone comdef mem bits)
target_compile_definitions(asmWrap PUBLIC -DCAPSTONE_DISASSEMBLER)
target_include_directories(asmWrap INTERFACE ${CMAKE_CURRENT_LIST_DIR})


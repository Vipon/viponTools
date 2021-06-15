add_library(ELF64_PARSE STATIC
  elf64Parse.c
  elf64Parse.h
  )

target_link_libraries(ELF64_PARSE file mem string comdef)

target_include_directories(ELF64_PARSE INTERFACE
  ${CMAKE_CURRENT_LIST_DIR}
  )

add_subdirectory(tests)


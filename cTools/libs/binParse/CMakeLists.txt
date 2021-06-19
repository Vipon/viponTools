add_subdirectory(elfParse)

add_vipon_library(
  NAME BIN_PARSE
  TYPE STATIC
  SOURCES binParse.c binParse.h
  LINK_LIBS elf64Parse elf32Parse
)

add_subdirectory(tests)


add_vipon_library(
  NAME elf64Parse
  TYPE STATIC
  SOURCES elf64Parse.c elf64Parse.h
  LINK_LIBS file mem string
)

add_subdirectory(tests)


add_vipon_library(
  NAME elf64Parse
  TYPE STATIC
  SOURCES elf64Parse.c elf64Parse.h
  LINK_LIBS file mem string
)

add_vipon_library(
  NAME elf32Parse
  TYPE STATIC
  SOURCES elf32Parse.c elf32Parse.h
  LINK_LIBS file mem string
)

if (NOT WIN32)
  add_subdirectory(tests)
endif (NOT WIN32)


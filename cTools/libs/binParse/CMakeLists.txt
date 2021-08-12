add_subdirectory(elfParse)
add_subdirectory(machoParse)
add_subdirectory(peParse)

add_vipon_library(
  NAME binParse
  TYPE STATIC
  SOURCES binParse.c binParse.h
  LINK_LIBS elf64Parse elf32Parse macho64Parse pe64Parse pe64Printer
)

if (NOT WIN32)
  add_subdirectory(tests)
endif ()


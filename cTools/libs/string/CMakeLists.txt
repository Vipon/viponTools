set(STRING_HEADERS
  string.h
)

set(STRING_SOURCES
  string.c
  strtoi64.c
  memcpy.c
)

add_vipon_library(
  NAME string
  TYPE STATIC
  SOURCES ${STRING_SOURCES} ${STRING_HEADERS}
)

add_subdirectory(test)

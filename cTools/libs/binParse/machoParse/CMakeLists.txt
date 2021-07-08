add_vipon_library(
  NAME macho64Parse
  TYPE STATIC
  SOURCES macho64Parse.c macho64Parse.h
  LINK_LIBS file mem string
)


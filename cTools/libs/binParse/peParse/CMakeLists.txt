add_vipon_library(
  NAME pe64Parse
  TYPE STATIC
  SOURCES pe64Parse.c pe64Parse.h
  LINK_LIBS file mem string
)

add_vipon_library(
  NAME pe64Printer
  TYPE STATIC
  SOURCES pe64Printer.c pe64Printer.h
  LINK_LIBS pe64Parse
)

add_subdirectory(test)


add_vipon_library(
  NAME pe64Parse
  TYPE STATIC
  SOURCES pe64Parse.c pe64Parse.h
  LINK_LIBS file mem string
)

set(PE64_PRINTER_SOURSES
  pe64PrinterHeaders.c
  pe64PrinterSections.c
  pe64PrinterSymbols.c
  pe64PrinterImports.c
  pe64PrinterDelayImports.c
)

set(PE64_PRINTER_HEADERS
  pe64Printer.h
)

add_vipon_library(
  NAME pe64Printer
  TYPE STATIC
  SOURCES ${PE64_PRINTER_SOURSES} ${PE64_PRINTER_HEADERS}
  LINK_LIBS pe64Parse
)

add_subdirectory(test)


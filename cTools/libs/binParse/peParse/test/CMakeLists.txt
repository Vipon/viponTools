add_vipon_test(
  NAME pe64PrinterTest
  SOURCES pe64PrinterTest.c
  LINK_LIBS pe64Printer
)

add_custom_target(testFile
  COMMAND ${CMAKE_C_COMPILER} -c -g3 -ggdb -Wno-unknown-argument "${CMAKE_CURRENT_LIST_DIR}/testFile.c"
  DEPENDS testFile.c
)

if (MSVC)
  set(TEST_FILE "${CMAKE_CURRENT_BINARY_DIR}/testFile.obj")
else ()
  set(TEST_FILE "${CMAKE_CURRENT_BINARY_DIR}/testFile.o")
endif ()

add_vipon_test(
  NAME pe64Symbol
  SOURCES pe64Symbol.c
  LINK_LIBS pe64Printer
  CMD_LINE "${TEST_FILE}"
  DEPENDS testFile
)


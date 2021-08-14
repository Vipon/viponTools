add_vipon_test(
  NAME pe64PrintExeTest
  SOURCES pe64PrintExeTest.c
  LINK_LIBS pe64Printer
)

add_custom_target(testObjFile
  COMMAND ${CMAKE_C_COMPILER} -c -g3 -ggdb -Wno-unknown-argument "${CMAKE_CURRENT_LIST_DIR}/testObjFile.c"
  DEPENDS testObjFile.c
)

if (MSVC)
  set(TEST_FILE "${CMAKE_CURRENT_BINARY_DIR}/testObjFile.obj")
else ()
  set(TEST_FILE "${CMAKE_CURRENT_BINARY_DIR}/testObjFile.o")
endif ()

add_vipon_test(
  NAME pe64PrintObjTest
  SOURCES pe64PrintObjTest.c
  LINK_LIBS pe64Printer
  CMD_LINE "${TEST_FILE}"
  DEPENDS testObjFile
)


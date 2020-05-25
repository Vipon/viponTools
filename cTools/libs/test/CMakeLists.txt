set(TEST_HEADERS
  test.h
  )

set(TEST_SRCS
  test.c
  )

add_library(vTest
  ${TEST_SRCS}
  ${TEST_HEADERS}
  )

target_include_directories(vTest
  INTERFACE
  ${CMAKE_CURRENT_LIST_DIR}
  )


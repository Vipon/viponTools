set(TEST_HEADERS
  test.h
  )

set(TEST_SRCS
  test.c
  )

add_vipon_library(
  NAME vTest
  TYPE STATIC
  SOURCES ${TEST_SRCS} ${TEST_HEADERS}
)


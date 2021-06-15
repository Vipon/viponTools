set(CONTAINERS_HEADERS
  containers.h
)

set(CONTAINERS_SRCS
  vector.c
)

add_vipon_library(
  NAME containers
  TYPE STATIC
  SOURCES ${CONTAINERS_SRCS} ${CONTAINERS_HEADERS}
  LINK_LIBS bits mem
)

add_subdirectory(tests)


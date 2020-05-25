set(CONTAINERS_HEADERS
  containers.h
  )

set(CONTAINERS_SRCS
  vector.c
  )

add_library(containers STATIC
  ${CONTAINERS_SRCS}
  ${CONTAINERS_HEADERS}
  )

target_link_libraries(containers comdef bits mem)

target_include_directories(containers INTERFACE
  ${CMAKE_CURRENT_LIST_DIR}
  )

add_subdirectory(tests)


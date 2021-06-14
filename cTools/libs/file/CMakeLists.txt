add_library(file STATIC
  file.c
  file.h
  )

target_link_libraries(file mem comdef)

target_include_directories(file INTERFACE
  ${CMAKE_CURRENT_LIST_DIR}
  )


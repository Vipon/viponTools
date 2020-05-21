add_library(file STATIC
  file.c
  file.h
  )

target_link_libraries(file comdef mem)

target_include_directories(file INTERFACE
  ${CMAKE_CURRENT_LIST_DIR}
  )


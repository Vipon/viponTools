add_library(LEB128 STATIC
  LEB128.c
  LEB128.h
  )

target_link_libraries(LEB128 comdef)

target_include_directories(LEB128 INTERFACE
  ${CMAKE_CURRENT_LIST_DIR}
  )


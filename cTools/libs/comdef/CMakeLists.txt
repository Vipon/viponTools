add_library(comdef STATIC
  comdef.c
  comdef.h
  )

target_include_directories(comdef INTERFACE
  ${CMAKE_CURRENT_LIST_DIR}
  )


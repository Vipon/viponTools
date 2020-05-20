add_library(bits
  bits.c
  bits.h
  )

target_include_directories(bits INTERFACE ${CMAKE_CURRENT_LIST_DIR})


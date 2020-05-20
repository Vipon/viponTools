add_library(mem STATIC
  mem.c
  mem.h
  )

target_link_libraries(mem comdef)
target_include_directories(mem INTERFACE ${CMAKE_CURRENT_LIST_DIR})


add_library(string STATIC
  string.c
  string.h
  )

target_link_libraries(string comdef)
target_include_directories(string INTERFACE ${CMAKE_CURRENT_LIST_DIR})


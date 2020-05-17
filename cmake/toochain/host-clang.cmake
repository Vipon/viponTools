include_guard(GLOBAL)

find_program(CMAKE_C_COMPILER clang)
  if(NOT CMAKE_C_COMPILER)
    message(FATAL_ERROR "clang not found")
  endif()

find_program(CMAKE_CXX_COMPILER clang++)
  if(NOT CMAKE_CXX_COMPILER)
    message(FATAL_ERROR "clang++ not found")
  endif()

include(${CMAKE_CURRENT_LIST_DIR}/ccache.cmake)


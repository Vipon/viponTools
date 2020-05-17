include_guard(GLOBAL)

find_program(CMAKE_C_COMPILER gcc)
  if(NOT CMAKE_C_COMPILER)
    message(FATAL_ERROR "gcc not found")
  endif()

find_program(CMAKE_CXX_COMPILER g++)
  if(NOT CMAKE_CXX_COMPILER)
    message(FATAL_ERROR "g++ not found")
  endif()

include(${CMAKE_CURRENT_LIST_DIR}/ccache.cmake)


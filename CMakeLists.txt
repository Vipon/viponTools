cmake_minimum_required(VERSION 3.15 FATAL_ERROR)
project(viponTools
  LANGUAGES C CXX
  VERSION 1.0
  )

include(${CMAKE_CURRENT_LIST_DIR}/cmake/toolchain/cflags/cflags.cmake)
include(CTest)

add_subdirectory(cTools)


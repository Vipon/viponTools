cmake_minimum_required(VERSION 3.15 FATAL_ERROR)
project(cTools
  LANGUAGES C CXX
  VERSION 1.0
)

include_directories(AFTER include)
add_subdirectory(libs)
add_subdirectory(tests)


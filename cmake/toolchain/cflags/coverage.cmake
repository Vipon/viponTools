include(${CMAKE_CURRENT_LIST_DIR}/cflags.cmake)

set(COVERAGE_CFLAGS)

# Need to specify libs for link, because cmake checking with linking.
set(CMAKE_REQUIRED_LIBRARIES --coverage)
if("${CMAKE_C_COMPILER_ID}" STREQUAL "GNU")
  append_cflags(COVERAGE_CFLAGS --coverage -fprofile-abs-path)
elseif("${CMAKE_C_COMPILER_ID}" STREQUAL "Clang")
  append_cflags(COVERAGE_CFLAGS "-fprofile-instr-generate -fcoverage-mapping")
endif()

append_cflags(COVERAGE_CFLAGS -fno-omit-frame-pointer)
unset(CMAKE_REQUIRED_LIBRARIES)

string(APPEND C_FLAGS_DEBUG   " ${COVERAGE_CFLAGS}")
string(APPEND CXX_FLAGS_DEBUG " ${COVERAGE_CFLAGS}")


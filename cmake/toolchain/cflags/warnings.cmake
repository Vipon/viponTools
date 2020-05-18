include(${CMAKE_CURRENT_LIST_DIR}/cflags.cmake)

set(WARNING_FLAGS)
append_cflags(WARNING_FLAGS -Wall -Wextra -Wcast-qual -Wnewline-eof -Wparentheses -Wsign-conversion -fdiagnostics-color=always)

if("${CMAKE_C_COMPILER_ID}" STREQUAL "GNU")
  # clang doesn't support these flags (this causes too many warnings)
  append_cflags(WARNING_FLAGS -Wduplicated-branches -Wduplicated-cond)
endif()

set(WARNING_CXXFLAGS "${WARNING_FLAGS}")

# only C warning flags
set(WARNING_CFLAGS)
append_cflags(WARNING_CFLAGS -Wmissing-prototypes -Wstrict-prototypes)

# only CXX warning flags
set(WARNING_CXXFLAGS)

string(APPEND C_FLAGS   " ${WARNING_FLAGS}")
string(APPEND C_FLAGS   " ${WARNING_CFLAGS}")

string(APPEND CXX_FLAGS " ${WARNING_FLAGS}")
string(APPEND CXX_FLAGS " ${WARNING_CXXFLAGS}")


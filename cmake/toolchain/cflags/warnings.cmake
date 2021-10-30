include(${CMAKE_CURRENT_LIST_DIR}/cflags.cmake)

set(WARNING_FLAGS)
append_cflags(WARNING_FLAGS -Werror -Wall -Wextra -Wcast-qual -Wnewline-eof -Wparentheses -Wsign-conversion -fdiagnostics-color=always)

if("${CMAKE_C_COMPILER_ID}" STREQUAL "GNU")
  # clang doesn't support these flags (this causes too many warnings)
  append_cflags(WARNING_FLAGS -Wduplicated-branches -Wduplicated-cond)
endif()

if (MSVC)
  append_cflags(WARNING_FLAGS
    -Wno-unknown-argument
    -Wno-reserved-id-macro
    -Wno-extra-semi-stmt
    -Wno-documentation-unknown-command
    -Wno-microsoft-fixed-enum
    -Wno-unused-command-line-argument
    -Wno-c++98-compat
    -Wno-covered-switch-default
    -Wno-gnu
    -Wno-empty-translation-unit
    -Wno-bad-function-cast
  )
endif (MSVC)

if (WIN32)
  append_cflags(WARNING_FLAGS
    -Wno-deprecated-declarations
  )
endif (WIN32)

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


include_guard(GLOBAL)
include(CheckCCompilerFlag)

macro(append_cflags var)
  foreach(flag ${ARGN})
    set(check "Flag: ${flag}")
    check_c_compiler_flag("${flag} -w" ${check})
    if(${check})
      string(APPEND ${var} " ${flag}")
    endif()
  endforeach()
endmacro()

#enable_language(C)
include(${CMAKE_CURRENT_LIST_DIR}/common.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/debug.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/release.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/coverage.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/sanitizers.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/warnings.cmake)

set(CMAKE_C_FLAGS ${C_FLAGS})
set(CMAKE_C_FLAGS_DEBUG ${C_FLAGS_DEBUG})
set(CMAKE_C_FLAGS_RELEASE ${C_FLAGS_RELEASE})

set(CMAKE_CXX_FLAGS ${CXX_FLAGS})
set(CMAKE_CXX_FLAGS_DEBUG ${CXX_FLAGS_DEBUG})
set(CMAKE_CXX_FLAGS_RELEASE ${CXX_FLAGS_RELEASE})


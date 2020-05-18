include(${CMAKE_CURRENT_LIST_DIR}/cflags.cmake)

set(SANITIZERS_FLAGS)

# Need to specify libs for link, because cmake checking with linking.
if("${CMAKE_C_COMPILER_ID}" STREQUAL "Clang")
  set(CMAKE_REQUIRED_LIBRARIES "${CLANG_RT_ASAN} ${CLANG_RT_UBSAN} -lpthread -lrt -lm -ldl -L${CLANG_RT_LIBS_PATH}")
else()
  set(CMAKE_REQUIRED_LIBRARIES "-lasan -lubsan")
endif()

append_cflags(SANITIZERS_FLAGS -fsanitize=address -fsanitize=leak -fsanitize=undefined)

unset(CMAKE_REQUIRED_LIBRARIES)

string(APPEND C_FLAGS_DEBUG   "${SANITIZERS_FLAGS}")
string(APPEND CXX_FLAGS_DEBUG "${SANITIZERS_FLAGS}")


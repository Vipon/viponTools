include(${CMAKE_CURRENT_LIST_DIR}/cflags.cmake)

set(SANITIZERS_FLAGS)

# Need to specify libs for link, because cmake checking with linking.
set(CMAKE_REQUIRED_LIBRARIES "-lasan -lubsan")

append_cflags(SANITIZERS_FLAGS -fsanitize=address -fsanitize=leak -fsanitize=undefined)

unset(CMAKE_REQUIRED_LIBRARIES)

string(APPEND C_FLAGS_DEBUG   " ${SANITIZERS_FLAGS}")
string(APPEND CXX_FLAGS_DEBUG " ${SANITIZERS_FLAGS}")


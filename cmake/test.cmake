# MIT License
#
# Copyright (c) 2021-2024 Konychev Valerii
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

include_guard()

###############################################################################
# Parameters:
#   NAME - test name
#   SOURCES - list of source files
#   LINK_LIBRARIES - list of link libraries
#   DEFINES - extra defines for compiler
#   CMD_LINE - args for test
#   DEPENDS - targets should be built before test building
#   TEST_FILES - files for test case
###############################################################################
function(add_vipon_test)
  cmake_parse_arguments(ARG
    # true_false_options
    ""
    # one_value_options
    "NAME"
    # multi_value_options
    "SOURCES;LINK_LIBS;DEFINES;CMD_LINE;DEPENDS;LINK_FLAGS;TEST_FILES;COMP_OPT"
    ${ARGN}
  )

  add_executable(${ARG_NAME}
    ${ARG_SOURCES}
  )

  if (ARG_DEPENDS)
    add_dependencies(${ARG_NAME} ${ARG_DEPENDS})
  endif()

  target_link_options(${ARG_NAME} BEFORE PUBLIC ${ARG_LINK_FLAGS})
  target_link_libraries(${ARG_NAME} vTest ${ARG_LINK_LIBS})
  target_compile_definitions(${ARG_NAME} PRIVATE ${ARG_DEFINES})
  target_compile_definitions(${ARG_NAME} PRIVATE EXECUTABLE)
  target_compile_options(${ARG_NAME} PRIVATE ${ARG_COMP_OPT})

  add_test(
    NAME ${ARG_NAME}
    COMMAND ./${ARG_NAME} ${ARG_CMD_LINE}
  )

  if (WIN32)
    if (MINGW)
      set(TEST_PATH_${ARG_NAME} "PATH=\$\{PATH\}:${VIPON_TOOLS_EXTERNAL_LIB_DIR}")
    else (MINGW)
      set(TEST_PATH_${ARG_NAME} "PATH=%PATH%\;${VIPON_TOOLS_EXTERNAL_LIB_DIR}")
    endif (MINGW)

    foreach(elem ${ARG_LINK_LIBS})
      if (TARGET ${elem})
        get_target_property(PATH_TO_LIB ${elem} BINARY_DIR)
        if (MSVC)
          set(PATH_TO_LIB "${PATH_TO_LIB}\\${CMAKE_BUILD_TYPE}")
        endif (MSVC)
        if (MINGW)
          string(APPEND TEST_PATH_${ARG_NAME} ":${PATH_TO_LIB}")
        else (MINGW)
          string(APPEND TEST_PATH_${ARG_NAME} "\;${PATH_TO_LIB}")
        endif (MINGW)
      endif (TARGET ${elem})
    endforeach(elem)

    set_tests_properties(${ARG_NAME}
      PROPERTIES ENVIRONMENT "${TEST_PATH_${ARG_NAME}}"
    )
  endif (WIN32)

  file(
    COPY ${ARG_TEST_FILES}
    DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
  )

endfunction(add_vipon_test)


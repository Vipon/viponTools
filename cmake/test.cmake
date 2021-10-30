# MIT License
#
# Copyright (c) 2021 Konychev Valerii
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
#   CMD_LINE - args for test
#   DEPENDS - targets should be built before test building
###############################################################################
function(add_vipon_test)
  cmake_parse_arguments(ARG
    # true_false_options
    ""
    # one_value_options
    "NAME"
    # multi_value_options
    "SOURCES;LINK_LIBS;CMD_LINE;DEPENDS;LINK_FLAGS"
    ${ARGN}
  )

  add_executable(${ARG_NAME}
    ${ARG_SOURCES}
  )

  if (NOT ${ARG_DEPENDS} EQUAL "")
    add_dependencies(${ARG_NAME} ${ARG_DEPENDS})
  endif()

  target_link_options(${ARG_NAME} BEFORE
    PUBLIC ${ARG_LINK_FLAGS}
  )

  target_link_libraries(${ARG_NAME}
    vTest
    ${ARG_LINK_LIBS}
  )

  add_test(
    NAME ${ARG_NAME}
    COMMAND ./${ARG_NAME} ${ARG_CMD_LINE}
  )
endfunction(add_vipon_test)


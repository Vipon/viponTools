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


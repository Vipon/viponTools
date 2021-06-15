include_guard()

###############################################################################
# Parameters:
#   NAME - test name
#   SOURCES - list of source files
#   LINK_LIBRARIES - list of link libraries
###############################################################################
function(add_vipon_test)
  cmake_parse_arguments(ARG
    # true_false_options
    ""
    # one_value_options
    "NAME"
    # multi_value_options
    "SOURCES;LINK_LIBS"
    ${ARGN}
  )

  add_executable(${ARG_NAME}
    ${ARG_SOURCES}
  )

  target_link_libraries(${ARG_NAME}
    ${ARG_LINK_LIBS}
    comdef
    vTest
  )

  add_test(
    NAME ${ARG_NAME}
    COMMAND ./${ARG_NAME}
  )
endfunction(add_vipon_test)


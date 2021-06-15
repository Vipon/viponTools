include_guard()

###############################################################################
# Parameters:
#   NAME - test name
#   TYPE - library type
#   SOURCES - list of source files
#   LINK_LIBRARIES - list of link libraries
###############################################################################
function(add_vipon_library)
  cmake_parse_arguments(ARG
    # true_false_options
    ""
    # one_value_options
    "NAME;TYPE"
    # multi_value_options
    "SOURCES;LINK_LIBS"
    ${ARGN}
  )

  add_library(${ARG_NAME} ${ARG_TYPE}
    ${ARG_SOURCES}
  )

  target_link_libraries(${ARG_NAME}
    ${ARG_LINK_LIBS}
    comdef
  )

  target_include_directories(${ARG_NAME} INTERFACE
    ${CMAKE_CURRENT_LIST_DIR}
  )
endfunction(add_vipon_library)


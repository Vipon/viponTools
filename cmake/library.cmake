# MIT License
#
# Copyright (c) 2021-2023 Konychev Valerii
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
#   TYPE - library type
#   HEADERS - list of library headers
#   SOURCES - list of source files
#   LINK_LIBRARIES - list of link libraries
#   LINK_OPTIONS - extra link options
#   DEFINES - extra defines for compiler
#   INSTALL - if ON when install
###############################################################################
function(add_vipon_library)
  cmake_parse_arguments(ARG
    # true_false_options
    ""
    # one_value_options
    "NAME;TYPE;INSTALL"
    # multi_value_options
    "HEADERS;SOURCES;LINK_LIBS;LINK_OPTIONS;DEFINES"
    ${ARGN}
  )

  add_library(${ARG_NAME} ${ARG_TYPE}
    ${ARG_SOURCES}
    ${ARG_HEADERS}
  )

  target_link_libraries(${ARG_NAME}
    ${ARG_LINK_LIBS}
    comdef
  )

  target_link_options(${ARG_NAME} PUBLIC ${ARG_LINK_OPTIONS})
  target_compile_definitions(${ARG_NAME} PRIVATE ${ARG_DEFINES})
  if (WIN32)
    if (ARG_TYPE STREQUAL "SHARED")
      target_compile_definitions(${ARG_NAME} PRIVATE SHARED_LIB)
    else ()
      target_compile_definitions(${ARG_NAME} PRIVATE STATIC_LIB)
    endif ()
  endif (WIN32)
  target_include_directories(${ARG_NAME} INTERFACE ${CMAKE_CURRENT_LIST_DIR})

  if(ARG_INSTALL)
    if (WIN32)
      if (ARG_TYPE STREQUAL "SHARED")
        install(
          TARGETS ${ARG_NAME}
          DESTINATION ${INSTALL_BIN_DIR}
        )
      else ()
        install(
          TARGETS ${ARG_NAME}
          DESTINATION ${INSTALL_LIB_DIR}
        )
      endif ()
    else (WIN32)
      install(
        TARGETS ${ARG_NAME}
        DESTINATION ${INSTALL_LIB_DIR}
      )
    endif (WIN32)

    installHeaders("${ARG_HEADERS}")
  endif(ARG_INSTALL)

endfunction(add_vipon_library)

###############################################################################
# Parameters:
#   NAME - test name
#   TYPE - library type
#   HEADERS - list of library headers
#   LIB_DIR - directory contains imported lib
#   INCLUDE_DIR - derectory contains lib headers
#   INSTALL - if ON when install
###############################################################################
function(add_vipon_import_library)
  cmake_parse_arguments(ARG
    # true_false_options
    ""
    # one_value_options
    "NAME;TYPE;INSTALL;LIB_DIR;INCLUDE_DIR"
    # multi_value_options
    "HEADERS"
    ${ARGN}
  )

  add_library(${ARG_NAME} ${ARG_TYPE} IMPORTED)
  if (WIN32)
    if (ARG_TYPE STREQUAL "SHARED")
      set(NAME_${ARG_NAME} ${ARG_NAME}.dll)
      set_target_properties(${ARG_NAME}
        PROPERTIES
          IMPORTED_LOCATION "${ARG_LIB_DIR}/${NAME_${ARG_NAME}}"
          IMPORTED_IMPLIB "${ARG_LIB_DIR}/${ARG_NAME}.lib"
      )
    else ()
      set(NAME_${ARG_NAME} ${ARG_NAME}.lib)
      set_target_properties(${ARG_NAME}
        PROPERTIES
          IMPORTED_LOCATION "${ARG_LIB_DIR}/${NAME_${ARG_NAME}}"
          IMPORTED_IMPLIB "${ARG_LIB_DIR}/${ARG_NAME}.lib"
      )
    endif ()
  else (WIN32)
    if (ARG_TYPE STREQUAL "SHARED")
      set(NAME_${ARG_NAME} ${ARG_NAME}.so)
      set_target_properties(${ARG_NAME}
        PROPERTIES
          IMPORTED_LOCATION "${ARG_LIB_DIR}/${NAME_${ARG_NAME}}"
      )
    else ()
      set(NAME_${ARG_NAME} ${ARG_NAME}.a)
      set_target_properties(${ARG_NAME}
        PROPERTIES
          IMPORTED_LOCATION "${ARG_LIB_DIR}/${NAME_${ARG_NAME}}"
      )
    endif ()
  endif (WIN32)

  target_include_directories(${ARG_NAME} INTERFACE ${ARG_INCLUDE_DIR})

  if(ARG_INSTALL)
    if (WIN32)
      if (ARG_TYPE STREQUAL "SHARED")
        install(
          FILES "${ARG_LIB_DIR}/${NAME_${ARG_NAME}}"
                "${ARG_LIB_DIR}/${ARG_NAME}.lib"
          DESTINATION ${INSTALL_BIN_DIR}
        )
      else ()
        install(
          FILES "${ARG_LIB_DIR}/${NAME_${ARG_NAME}}"
          DESTINATION ${INSTALL_LIB_DIR}
        )
      endif ()
    else (WIN32)
      install(
        FILES "${ARG_LIB_DIR}/${NAME_${ARG_NAME}}"
        DESTINATION ${INSTALL_LIB_DIR}
      )
    endif (WIN32)

    addPrefix("${ARG_INCLUDE_DIR}/" ARG_HEADERS "${ARG_HEADERS}")
    installHeaders("${ARG_HEADERS}")
  endif(ARG_INSTALL)

endfunction(add_vipon_import_library)


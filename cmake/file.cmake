include_guard()

function(createDir DIR_PATH)
  if(NOT EXISTS "${DIR_PATH}")
    file(MAKE_DIRECTORY "${DIR_PATH}")
  endif()
endfunction(createDir)

function(downloadFile)
  cmake_parse_arguments(ARG
    # true_false_options
    "CHECK_STAMP"
    # one_value_options
    "FILE_URL;FILE_NAME;FILE_HASH"
    # multi_value_options
    ""
    ${ARGN}
  )

  if(NOT ARG_FILE_URL)
    message(FATAL_ERROR "Please specify FILE_URL")
  endif()

  if(NOT ARG_FILE_NAME)
    message(FATAL_ERROR "Please specify FILE_NAME")
  endif()

  get_filename_component(FILE_DIR "${ARG_FILE_NAME}" DIRECTORY ABSOLUTE)
  createDir("${FILE_DIR}")

  set(CHECK_HASH)
  if(ARG_FILE_HASH)
    set(CHECK_HASH EXPECTED_HASH ${ARG_FILE_HASH})
  endif()

  get_filename_component(JUST_NAME "${ARG_FILE_NAME}" NAME)
  set(FILE_STAMP "${FILE_DIR}/.stam_${JUST_NAME}")
  if(ARG_CHECK_STAMP)
    if (EXISTS "${FILE_STAMP}")
      return()
    endif()
  endif()

  message(STATUS "Download ${ARG_FILE_NAME} from ${ARG_FILE_URL}")

  file(
    DOWNLOAD "${ARG_FILE_URL}" "${ARG_FILE_NAME}"
    ${CHECK_HASH}
    SHOW_PROGRESS
  )

  if(ARG_CHECK_STAMP)
    file(TOUCH "${FILE_STAMP}")
  endif()
endfunction(downloadFile)


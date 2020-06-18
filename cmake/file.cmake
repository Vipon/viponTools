include_guard()


function(createDir DIR_PATH)
  if(NOT EXISTS "${DIR_PATH}")
    message(STATUS "Create directory ${DIR_PATH}")
    file(MAKE_DIRECTORY "${DIR_PATH}")
  endif()
endfunction(createDir)


function(removeDir DIR_PATH)
  message(STATUS "Remove directory ${DIR_PATH}")
  file(REMOVE_RECURSE "${DIR_PATH}")
endfunction(removeDir)


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
  set(FILE_STAMP "${FILE_DIR}/.download_stamp_${JUST_NAME}")
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


function(extractArchive)
  cmake_parse_arguments(ARG
    # true_false_options
    "CHECK_STAMP"
    # one_value_options
    "ARH_NAME;OUT_DIR;MEMBER;STRIP_COMPONENT"
    # multi_value_options
    ""
    ${ARGN}
  )

  set(TAR_ARGS xvf)

  if(NOT ARG_ARH_NAME)
    message(FATAL_ERROR "Please specify ARH_NAME")
  endif()
  list(APPEND TAR_ARGS "${ARG_ARH_NAME}" "${ARG_MEMBER}")

  get_filename_component(ARH_DIR "${ARG_ARH_NAME}" DIRECTORY ABSOLUTE)

  get_filename_component(JUST_ARH_NAME "${ARG_ARH_NAME}" NAME)
  set(ARH_STAMP "${ARH_DIR}/.arh_stamp_${JUST_ARH_NAME}")
  if(ARG_CHECK_STAMP)
    if (EXISTS "${ARH_STAMP}")
      return()
    endif()
  endif()

  if(ARG_STRIP_COMPONENT)
    list(APPEND TAR_ARGS "--strip-components=${ARG_STRIP_COMPONENT}")
  endif()

  if(ARG_OUT_DIR)
    list(APPEND TAR_ARGS "--one-top-level=${ARG_OUT_DIR}")
  endif()

  message(STATUS "Extract ${ARG_ARH_NAME} to ${ARG_OUT_DIR}")

  execute_process(
    COMMAND tar ${TAR_ARGS}
    WORKING_DIRECTORY "${ARH_DIR}"
    RESULT_VARIABLE ret
  )

  if(NOT ${ret} EQUAL "0")
    message(FATAL_ERROR "FAILED to extract archive ${ARG_ARH_NAME}")
  endif()

  if(ARG_CHECK_STAMP)
    file(TOUCH "${ARH_STAMP}")
  endif()
endfunction(extractArchive)


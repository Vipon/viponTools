set(PERF_ANALYSIS_SRC
  perf.c
  )

set(PERF_ANALYSIS_HEADERS
  perf.h
  )

add_library(perfAnalysis
  ${PERF_ANALYSIS_SRC}
  ${PERF_ANALYSIS_HEADERS}
  )

target_link_libraries(perfAnalysis
  comdef
  )

target_include_directories(perfAnalysis INTERFACE
  ${CMAKE_CURRENT_LIST_DIR}
  )

add_subdirectory(tests)


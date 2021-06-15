set(PERF_ANALYSIS_SRC
  perf.c
)

set(PERF_ANALYSIS_HEADERS
  perf.h
)

add_vipon_library(
  NAME perfAnalysis
  TYPE STATIC
  SOURCES ${PERF_ANALYSIS_SRC} ${PERF_ANALYSIS_HEADERS}
)

add_subdirectory(tests)


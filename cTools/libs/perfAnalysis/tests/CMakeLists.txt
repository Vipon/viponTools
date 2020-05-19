add_executable(perfTimerTest
  perfTimerTest.c
  )

target_link_libraries(perfTimerTest perfAnalysis)

add_test(
  NAME PerfTimerTest
  COMMAND ./perfTimerTest
  )


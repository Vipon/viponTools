add_executable(vectorTest
  vectorTest.c
  )

target_link_libraries(vectorTest containers comdef)

add_test(
  NAME vector-test
  COMMAND ./vectorTest
  )


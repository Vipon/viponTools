add_library(elfFoo SHARED
  foo.c
)

add_library(elfBar SHARED
  bar.c
)

add_vipon_test(
  NAME elfParseTest
  SOURCES elfParseTest.c
  LINK_LIBS elf64Parse elfFoo elfBar
)


add_library(FOO SHARED
  foo.c
)

add_library(BAR SHARED
  bar.c
)

add_vipon_test(
  NAME elfParseTest
  SOURCES elfParseTest.c
  LINK_LIBS ELF64_PARSE FOO BAR
)


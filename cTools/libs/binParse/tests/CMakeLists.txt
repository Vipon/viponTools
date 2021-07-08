add_library(BINFOO SHARED
  foo.c
)

add_library(BINBAR SHARED
  bar.c
)

add_vipon_test(
  NAME binParseTest
  SOURCES binParseTest.c
  LINK_LIBS binParse BINFOO BINBAR
)


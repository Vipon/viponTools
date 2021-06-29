include_guard(GLOBAL)

find_program(CCACHE ccache)
  if(NOT CCACHE)
    message(SEND_ERROR "ccache not found")
  else()
    set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE "ccache")
    set_property(GLOBAL PROPERTY RULE_LAUNCH_LINK "ccache")
  endif()


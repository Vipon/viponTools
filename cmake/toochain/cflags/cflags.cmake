include_guard(GLOBAL)

macro(append_cflags var)
  foreach(flag ${ARGN})
    set(check check-${flag})
    check_c_compiler_flag("${flag} -w" ${check})
    if(${check})
      string(APPEND ${var} " ${flag}")
    endif()
  endforeach()
endmacro()


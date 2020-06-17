include_guard()

function(addPrefix prefix list)
  set(newList "")
  foreach(elem ${ARGN})
    list(APPEND newList "${prefix}${elem}")
  endforeach(elem)
  set(${list} "${newList}" PARENT_SCOPE)
endfunction(addPrefix)


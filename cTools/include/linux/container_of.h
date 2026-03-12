#ifndef _LINUX_CONTAINER_OF_H
#define _LINUX_CONTAINER_OF_H

#define typeof_member(T, m)	typeof(((T*)0)->m)

/**
 * @brief cast a member of a structure out to the containing structure
 * @param[in] ptr    the pointer to the member
 * @param[in] type   the type of the container struct this is embedded in
 * @param[in] member the name of the member within the struct
 */
#define container_of(ptr, type, member) ({                \
    const typeof_member(type, member) *__mptr = (ptr);    \
    (type *)( (char *)__mptr - offsetof(type,member) );})

#endif /* _LINUX_CONTAINER_OF_H */

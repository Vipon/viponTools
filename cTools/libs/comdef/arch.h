#ifndef __ARCH_H
#define __ARCH_H

#ifdef __GNUC__
# if defined(i386) || defined(__i386) || defined(__i386__)
#  define IA32_DEFINED 1
# endif

# if defined(__amd64) || defined(__amd64__) || defined(__x86_64) || defined(__x86_64__)
#  define IA32_DEFINED 1
#  define IA32E_DEFINED 1
#  define AMD64_DEFINED 1
#  define X86_64_DEFINED 1
# endif

# if defined(__aarch64__)
#  define ARM64_DEFINED 1
#  define ARMV8_DEFINED 1
#  define AARCH64_DEFINED 1
# endif

# if defined(__arm__)
#  define ARM32_DEFINED 1
# endif
#else
# error Unknown compiler type
#endif

#endif /* __ARCH_H */


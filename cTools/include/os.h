/***
 * MIT License
 *
 * Copyright (c) 2021-2023 Konychev Valera
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#ifndef __OS_H
#define __OS_H

#if defined(unix) || defined(__unix) || defined(__unix__)
# ifndef __UNIX__
#  define __UNIX__
# endif
#endif

#if defined(__linux__)
# ifndef __LINUX__
#  define __LINUX__
# endif
# define SYM_PREFIX
#endif

#if defined(__APPLE__) && defined(__MACH__)
# ifndef __MAC_OS_X__
#  define __MAC_OS_X__
# endif
# define SYM_PREFIX "_"
#endif

#if defined(_WIN32) || defined(_WIN64)
# ifndef __WIN__
#  define __WIN__
# endif
# define SYM_PREFIX
#endif

#ifdef __WIN__
# define DLLEXPORT __declspec(dllexport)
# define DLLIMPORT __declspec(dllimport)
# define EXPORT_FUNC DLLEXPORT
# define EXPORT_VAR DLLEXPORT
# define IMPORT_VAR DLLIMPORT
#else /* __WIN__ */
# define EXPORT_FUNC
# define EXPORT_VAR
# define IMPORT_VAR
# define DLLEXPORT
# define DLLIMPORT
#endif /* __WIN__ */

#endif /* __OS_H */


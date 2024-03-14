/***
 * MIT License
 *
 * Copyright (c) 2020-2024 Konychev Valera
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

#ifndef _FILE_H
#define _FILE_H

#include "os.h"

// C standard headers
#include <stdio.h>
#include <stddef.h>
#include <stdint.h>

// OS headers
# include <sys/types.h>
#if defined(__UNIX__) || defined(__LINUX__) || defined(__MAC_OS_X__)
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>

typedef int FileD;
typedef int FileFlag;
typedef int mprot_t;

#define INV_FD -1
#define IS_VLD_FD(fd) (fd >= 0)
#define IS_INV_FD(fd) (fd < 0)

#elif defined(__WIN__)
# include <Windows.h>

# define O_RDONLY       GENERIC_READ
# define O_WRONLY       GENERIC_WRITE
# define O_RDWR         O_RDONLY | O_WRONLY
# define FILE_SHARE_RW  FILE_SHARE_READ | FILE_SHARE_WRITE
# ifdef SEEK_SET
#  undef SEEK_SET
# endif
# define SEEK_SET       FILE_BEGIN
# ifdef SEEK_CUR
#  undef SEEK_CUR
# endif
# define SEEK_CUR       FILE_CURRENT
# ifdef SEEK_END
#  undef SEEK_END
# endif
# define SEEK_END       FILE_END

typedef HANDLE FileD;
typedef DWORD FileFlag;
typedef DWORD mprot_t;

#define INV_FD NULL
#define IS_VLD_FD(fd) (fd != INV_FD)
#define IS_INV_FD(fd) (fd == INV_FD)

#define PROT_READ  FILE_MAP_READ
#define PROT_WRITE FILE_MAP_WRITE

typedef intmax_t ssize_t;

EXPORT_FUNC
FileD open(const char *fn, FileFlag flags);
EXPORT_FUNC
void close(FileD fd);
EXPORT_FUNC
off_t lseek(FileD fd, off_t offset, int whence);
EXPORT_FUNC
ssize_t read(FileD fd, void *buf, size_t count);

#else
# error "*** ERROR: Unknown OS. ***"
#endif

#ifndef MAP_FAILED
# define MAP_FAILED ((void *) -1)
#endif

/***
 * @brief return size of according file
 *
 * @param[in] fd file descriptor
 *
 * @return size of files or -1.
 */
EXPORT_FUNC
size_t get_file_size(FileD fd);

/***
 * @brief read data from offset position in file
 *
 * @param[in] fd target file descriptor.
 * @param[in] off offset position in file. If off is equal NULL, function will
 *                read from current position in file.
 * @param[in] size amount of bytes, that should be readed.
 *
 * @return point to read data or NULL point.
 *
 * Need to free memory.
 */
EXPORT_FUNC
void *readFromFile(FileD fd, const size_t *off, size_t size);

/***
 * @brief compare two files with names a and b
 *
 * @param[in] a name of the first file
 * @param[in] b name of the second file
 *
 * @return 0 if equal, or line number where diff
 *         -1 if fail
 */
EXPORT_FUNC
int cmpFiles(const char *a, const char *b);

EXPORT_FUNC
void *map_file(FileD fd, size_t fs, mprot_t prot);

/***
 *
 */
EXPORT_FUNC
int unmap_file(void *addr, size_t fileSize);

#endif /* _FILE_H */


/***
 * MIT License
 *
 * Copyright (c) 2020 Konychev Valera
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

#define INV_FD -1
#define IS_VLD_FD(fd) (fd >= 0)
#define IS_INV_FD(fd) (fd < 0)

/***
 * \def mapFileForRead
 *
 * \param[in] fd file descriptor
 * \param[out] fileSize size of file
 *
 * \return addr of mapped file or MAP_FAILED
 */
void *mapFileForRead(FileD fd, size_t fileSize);

/***
 *
 */
int unmapFile(void *addr, size_t fileSize);

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

#define INV_FD NULL
#define IS_VLD_FD(fd) (fd != INV_FD)
#define IS_INV_FD(fd) (fd == INV_FD)

FileD open(const char *fn, FileFlag flags);
void close(FileD fd);
off_t lseek(FileD fd, off_t offset, int whence);
typedef intmax_t ssize_t;
ssize_t read(FileD fd, void *buf, size_t count);

#else
# error "*** ERROR: Unknown OS. ***"
#endif

#ifndef MAP_FAILED
# define MAP_FAILED ((void *) -1)
#endif

/**
 * \def getFileSize
 *
 * \param[in] fd file descriptor
 *
 * \return size of files or -1.
 */
size_t getFileSize(FileD fd);

/*
 * Description: read data from offset position in file.
 * Input:
 *  @fd  - target file descriptor.
 *  @off - offset position in file. If off is equal NULL, function will
 *          read from current position in file.
 *  @size- amount of bytes, that should be readed.
 * Output:
 *  Success:
 *      point to readed data.
 *  Fail:
 *      NULL point.
 * After:
 *  Need to free memory.
 */
void *readFromFile(FileD fd, size_t *off, size_t size);

#endif /* _FILE_H */


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

#include "mem.h"
#include "file.h"
#include "comdef.h"

// OS standard headers
#if defined(__UNIX__) || defined(__LINUX__) || defined(__MAC_OS_X__)
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>

size_t getFileSize(int fd)
{
    if (fd < 0) {
        ERROR("Invalid arguments.");
        return (size_t)-1;
    }

    struct stat st;
    if (fstat(fd, &st)) {
        PERROR("fstat");
        return (size_t)-1;
    }

    return (size_t)st.st_size;
}

void *readFromFile(int fd, size_t *off, size_t size)
{
    if (fd < 0) {
        STDERROR_PRINT_DEBUG("Invalid arguments.");
        return NULL;
    }

    if (off)
        if (lseek(fd, (off_t)*off, SEEK_SET) < 0) {
            PERROR_DEBUG("lseek()");
            return NULL;
        }

    void *data = Malloc(size);
    if (data == NULL) {
        STDERROR_PRINT_DEBUG("Can't allocate %zu bytes.", size);
        return NULL;
    }

    ssize_t num = 0;
    void *buf = data;
    while ((num = read(fd, buf, size)) > 0) {
        size -= (size_t)num;
        buf = (void*)((size_t)buf + (size_t)num);
    }

    if (num < 0) {
        PERROR_DEBUG("read()");
        Free(data);
        return NULL;
    }

    return data;
}

void *mapFileForRead(int fd, size_t fileSize)
{
    if (fd < 0 || fileSize == (size_t)-1) {
        ERROR("Invalid arguments.");
        return MAP_FAILED;
    }

    fileSize = alignUpToPageSize(fileSize);
    return mmap(NULL, fileSize, PROT_READ, MAP_PRIVATE | MAP_FILE, fd, 0);
}

int unmapFile(void *addr, size_t fileSize)
{
    return munmap(addr, fileSize);
}

#elif defined(__WIN__)
#include <Windows.h>

FileD open(const char *fn, FileFlag flags)
{
    return CreateFile(  fn,                     // file to open
                        flags,                  // purpose flags
                        FILE_SHARE_RW,          // share for reading
                        NULL,                   // default security
                        OPEN_EXISTING,          // existing file only
                        FILE_ATTRIBUTE_NORMAL,  // normal file
                        NULL);
}

void close(FileD fd)
{
    CloseHandle(fd);
}

off_t lseek(FileD fd, off_t offset, int whence)
{
    // !TODO: work only with 32bits offset
    return (off_t)SetFilePointer(fd, offset, NULL, (DWORD)whence);
}

ssize_t read(FileD fd, void *buf, size_t count)
{
    DWORD num = 0;
    BOOL r = ReadFile(fd, buf, (DWORD)count, &num, NULL);
    if (r)
        return num;
    else
        return -1;
}

size_t getFileSize(FileD fd)
{
    return GetFileSize(fd, NULL);
}

void *readFromFile(FileD fd, size_t *off, size_t size)
{
    if (fd == NULL) {
        STDERROR_PRINT_DEBUG("Invalid arguments.");
        return NULL;
    }

    if (off)
        if (lseek(fd, (off_t)*off, SEEK_SET) < 0) {
            PERROR_DEBUG("lseek()");
            return NULL;
        }

    void *data = Malloc(size);
    if (data == NULL) {
        STDERROR_PRINT_DEBUG("Can't allocate %zu bytes.", size);
        return NULL;
    }

    ssize_t num = 0;
    void *buf = data;
    while ((num = read(fd, buf, size)) > 0) {
        size -= (size_t)num;
        buf = (void*)((size_t)buf + (size_t)num);
    }

    if (num < 0) {
        PERROR_DEBUG("read()");
        Free(data);
        return NULL;
    }

    return data;
}

#else
# error "*** ERROR: Unknown OS. ***"
#endif


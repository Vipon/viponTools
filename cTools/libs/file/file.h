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

// C standard headers
#include <stddef.h>


#ifndef MAP_FAILED
# define MAP_FAILED ((void *) -1)
#endif

/**
 * \def getFileSize
 *
 * \param[in] fd file descriptor
 *
 *  \return size of files or -1.
 */
size_t getFileSize(int fd);


/**
 * \def mapFileForRead
 *
 * \param[in] fd file descriptor
 * \param[out] fileSize size of file
 *
 * \return addr of mapped file or MAP_FAILED
 */
void *mapFileForRead(int fd, size_t fileSize);


/**
 *
 */
int unmapFile(void *addr, size_t fileSize);

#endif /* _FILE_H */

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

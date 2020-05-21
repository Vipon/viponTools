#include "mem.h"
#include "file.h"
#include "comdef.h"

// OS standard headers
#if defined(unix) || defined(__unix) || \
    defined(__unix__) || (defined(__APPLE__) && defined(__MACH__))
    #include <fcntl.h>
    #include <unistd.h>
    #include <sys/mman.h>
    #include <sys/stat.h>
#else
    #error "*** ERROR: Unknown OS. ***"
#endif


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

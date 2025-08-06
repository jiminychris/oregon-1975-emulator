#include <sys/stat.h>
#include <sys/ioctl.h>
#include <stdlib.h>
#include <fcntl.h>
#include <poll.h>

void Exit(int rval)
{
    return exit(rval);
}

ssize_t Read(int fildes, void *buf, size_t nbyte)
{
    return read(fildes, buf, nbyte);
}

ssize_t Write(int fildes, const void *buf, size_t nbyte)
{
    return write(fildes, buf, nbyte);
}

int Open(const char *pathname, int flags, mode_t mode)
{
    return open(pathname, flags, mode);
}

int Close(int fd)
{
    return close(fd);
}

int InputOutputControl(int fd, int request, size_t arg)
{
    return ioctl(fd, request, arg);
}

int FileControl(int fd, int op, size_t arg)
{
    return fcntl(fd, op, arg);
}

int ChangeMode(int fd, mode_t mode)
{
    return fchmod(fd, mode);
}

off_t Seek(int fd, off_t offset, int whence)
{
    return lseek(fd, offset, whence);
}

int Poll(struct pollfd fds[], size_t nfds, int timeout)
{
    return poll(fds, nfds, timeout);
}

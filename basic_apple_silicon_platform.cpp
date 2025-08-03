#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/ttycom.h>

#define STR_HELPER(X) #X
#define STR(X) STR_HELPER(X)

#define SYSCALL_READ 3
#define SYSCALL_WRITE 4
#define SYSCALL_OPEN 5
#define SYSCALL_CLOSE 6
#define SYSCALL_CHMOD 15
#define SYSCALL_IOCTL 54
#define SYSCALL_FCNTL 92
#define SYSCALL_LSEEK 199
#define SYSCALL_POLL 230

#define SYSCALL_1(X, A)                                      \
    ssize_t Result;                                          \
    asm volatile (                                           \
        "mov x0, %1\n"                                       \
        "mov x16, #" STR(X) "\n"                             \
        "svc #0x80\n"                                        \
        "mov %0, x0\n"                                       \
        : "=r"(Result)                                       \
        : "r"((ssize_t)A)                                    \
        : "x0", "x16"                                        \
        );                                                   \
    return Result

#define SYSCALL_2(X, A, B)                                   \
    ssize_t Result;                                          \
    asm volatile (                                           \
        "mov x0, %1\n"                                       \
        "mov x1, %2\n"                                       \
        "mov x16, #" STR(X) "\n"                             \
        "svc #0x80\n"                                        \
        "mov %0, x0\n"                                       \
        : "=r"(Result)                                       \
        : "r"((ssize_t)A), "r"((ssize_t)B)                   \
        : "x0", "x1", "x16"                                  \
        );                                                   \
    return Result

#define SYSCALL_3(X, A, B, C)                                \
    ssize_t Result;                                          \
    asm volatile (                                           \
        "mov x0, %1\n"                                       \
        "mov x1, %2\n"                                       \
        "mov x2, %3\n"                                       \
        "mov x16, #" STR(X) "\n"                             \
        "svc #0x80\n"                                        \
        "mov %0, x0\n"                                       \
        : "=r"(Result)                                       \
        : "r"((ssize_t)A), "r"((ssize_t)B), "r"((size_t)C)   \
        : "x0", "x1", "x2", "x16"                            \
        );                                                   \
    return Result

ssize_t Read(int fildes, void *buf, size_t nbyte)
{
    SYSCALL_3(SYSCALL_READ, fildes, buf, nbyte);
}

ssize_t Write(int fildes, const void *buf, size_t nbyte)
{
    SYSCALL_3(SYSCALL_WRITE, fildes, buf, nbyte);
}

int Open(const char *pathname, int flags, mode_t mode = 0)
{
    SYSCALL_3(SYSCALL_OPEN, pathname, flags, mode);
}

int Close(int fd)
{
    SYSCALL_1(SYSCALL_CLOSE, fd);
}

int ChangeMode(int fd, mode_t mode)
{
    SYSCALL_2(SYSCALL_CHMOD, fd, mode);
}

int InputOutputControl(int fd, int request, size_t arg)
{
    SYSCALL_3(SYSCALL_IOCTL, fd, request, arg);
}

int FileControl(int fd, int op, size_t arg = 0)
{
    SYSCALL_3(SYSCALL_FCNTL, fd, op, arg);
}

off_t Seek(int fd, off_t offset, int whence)
{
    SYSCALL_3(SYSCALL_LSEEK, fd, offset, whence);
}

int Poll(struct pollfd fds[], size_t nfds, int timeout)
{
    SYSCALL_3(SYSCALL_POLL, fds, nfds, timeout);
}

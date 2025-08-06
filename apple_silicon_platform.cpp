#include <fcntl.h>
#include <unistd.h>
#include <poll.h>

#define STR_HELPER(X) #X
#define STR(X) STR_HELPER(X)

#define SYSCALL_EXIT 1
#define SYSCALL_READ 3
#define SYSCALL_WRITE 4
#define SYSCALL_OPEN 5
#define SYSCALL_CLOSE 6
#define SYSCALL_IOCTL 54
#define SYSCALL_FCNTL 92
#define SYSCALL_FCHMOD 124
#define SYSCALL_LSEEK 199
#define SYSCALL_POLL 230

#define SYSCALL_1(Result, X, A)                               \
    ssize_t Result;                                          \
    asm volatile (                                           \
        "mov x0, %1\n"                                       \
        "mov x16, #" STR(X) "\n"                             \
        "svc #0x80\n"                                        \
        "mov %0, x0\n"                                       \
        : "=r"(Result)                                       \
        : "r"((ssize_t)A)                                    \
        : "x0", "x16"                                        \
        )

#define SYSCALL_2(Result, X, A, B)                            \
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
        )

#define SYSCALL_3(Result, X, A, B, C)                        \
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
        )

void Exit(int rval)
{
    SYSCALL_1(Result, SYSCALL_EXIT, rval);
}

ssize_t Read(int fildes, void *buf, size_t nbyte)
{
    SYSCALL_3(Result, SYSCALL_READ, fildes, buf, nbyte);
    return Result;
}

ssize_t Write(int fildes, const void *buf, size_t nbyte)
{
    SYSCALL_3(Result, SYSCALL_WRITE, fildes, buf, nbyte);
    return Result;
}

int Open(const char *pathname, int flags, mode_t mode = 0)
{
    SYSCALL_3(Result, SYSCALL_OPEN, pathname, flags, mode);
    return Result <= STDERR_FILENO ? -1 : Result;
}

int Close(int fd)
{
    SYSCALL_1(Result, SYSCALL_CLOSE, fd);
    return Result;
}

int InputOutputControl(int fd, int request, size_t arg)
{
    SYSCALL_3(Result, SYSCALL_IOCTL, fd, request, arg);
    return Result;
}

int FileControl(int fd, int op, size_t arg = 0)
{
    SYSCALL_3(Result, SYSCALL_FCNTL, fd, op, arg);
    return Result;
}

int ChangeMode(int fd, mode_t mode)
{
    SYSCALL_2(Result, SYSCALL_FCHMOD, fd, mode);
    return Result;
}

off_t Seek(int fd, off_t offset, int whence)
{
    SYSCALL_3(Result, SYSCALL_LSEEK, fd, offset, whence);
    return Result;
}

int Poll(struct pollfd fds[], size_t nfds, int timeout)
{
    SYSCALL_3(Result, SYSCALL_POLL, fds, nfds, timeout);
    return Result;
}

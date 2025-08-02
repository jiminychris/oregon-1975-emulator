#include <string.h>
#include <unistd.h>

#include "primitives.h"

ssize_t write(int fildes, const void *buf, size_t nbyte)
{
    ssize_t Result = 0;
    asm volatile (
        "mov x0, %1\n"      // fd (e.g., 1 for stdout)
        "mov x1, %2\n"      // pointer to buffer
        "mov x2, %3\n"      // length of buffer
        "mov x16, #4\n"     // syscall number for write
        "svc #0x80\n"       // invoke syscall
        "mov %0, x0\n"      // return value
        : "=r"(Result)
        : "r"((s64)fildes), "r"(buf), "r"(nbyte) // Input operands
        : "x0", "x1", "x2", "x16" // Clobbered registers
        );
    return Result;
}

void *memcpy(void *dest, const void *src, size_t count)
{
    u8 *Dest = (u8*)dest;
    u8 *Source = (u8*)src;
    while (count--)
    {
        *Dest++ = *Source++;
    }
    return dest;
}

void *memset(void *dest, int ch, size_t count)
{
    u8 *Dest = (u8*)dest;
    while (count--)
    {
        *Dest++ = ch;
    }
    return dest;
}

#include "main.cpp"

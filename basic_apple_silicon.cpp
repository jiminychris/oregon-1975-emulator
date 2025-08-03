#include "basic_apple_silicon_platform.cpp"
#include "primitives.h"

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

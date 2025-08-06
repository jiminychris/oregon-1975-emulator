#include "primitives.h"

extern "C" {
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
}

#if 1
#include "apple_silicon_platform.cpp"
#else
#include "posix_platform.cpp"
#endif

#include "common.cpp"
#include "main.cpp"

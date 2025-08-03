#include <stdint.h>

#define u8 uint8_t
#define u16 uint16_t
#define u32 uint32_t
#define u64 uint64_t

#define s8 int8_t
#define s16 int16_t
#define s32 int32_t
#define s64 int64_t

#define r32 float
#define r64 double

int Create(const char *pathname, mode_t mode)
{
    return Open(pathname, O_CREAT|O_WRONLY|O_TRUNC, mode);
}

signed char ReadCharacter()
{
    signed char Result = EOF;
    Read(STDIN_FILENO, &Result, 1);
    return Result;
}

int WriteCharacter(int Character)
{
    int Result = Character;
    ssize_t BytesWritten = Write(STDOUT_FILENO, &Character, 1);
    if (BytesWritten < 0)
    {
        Result = EOF;
    }
    return Result;
}

s32 FlagIsSet(s32 Field, s32 Flag)
{
    return Flag == (Field & Flag);
}

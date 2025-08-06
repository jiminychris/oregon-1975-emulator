internal inline int Create(const char *pathname, mode_t mode)
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

internal inline bool FlagIsSet(s32 Field, s32 Flag)
{
    return Flag == (Field & Flag);
}

internal inline bool IsDigit(int c)
{
    return '0' <= c && c <= '9';
}

internal inline bool IsUpper(int c)
{
    return 'A' <= c && c <= 'Z';
}

internal inline bool IsLower(int c)
{
    return 'a' <= c && c <= 'z';
}

internal inline bool IsAlpha (int c)
{
    return IsUpper(c) || IsLower(c);
}

internal inline bool IsUpperOrDigit(int Char)
{
    return IsDigit(Char) || IsUpper(Char);
}

internal inline bool IsWhitespace(int Char)
{
    return Char == ' ' || Char == '\n' || Char == '\r' || Char == '\t';
}

internal inline bool IsIntralineWhitespace(int Char)
{
    return Char == ' ' || Char == '\t';
}

internal inline bool IsPrintable(int Char)
{
    return 32 <= Char && Char <= 126;
}

size_t Min(size_t A, size_t B)
{
    return A < B ? A : B;
}

size_t Max(size_t A, size_t B)
{
    return A > B ? A : B;
}

s32 Min(s32 A, s32 B)
{
    return A < B ? A : B;
}

s32 Max(s32 A, s32 B)
{
    return A > B ? A : B;
}

u64 Min(u64 A, u64 B)
{
    return A < B ? A : B;
}

u64 Max(u64 A, u64 B)
{
    return A > B ? A : B;
}

r32 Min(r32 A, r32 B)
{
    return A < B ? A : B;
}

r32 Max(r32 A, r32 B)
{
    return A > B ? A : B;
}

r64 Min(r64 A, r64 B)
{
    return A < B ? A : B;
}

r64 Max(r64 A, r64 B)
{
    return A > B ? A : B;
}

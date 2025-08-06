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

r32 Power(r32 Base, s32 Exponent)
{
    r32 Result = 1.0f;
    bool Invert = Exponent < 0;
    if (Invert) {
        Exponent *= -1;
    }
    while (Exponent--)
    {
        Result *= Base;
    }
    if (Invert)
    {
        Result = 1.0f / Result;
    }
    return Result;
}

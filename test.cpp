string_reference ParseTest(environment *Environment, string_reference ExpressionString, lexeme *Value)
{
    Reset(Environment);
    parser *Parser = &Environment->Parser;
    memory_arena *Arena = &Parser->StringArena;
    Parser->Size = ExpressionString.Length;
    Parser->Contents = (u8*)ExpressionString.Memory;
    Lex(Parser);
    lexeme *Expr = Expression(Parser);
    EvaluateExpression(Environment, Expr, Value);
    string_reference Actual = BeginString(Arena);
    Actual.Length = RenderExpression(Expr, Arena);
    return Actual;
}

int ExpressionTest(environment *Environment, string_reference ExpressionString, string_reference Expected, s32 ExpectedValue)
{
    int Result = 0;
    lexeme Value;
    string_reference Actual = ParseTest(Environment, ExpressionString, &Value);
    if (Equals(Expected, Actual) && (Value.Type == token_type_BOOLEAN || Value.Type == token_type_INTEGER) && Value.Integer == ExpectedValue)
    {
#if PRINT_SUCCESSFUL_TESTS
        printf("Expecting: %.*s=%d . . . ", Expected.Length, Expected.Memory, ExpectedValue);
        printf("\033[32mPASS\033[0m\n");
#endif
    }
    else
    {
        printf("Expecting: %.*s=%d . . . ", Expected.Length, Expected.Memory, ExpectedValue);
        printf("\033[31mFAIL\nActual:    %.*s=%d\033[0m\n", Actual.Length, Actual.Memory, Value.Integer);
        Result = 1;
    }
    return Result;
}

#define EPSILON 0.00001f

int AboutEqual(r32 A, r32 B)
{
    return fabs(A - B) < EPSILON;
}

int ExpressionTest(environment *Environment, string_reference ExpressionString, string_reference Expected, r64 ExpectedValue)
{
    int Result = 0;
    lexeme Value;
    string_reference Actual = ParseTest(Environment, ExpressionString, &Value);
    if (Equals(Expected, Actual) && Value.Type == token_type_REAL && AboutEqual(Value.Real, ExpectedValue))
    {
#if PRINT_SUCCESSFUL_TESTS
        printf("Expecting: %.*s=%g . . . ", Expected.Length, Expected.Memory, ExpectedValue);
        printf("\033[32mPASS\033[0m\n");
#endif
    }
    else
    {
        printf("Expecting: %.*s=%g . . . ", Expected.Length, Expected.Memory, ExpectedValue);
        printf("\033[31mFAIL\nActual:    %.*s=%g\033[0m\n", Actual.Length, Actual.Memory, Value.Real);
        Result = 1;
    }
    return Result;
}

int ExpressionTest(environment *Environment, string_reference ExpressionString, string_reference Expected, char ExpectedValue)
{
    int Result = 0;
    lexeme Value;
    string_reference Actual = ParseTest(Environment, ExpressionString, &Value);
    if (Equals(Expected, Actual) && Value.Type == token_type_CHAR && Value.Character == ExpectedValue)
    {
#if PRINT_SUCCESSFUL_TESTS
        printf("Expecting: %.*s='%d . . . ", Expected.Length, Expected.Memory, ExpectedValue);
        printf("\033[32mPASS\033[0m\n");
#endif
    }
    else
    {
        printf("Expecting: %.*s='%d . . . ", Expected.Length, Expected.Memory, ExpectedValue);
        printf("\033[31mFAIL\nActual:    %.*s='%d\033[0m\n", Actual.Length, Actual.Memory, Value.Character);
        Result = 1;
    }
    return Result;
}

int ExpressionTest(environment *Environment, string_reference ExpressionString, string_reference Expected, const char *ExpectedValue)
{
    string_builder Builder;
    Reset(&Builder);
    RenderString(StringReference(ExpectedValue), &Builder.Arena);
    string_reference ExpectedValueString = StringReference(&Builder);

    int Result = 0;
    lexeme Value;
    string_reference Actual = ParseTest(Environment, ExpressionString, &Value);
    if (Equals(Expected, Actual) && Value.Type == token_type_STRING && Equals(Value.String, ExpectedValue))
    {
#if PRINT_SUCCESSFUL_TESTS
        printf("Expecting: %.*s=%.*s . . . ", Expected.Length, Expected.Memory, ExpectedValueString.Length, ExpectedValueString.Memory);
        printf("\033[32mPASS\033[0m\n");
#endif
    }
    else
    {
        printf("Expecting: %.*s=%.*s . . . ", Expected.Length, Expected.Memory, ExpectedValueString.Length, ExpectedValueString.Memory);
        printf("\033[31mFAIL\nActual:    %.*s=%.*s\033[0m\n", Actual.Length, Actual.Memory, Value.String.Length, Value.String.Memory);
        Result = 1;
    }
    return Result;
}

int ExpectEquals(string_reference A, const char *B)
{
    u8 Success = Equals(A, B);
    if (Success)
    {
#if PRINT_SUCCESSFUL_TESTS
        printf("Expecting: STRING_REFERENCE(\"%.*s\") = \"%s\" . . . ", A.Length, A.Memory, B);
        printf("\033[32mPASS\033[0m\n");
#endif
    }
    else
    {
        printf("Expecting: STRING_REFERENCE(\"%.*s\") = \"%s\" . . . ", A.Length, A.Memory, B);
        printf("\033[31mFAIL\033[0m\n");
    }
    return !Success;
}

int ExpectNotEquals(string_reference A, const char *B)
{
    u8 Success = !Equals(A, B);
    if (Success)
    {
#if PRINT_SUCCESSFUL_TESTS
        printf("Expecting: STRING_REFERENCE(\"%.*s\") != \"%s\" . . . ", A.Length, A.Memory, B);
        printf("\033[32mPASS\033[0m\n");
#endif
    }
    else
    {
        printf("Expecting: STRING_REFERENCE(\"%.*s\") != \"%s\" . . . ", A.Length, A.Memory, B);
        printf("\033[31mFAIL\033[0m\n");
    }
    return !Success;
}

int ExpectEquals(buffer A, const char *B)
{
    u8 Success = Equals(A, B);
    if (Success)
    {
#if PRINT_SUCCESSFUL_TESTS
        printf("Expecting: BUFFER(\"%.*s\") = \"%s\" . . . ", (int)(A.At - A.Contents), A.Contents, B);
        printf("\033[32mPASS\033[0m\n");
#endif
    }
    else
    {
        printf("Expecting: BUFFER(\"%.*s\") = \"%s\" . . . ", (int)(A.At - A.Contents), A.Contents, B);
        printf("\033[31mFAIL\033[0m\n");
    }
    return !Success;
}

int ExpectNotEquals(buffer A, const char *B)
{
    u8 Success = !Equals(A, B);
    if (Success)
    {
#if PRINT_SUCCESSFUL_TESTS
        printf("Expecting: BUFFER(\"%.*s\") != \"%s\" . . . ", (int)(A.At - A.Contents), A.Contents, B);
        printf("\033[32mPASS\033[0m\n");
#endif
    }
    else
    {
        printf("Expecting: BUFFER(\"%.*s\") != \"%s\" . . . ", (int)(A.At - A.Contents), A.Contents, B);
        printf("\033[31mFAIL\033[0m\n");
    }
    return !Success;
}

int ExpectEquals(string_reference A, string_reference B)
{
    u8 Success = Equals(A, B);
    if (Success)
    {
#if PRINT_SUCCESSFUL_TESTS
        printf("Expecting: STRING_REFERENCE(\"%.*s\") = STRING_REFERENCE(\"%.*s\") . . . ", A.Length, A.Memory, B.Length, B.Memory);
        printf("\033[32mPASS\033[0m\n");
#endif
    }
    else
    {
        printf("Expecting: STRING_REFERENCE(\"%.*s\") = STRING_REFERENCE(\"%.*s\") . . . ", A.Length, A.Memory, B.Length, B.Memory);
        printf("\033[31mFAIL\033[0m\n");
    }
    return !Success;
}

int ExpectNotEquals(string_reference A, string_reference B)
{
    u8 Success = !Equals(A, B);
    if (Success)
    {
#if PRINT_SUCCESSFUL_TESTS
        printf("Expecting: STRING_REFERENCE(\"%.*s\") != STRING_REFERENCE(\"%.*s\") . . . ", A.Length, A.Memory, B.Length, B.Memory);
        printf("\033[32mPASS\033[0m\n");
#endif
    }
    else
    {
        printf("Expecting: STRING_REFERENCE(\"%.*s\") != STRING_REFERENCE(\"%.*s\") . . . ", A.Length, A.Memory, B.Length, B.Memory);
        printf("\033[31mFAIL\033[0m\n");
    }
    return !Success;
}

int ExpectEquals(const char *A, const char *B)
{
    u8 Success = Equals(A, B);
    if (Success)
    {
#if PRINT_SUCCESSFUL_TESTS
        printf("Expecting: \"%s\" = \"%s\" . . . ", A, B);
        printf("\033[32mPASS\033[0m\n");
#endif
    }
    else
    {
        printf("Expecting: \"%s\" = \"%s\" . . . ", A, B);
        printf("\033[31mFAIL\033[0m\n");
    }
    return !Success;
}

int ExpectNotEquals(const char *A, const char *B)
{
    u8 Success = !Equals(A, B);
    if (Success)
    {
#if PRINT_SUCCESSFUL_TESTS
        printf("Expecting: \"%s\" != \"%s\" . . . ", A, B);
        printf("\033[32mPASS\033[0m\n");
#endif
    }
    else
    {
        printf("Expecting: \"%s\" != \"%s\" . . . ", A, B);
        printf("\033[31mFAIL\033[0m\n");
    }
    return !Success;
}

#define EXPRESSION_TEST(Environment, ExpressionString, ExpectedString, ExpectedValue) ExpressionTest(Environment, STRING_REFERENCE(ExpressionString), STRING_REFERENCE(ExpectedString), ExpectedValue)
#define STRING_TEST(T, A, B) T(A, B) + T(STRING_REFERENCE(A), B) + T(STRING_REFERENCE(A), STRING_REFERENCE(B)) + T(BUFFER(A), B)

int Test(environment *Environment)
{
    int Failures = 0;
    Failures += STRING_TEST(ExpectEquals, "Foo", "Foo");
    Failures += STRING_TEST(ExpectNotEquals, "", "Foo");
    Failures += STRING_TEST(ExpectNotEquals, "Foo", "");
    Failures += STRING_TEST(ExpectEquals, "", "");
    Failures += STRING_TEST(ExpectNotEquals, "Foobar", "Foo");
    Failures += STRING_TEST(ExpectNotEquals, "Foo", "Foobar");
    Failures += STRING_TEST(ExpectNotEquals, "xx", "x");
    Failures += STRING_TEST(ExpectNotEquals, "x", "xx");
    Failures += STRING_TEST(ExpectNotEquals, "x", "");
    Failures += STRING_TEST(ExpectNotEquals, "", "x");
    Failures += STRING_TEST(ExpectNotEquals, "x", "y");
    Failures += EXPRESSION_TEST(Environment, "'1", "'1", (char)1);
    Failures += EXPRESSION_TEST(Environment, "'1\"FOO\"", "'1\"FOO\"", "\x1""FOO");
    Failures += EXPRESSION_TEST(Environment, "\"FOO\"'1", "\"FOO\"'1", "FOO""\x1");
    Failures += EXPRESSION_TEST(Environment, "0", "0", 0);
    Failures += EXPRESSION_TEST(Environment, "1", "1", 1);
    Failures += EXPRESSION_TEST(Environment, "NOT 1", "(NOT 1)", !1);
    Failures += EXPRESSION_TEST(Environment, "NOT 0", "(NOT 0)", !0);
    Failures += EXPRESSION_TEST(Environment, "NOT -1", "(NOT (-1))", !-1);
    Failures += EXPRESSION_TEST(Environment, "0 OR 0", "(0 OR 0)", 0 || 0);
    Failures += EXPRESSION_TEST(Environment, "0 OR 1", "(0 OR 1)", 0 || 1);
    Failures += EXPRESSION_TEST(Environment, "1 OR 0", "(1 OR 0)", 1 || 0);
    Failures += EXPRESSION_TEST(Environment, "1 OR 1", "(1 OR 1)", 1 || 1);
    Failures += EXPRESSION_TEST(Environment, "0 AND 0", "(0 AND 0)", 0 && 0);
    Failures += EXPRESSION_TEST(Environment, "0 AND 1", "(0 AND 1)", 0 && 1);
    Failures += EXPRESSION_TEST(Environment, "1 AND 0", "(1 AND 0)", 1 && 0);
    Failures += EXPRESSION_TEST(Environment, "1 AND 1", "(1 AND 1)", 1 && 1);
    Failures += EXPRESSION_TEST(Environment, "1 AND 1 OR 1", "((1 AND 1) OR 1)", 1 && 1 || 1);
    Failures += EXPRESSION_TEST(Environment, "1 OR 1 AND 1", "(1 OR (1 AND 1))", 1 || 1 && 1);
    Failures += EXPRESSION_TEST(Environment, "32+2/3*18", "(32+((2/3)*18))", 32+2.0f/3.0f*18);
    Failures += EXPRESSION_TEST(Environment, "1+2*3+4", "((1+(2*3))+4)", 1+2*3+4);
    Failures += EXPRESSION_TEST(Environment, "1*2+3*4", "((1*2)+(3*4))", 1*2+3*4);
    Failures += EXPRESSION_TEST(Environment, "1*2=3*4", "((1*2)=(3*4))", 1*2==3*4);
    Failures += EXPRESSION_TEST(Environment, "4^3^2", "(4^(3^2))", pow(4, pow(3, 2)));
    Failures += EXPRESSION_TEST(Environment, "2-1", "(2-1)", 2-1);
    Failures += EXPRESSION_TEST(Environment, "1-2", "(1-2)", 1-2);
    Failures += EXPRESSION_TEST(Environment, "-1", "(-1)", -1);
    Failures += EXPRESSION_TEST(Environment, "--1", "(-(-1))", -(-1));
    Failures += EXPRESSION_TEST(Environment, "-3^2", "(-(3^2))", -pow(3, 2));
    Failures += EXPRESSION_TEST(Environment, "1+2*3", "(1+(2*3))", 1+2*3);
    Failures += EXPRESSION_TEST(Environment, "(1+2)*3", "((1+2)*3)", (1+2)*3);
    Failures += EXPRESSION_TEST(Environment, "0.5*10", "(0.5*10)", 0.5*10);
    Failures += EXPRESSION_TEST(Environment, "(20/100-4)^2+72", "((((20/100)-4)^2)+72)", pow(20.0f/100-4,2)+72);
    Failures += EXPRESSION_TEST(Environment, "(20/100-4)^2+12", "((((20/100)-4)^2)+12)", pow(20.0f/100-4,2)+12);
    Failures += EXPRESSION_TEST(Environment, "((20/100-4)^2+72)/((20/100-4)^2+12)-1", "((((((20/100)-4)^2)+72)/((((20/100)-4)^2)+12))-1)", (pow(20.0f/100-4,2)+72)/(pow(20.0f/100-4,2)+12)-1);
    Failures += EXPRESSION_TEST(Environment, "0.5*10>((20/100-4)^2+72)/((20/100-4)^2+12)-1", "((0.5*10)>((((((20/100)-4)^2)+72)/((((20/100)-4)^2)+12))-1))", 0.5*10>(pow(20.0f/100-4,2)+72)/(pow(20.0f/100-4,2)+12)-1);
    Failures += EXPRESSION_TEST(Environment, "0.3*10>((20/100-4)^2+72)/((20/100-4)^2+12)-1", "((0.3*10)>((((((20/100)-4)^2)+72)/((((20/100)-4)^2)+12))-1))", 0.3*10>(pow(20.0f/100-4,2)+72)/(pow(20.0f/100-4,2)+12)-1);
    Failures += EXPRESSION_TEST(Environment, "0.2*10>((20/100-4)^2+72)/((20/100-4)^2+12)-1", "((0.2*10)>((((((20/100)-4)^2)+72)/((((20/100)-4)^2)+12))-1))", 0.2*10>(pow(20.0f/100-4,2)+72)/(pow(20.0f/100-4,2)+12)-1);
    return Failures;
}

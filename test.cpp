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

size_t AppendExpectingHeader(string_builder *Builder, string_reference Expected)
{
    size_t Result = 0;
    Result += Append(Builder, "Expecting: ");
    Result += Append(Builder, Expected);
    Result += Append(Builder, '=');
    return Result;
}

size_t AppendExpectingFooter(string_builder *Builder)
{
    size_t Result = 0;
    Result += Append(Builder, " . . . ");
    return Result;
}

size_t AppendPass(string_builder *Builder)
{
    size_t Result = 0;
    Result += Append(Builder, "\033[32mPASS\033[0m");
    Result += Newline(Builder);
    return Result;
}

size_t AppendFailHeader(string_builder *Builder, string_reference Actual)
{
    size_t Result = 0;
    Result += Append(Builder, "\033[31mFAIL");
    Result += Newline(Builder);
    Result += Append(Builder, "Actual:    ");
    Result += Append(Builder, Actual);
    Result += Append(Builder, '=');
    return Result;
}

size_t AppendFailFooter(string_builder *Builder)
{
    size_t Result = 0;
    Result += Append(Builder, "\033[0m");
    Result += Newline(Builder);
    return Result;
}

#define APPEND_EXPECTING(Builder, Expected, ExpectedValue) {    \
        AppendExpectingHeader(Builder, Expected);               \
        Append(Builder, ExpectedValue);                         \
        AppendExpectingFooter(Builder);                         \
    }

#define APPEND_FAIL(Builder, Actual, ActualValue) { \
        AppendFailHeader(Builder, Actual);          \
        Append(Builder, ActualValue);               \
        AppendFailFooter(Builder);                  \
    }

int ExpressionTest(environment *Environment, string_reference ExpressionString, string_reference Expected, s32 ExpectedValue)
{
    lexeme Value;
    string_builder Builder;
    Reset(&Builder);
    string_reference Actual = ParseTest(Environment, ExpressionString, &Value);
    int Success = Equals(Expected, Actual) && (Value.Type == token_type_BOOLEAN || Value.Type == token_type_INTEGER) && Value.Integer == ExpectedValue;
    if (Success)
    {
#if PRINT_SUCCESSFUL_TESTS
        APPEND_EXPECTING(&Builder, Expected, ExpectedValue);
        AppendPass(&Builder);
#endif
    }
    else
    {
        APPEND_EXPECTING(&Builder, Expected, ExpectedValue);
        APPEND_FAIL(&Builder, Actual, Value.Integer);
    }
    Print(&Builder);
    return !Success;
}

#define EPSILON 0.00001f

int AboutEqual(r32 A, r32 B)
{
    return fabs(A - B) < EPSILON;
}

int ExpressionTest(environment *Environment, string_reference ExpressionString, string_reference Expected, r64 ExpectedValue)
{
    lexeme Value;
    string_builder Builder;
    Reset(&Builder);
    string_reference Actual = ParseTest(Environment, ExpressionString, &Value);
    int Success = Equals(Expected, Actual) && Value.Type == token_type_REAL && AboutEqual(Value.Real, ExpectedValue);
    if (Success)
    {
#if PRINT_SUCCESSFUL_TESTS
        APPEND_EXPECTING(&Builder, Expected, ExpectedValue);
        AppendPass(&Builder);
#endif
    }
    else
    {
        APPEND_EXPECTING(&Builder, Expected, ExpectedValue);
        APPEND_FAIL(&Builder, Actual, Value.Real);
    }
    Print(&Builder);
    return !Success;
}

int ExpressionTest(environment *Environment, string_reference ExpressionString, string_reference Expected, char ExpectedValue)
{
    lexeme Value;
    string_builder Builder;
    Reset(&Builder);
    string_builder ExpectedValueBuilder;
    Reset(&ExpectedValueBuilder);
    RenderString({1, &ExpectedValue}, &ExpectedValueBuilder.Arena);
    string_reference ExpectedValueString = StringReference(&ExpectedValueBuilder);

    string_reference Actual = ParseTest(Environment, ExpressionString, &Value);
    int Success = Equals(Expected, Actual) && Value.Type == token_type_CHAR && Value.Character == ExpectedValue;
    if (Success)
    {
#if PRINT_SUCCESSFUL_TESTS
        APPEND_EXPECTING(&Builder, Expected, ExpectedValueString);
        AppendPass(&Builder);
#endif
    }
    else
    {
        APPEND_EXPECTING(&Builder, Expected, ExpectedValue);
        APPEND_FAIL(&Builder, Actual, Value.Character);
    }
    Print(&Builder);
    return !Success;
}

int ExpressionTest(environment *Environment, string_reference ExpressionString, string_reference Expected, const char *ExpectedValue)
{
    string_builder Builder;
    Reset(&Builder);
    string_builder ExpectedValueBuilder;
    Reset(&ExpectedValueBuilder);
    RenderString(StringReference(ExpectedValue), &ExpectedValueBuilder.Arena);
    string_reference ExpectedValueString = StringReference(&ExpectedValueBuilder);

    lexeme Value;
    string_reference Actual = ParseTest(Environment, ExpressionString, &Value);
    int Success = Equals(Expected, Actual) && Value.Type == token_type_STRING && Equals(Value.String, ExpectedValue);
    if (Success)
    {
#if PRINT_SUCCESSFUL_TESTS
        APPEND_EXPECTING(&Builder, Expected, ExpectedValueString);
        AppendPass(&Builder);
#endif
    }
    else
    {
        APPEND_EXPECTING(&Builder, Expected, ExpectedValue);
        APPEND_FAIL(&Builder, Actual, Value.String);
    }
    Print(&Builder);
    return !Success;
}

size_t AppendWrappedString(string_builder *Builder, const char *String)
{
    size_t Result = 0;
    Append(Builder, '"');
    Append(Builder, String);
    Append(Builder, '"');
    return Result;
}

#define APPEND_WRAPPED_STRING(Result, Builder, Type, String) {   \
    Result += Append(Builder, Type);                            \
    Result += Append(Builder, "(\"");                             \
    Result += Append(Builder, String);                                            \
    Result += Append(Builder, "\")");                                               \
    }
size_t AppendWrappedString(string_builder *Builder, string_reference String)
{
    size_t Result = 0;
    APPEND_WRAPPED_STRING(Result, Builder, "STRING_REFERENCE", String);
    return Result;
}

size_t AppendWrappedString(string_builder *Builder, buffer String)
{
    size_t Result = 0;
    APPEND_WRAPPED_STRING(Result, Builder, "BUFFER", String);
    return Result;
}

#define ExpectEquals(Failures, A, B) {                              \
        string_builder Builder;                                     \
        Reset(&Builder);                                            \
        u8 Success = Equals(A, B);                                  \
        if (Success)                                                \
        {                                                           \
            if(PRINT_SUCCESSFUL_TESTS)                              \
            {                                                       \
                Append(&Builder, "Expecting: ");                  \
                AppendWrappedString(&Builder, A);                                \
                Append(&Builder, " = ");                        \
                AppendWrappedString(&Builder, B);                                \
                Append(&Builder, " . . . ");                      \
                AppendPass(&Builder);                               \
            }                                                       \
        }                                                           \
        else                                                        \
        {                                                           \
            Append(&Builder, "Expecting: ");                      \
            AppendWrappedString(&Builder, A);                                    \
            Append(&Builder, " = ");                            \
            AppendWrappedString(&Builder, B);                                    \
            Append(&Builder, " . . . ");                          \
            Append(&Builder, "\033[31mFAIL\033[0m");                \
            Newline(&Builder);                                      \
        }                                                           \
        Print(&Builder);                                            \
        Failures += !Success;                                       \
    }

#define ExpectNotEquals(Failures, A, B) {       \
        string_builder Builder;                 \
        Reset(&Builder);                        \
        u8 Success = !Equals(A, B);              \
        if (Success)                            \
        {                                           \
            if(PRINT_SUCCESSFUL_TESTS)                              \
            {                                                       \
                Append(&Builder, "Expecting: ");                  \
                AppendWrappedString(&Builder, A);                                \
                Append(&Builder, " != ");                        \
                AppendWrappedString(&Builder, B);                                \
                Append(&Builder, " . . . ");                      \
                AppendPass(&Builder);                               \
            }                                                       \
        }                                                           \
        else                                                        \
        {                                                           \
            Append(&Builder, "Expecting: ");                      \
            AppendWrappedString(&Builder, A);                                    \
            Append(&Builder, " != ");                           \
            AppendWrappedString(&Builder, B);                                    \
            Append(&Builder, " . . . ");                          \
            Append(&Builder, "\033[31mFAIL\033[0m");                \
            Newline(&Builder);                                      \
        }                                                           \
        Print(&Builder);                                            \
        Failures += !Success;                                       \
    }

#define EXPRESSION_TEST(Environment, ExpressionString, ExpectedString, ExpectedValue) ExpressionTest(Environment, STRING_REFERENCE(ExpressionString), STRING_REFERENCE(ExpectedString), ExpectedValue)
#define STRING_TEST(Failures, T, A, B) T(Failures, A, B); T(Failures, STRING_REFERENCE(A), B); T(Failures, STRING_REFERENCE(A), STRING_REFERENCE(B)); T(Failures, BUFFER(A), B);

int Test(environment *Environment)
{
    int Failures = 0;
    STRING_TEST(Failures, ExpectEquals, "Foo", "Foo");
    STRING_TEST(Failures, ExpectNotEquals, "", "Foo");
    STRING_TEST(Failures, ExpectNotEquals, "Foo", "");
    STRING_TEST(Failures, ExpectEquals, "", "");
    STRING_TEST(Failures, ExpectNotEquals, "Foobar", "Foo");
    STRING_TEST(Failures, ExpectNotEquals, "Foo", "Foobar");
    STRING_TEST(Failures, ExpectNotEquals, "xx", "x");
    STRING_TEST(Failures, ExpectNotEquals, "x", "xx");
    STRING_TEST(Failures, ExpectNotEquals, "x", "");
    STRING_TEST(Failures, ExpectNotEquals, "", "x");
    STRING_TEST(Failures, ExpectNotEquals, "x", "y");
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

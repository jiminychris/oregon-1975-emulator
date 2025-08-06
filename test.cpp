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

print_buffer *AppendExpectingHeader(print_buffer *Buffer, string_reference Expected)
{
    Append(Buffer, "Expecting: ");
    Append(Buffer, Expected);
    Append(Buffer, '=');
    return Buffer;
}

print_buffer *AppendExpectingFooter(print_buffer *Buffer)
{
    Append(Buffer, " . . . ");
    return Buffer;
}

print_buffer *AppendPass(print_buffer *Buffer)
{
    Append(Buffer, "\033[32mPASS\033[0m");
    Newline(Buffer);
    return Buffer;
}

print_buffer *AppendFailHeader(print_buffer *Buffer, string_reference Actual)
{
    Append(Buffer, "\033[31mFAIL");
    Newline(Buffer);
    Append(Buffer, "Actual:    ");
    Append(Buffer, Actual);
    Append(Buffer, '=');
    return Buffer;
}

print_buffer *AppendFailFooter(print_buffer *Buffer)
{
    Append(Buffer, "\033[0m");
    Newline(Buffer);
    return Buffer;
}

#define APPEND_EXPECTING(Buffer, Expected, ExpectedValue) {    \
        AppendExpectingHeader(Buffer, Expected);               \
        Append(Buffer, ExpectedValue);                         \
        AppendExpectingFooter(Buffer);                         \
    }

#define APPEND_FAIL(Buffer, Actual, ActualValue) { \
        AppendFailHeader(Buffer, Actual);          \
        Append(Buffer, ActualValue);               \
        AppendFailFooter(Buffer);                  \
    }

int ExpressionTest(environment *Environment, string_reference ExpressionString, string_reference Expected, s32 ExpectedValue)
{
    lexeme Value;
    PRINT_BUFFER(Buffer, 1024, 0);
    string_reference Actual = ParseTest(Environment, ExpressionString, &Value);
    int Success = Equals(Expected, Actual) && (Value.Type == token_type_BOOLEAN || Value.Type == token_type_INTEGER) && Value.Integer == ExpectedValue;
    if (Success)
    {
#if PRINT_SUCCESSFUL_TESTS
        APPEND_EXPECTING(&Buffer, Expected, ExpectedValue);
        AppendPass(&Buffer);
#endif
    }
    else
    {
        APPEND_EXPECTING(&Buffer, Expected, ExpectedValue);
        APPEND_FAIL(&Buffer, Actual, Value.Integer);
    }
    return !Success;
}

#define EPSILON 0.00001f

int AboutEqual(r32 A, r32 B)
{
    return fabs(A - B) < EPSILON;
}

int AppendTest(string_reference Expected, r64 Value)
{
    PRINT_BUFFER(Buffer, 1024, 0);
    STRING_BUILDER(Builder, 1024);
    Append(&Builder, Value);
    int Success = Equals(StringReference(&Builder), Expected);
    if (Success)
    {
#if PRINT_SUCCESSFUL_TESTS
        APPEND_EXPECTING(&Buffer, Expected, Value);
        AppendPass(&Buffer);
#endif
    }
    else
    {
        APPEND_EXPECTING(&Buffer, Expected, Value);
        Append(&Buffer, "\033[31mFAIL\033[0m");
        Newline(&Buffer);
    }
    return !Success;
}

int ExpressionTest(environment *Environment, string_reference ExpressionString, string_reference Expected, r64 ExpectedValue)
{
    lexeme Value;
    PRINT_BUFFER(Buffer, 1024, 0);
    string_reference Actual = ParseTest(Environment, ExpressionString, &Value);
    int Success = Equals(Expected, Actual) && Value.Type == token_type_REAL && AboutEqual(Value.Real, ExpectedValue);
    if (Success)
    {
#if PRINT_SUCCESSFUL_TESTS
        APPEND_EXPECTING(&Buffer, Expected, ExpectedValue);
        AppendPass(&Buffer);
#endif
    }
    else
    {
        APPEND_EXPECTING(&Buffer, Expected, ExpectedValue);
        APPEND_FAIL(&Buffer, Actual, Value.Real);
    }
    return !Success;
}

int ExpressionTest(environment *Environment, string_reference ExpressionString, string_reference Expected, char ExpectedValue)
{
    lexeme Value;
    PRINT_BUFFER(Buffer, 1024, 0);
    STRING_BUILDER(ExpectedValueBuilder, 1024);
    RenderString({1, &ExpectedValue}, &ExpectedValueBuilder);
    string_reference ExpectedValueString = StringReference(&ExpectedValueBuilder);

    string_reference Actual = ParseTest(Environment, ExpressionString, &Value);
    int Success = Equals(Expected, Actual) && Value.Type == token_type_CHAR && Value.Character == ExpectedValue;
    if (Success)
    {
#if PRINT_SUCCESSFUL_TESTS
        APPEND_EXPECTING(&Buffer, Expected, ExpectedValueString);
        AppendPass(&Buffer);
#endif
    }
    else
    {
        APPEND_EXPECTING(&Buffer, Expected, ExpectedValue);
        APPEND_FAIL(&Buffer, Actual, Value.Character);
    }
    return !Success;
}

int ExpressionTest(environment *Environment, string_reference ExpressionString, string_reference Expected, const char *ExpectedValue)
{
    PRINT_BUFFER(Buffer, 1024, 0);
    STRING_BUILDER(ExpectedValueBuilder, 1024);
    RenderString(StringReference(ExpectedValue), &ExpectedValueBuilder);
    string_reference ExpectedValueString = StringReference(&ExpectedValueBuilder);

    lexeme Value;
    string_reference Actual = ParseTest(Environment, ExpressionString, &Value);
    int Success = Equals(Expected, Actual) && Value.Type == token_type_STRING && Equals(Value.String, ExpectedValue);
    if (Success)
    {
#if PRINT_SUCCESSFUL_TESTS
        APPEND_EXPECTING(&Buffer, Expected, ExpectedValueString);
        AppendPass(&Buffer);
#endif
    }
    else
    {
        APPEND_EXPECTING(&Buffer, Expected, ExpectedValue);
        APPEND_FAIL(&Buffer, Actual, Value.String);
    }
    return !Success;
}

print_buffer *AppendWrappedString(print_buffer *Buffer, const char *String)
{
    Append(Buffer, '"');
    Append(Buffer, String);
    Append(Buffer, '"');
    return Buffer;
}

#define APPEND_WRAPPED_STRING(Buffer, Type, String)   \
    Append(Buffer, Type);                            \
    Append(Buffer, "(\"");                             \
    Append(Buffer, String);                                            \
    Append(Buffer, "\")");                                               \
    return Buffer
print_buffer *AppendWrappedString(print_buffer *Buffer, string_reference String)
{
    APPEND_WRAPPED_STRING(Buffer, "STRING_REFERENCE", String);
}

print_buffer *AppendWrappedString(print_buffer *Buffer, buffer String)
{
    APPEND_WRAPPED_STRING(Buffer, "BUFFER", String);
}

#define ExpectEquals(Failures, A, B) {                              \
        PRINT_BUFFER(Buffer, 1024, 0);                                \
        u8 Success = Equals(A, B);                                  \
        if (Success)                                                \
        {                                                           \
            if(PRINT_SUCCESSFUL_TESTS)                              \
            {                                                       \
                Append(&Buffer, "Expecting: ");                  \
                AppendWrappedString(&Buffer, A);                                \
                Append(&Buffer, " = ");                        \
                AppendWrappedString(&Buffer, B);                                \
                Append(&Buffer, " . . . ");                      \
                AppendPass(&Buffer);                               \
            }                                                       \
        }                                                           \
        else                                                        \
        {                                                           \
            Append(&Buffer, "Expecting: ");                      \
            AppendWrappedString(&Buffer, A);                                    \
            Append(&Buffer, " = ");                            \
            AppendWrappedString(&Buffer, B);                                    \
            Append(&Buffer, " . . . ");                          \
            Append(&Buffer, "\033[31mFAIL\033[0m");                \
            Newline(&Buffer);                                      \
        }                                                           \
        Failures += !Success;                                       \
    }

#define ExpectNotEquals(Failures, A, B) {       \
        PRINT_BUFFER(Buffer, 1024, 0);             \
        u8 Success = !Equals(A, B);              \
        if (Success)                            \
        {                                           \
            if(PRINT_SUCCESSFUL_TESTS)                              \
            {                                                       \
                Append(&Buffer, "Expecting: ");                  \
                AppendWrappedString(&Buffer, A);                                \
                Append(&Buffer, " != ");                        \
                AppendWrappedString(&Buffer, B);                                \
                Append(&Buffer, " . . . ");                      \
                AppendPass(&Buffer);                               \
            }                                                       \
        }                                                           \
        else                                                        \
        {                                                           \
            Append(&Buffer, "Expecting: ");                      \
            AppendWrappedString(&Buffer, A);                                    \
            Append(&Buffer, " != ");                           \
            AppendWrappedString(&Buffer, B);                                    \
            Append(&Buffer, " . . . ");                          \
            Append(&Buffer, "\033[31mFAIL\033[0m");                \
            Newline(&Buffer);                                      \
        }                                                           \
        Failures += !Success;                                       \
    }

#define APPEND_TEST(Failures, Expected, Value) Failures += AppendTest(STRING_REFERENCE(Expected), Value)
#define EXPRESSION_TEST(Failures, Environment, ExpressionString, ExpectedString, ExpectedValue) Failures += ExpressionTest(Environment, STRING_REFERENCE(ExpressionString), STRING_REFERENCE(ExpectedString), ExpectedValue)
#define STRING_TEST(Failures, T, A, B) T(Failures, A, B); T(Failures, STRING_REFERENCE(A), B); T(Failures, STRING_REFERENCE(A), STRING_REFERENCE(B)); T(Failures, BUFFER(A), B);

int Test(environment *Environment)
{
    int Failures = 0;
    APPEND_TEST(Failures, "nan", NAN);
    APPEND_TEST(Failures, "nan", -NAN);
    APPEND_TEST(Failures, "inf", INFINITY);
    APPEND_TEST(Failures, "-inf", -INFINITY);
    APPEND_TEST(Failures, "0", 0.0);
    APPEND_TEST(Failures, "-0", -0.0);
    APPEND_TEST(Failures, "-1", -1.0);
    APPEND_TEST(Failures, "1", 1.0);
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
    EXPRESSION_TEST(Failures, Environment, "'1", "'1", (char)1);
    EXPRESSION_TEST(Failures, Environment, "'1\"FOO\"", "'1\"FOO\"", "\x1""FOO");
    EXPRESSION_TEST(Failures, Environment, "\"FOO\"'1", "\"FOO\"'1", "FOO""\x1");
    EXPRESSION_TEST(Failures, Environment, "0", "0", 0);
    EXPRESSION_TEST(Failures, Environment, "1", "1", 1);
    EXPRESSION_TEST(Failures, Environment, "NOT 1", "(NOT 1)", !1);
    EXPRESSION_TEST(Failures, Environment, "NOT 0", "(NOT 0)", !0);
    EXPRESSION_TEST(Failures, Environment, "NOT -1", "(NOT (-1))", !-1);
    EXPRESSION_TEST(Failures, Environment, "0 OR 0", "(0 OR 0)", 0 || 0);
    EXPRESSION_TEST(Failures, Environment, "0 OR 1", "(0 OR 1)", 0 || 1);
    EXPRESSION_TEST(Failures, Environment, "1 OR 0", "(1 OR 0)", 1 || 0);
    EXPRESSION_TEST(Failures, Environment, "1 OR 1", "(1 OR 1)", 1 || 1);
    EXPRESSION_TEST(Failures, Environment, "0 AND 0", "(0 AND 0)", 0 && 0);
    EXPRESSION_TEST(Failures, Environment, "0 AND 1", "(0 AND 1)", 0 && 1);
    EXPRESSION_TEST(Failures, Environment, "1 AND 0", "(1 AND 0)", 1 && 0);
    EXPRESSION_TEST(Failures, Environment, "1 AND 1", "(1 AND 1)", 1 && 1);
    EXPRESSION_TEST(Failures, Environment, "1 AND 1 OR 1", "((1 AND 1) OR 1)", 1 && 1 || 1);
    EXPRESSION_TEST(Failures, Environment, "1 OR 1 AND 1", "(1 OR (1 AND 1))", 1 || 1 && 1);
    EXPRESSION_TEST(Failures, Environment, "32+2/3*18", "(32+((2/3)*18))", 32+2.0f/3.0f*18);
    EXPRESSION_TEST(Failures, Environment, "1+2*3+4", "((1+(2*3))+4)", 1+2*3+4);
    EXPRESSION_TEST(Failures, Environment, "1*2+3*4", "((1*2)+(3*4))", 1*2+3*4);
    EXPRESSION_TEST(Failures, Environment, "1*2=3*4", "((1*2)=(3*4))", 1*2==3*4);
#if 0
    EXPRESSION_TEST(Failures, Environment, "4^3^2", "(4^(3^2))", Power(4, Power(3, 2)));
#endif
    EXPRESSION_TEST(Failures, Environment, "2-1", "(2-1)", 2-1);
    EXPRESSION_TEST(Failures, Environment, "1-2", "(1-2)", 1-2);
    EXPRESSION_TEST(Failures, Environment, "-1", "(-1)", -1);
    EXPRESSION_TEST(Failures, Environment, "--1", "(-(-1))", -(-1));
    EXPRESSION_TEST(Failures, Environment, "-3^2", "(-(3^2))", -Power(3, 2));
    EXPRESSION_TEST(Failures, Environment, "1+2*3", "(1+(2*3))", 1+2*3);
    EXPRESSION_TEST(Failures, Environment, "(1+2)*3", "((1+2)*3)", (1+2)*3);
    EXPRESSION_TEST(Failures, Environment, "0.5*10", "(0.5*10)", 0.5*10);
    EXPRESSION_TEST(Failures, Environment, "(20/100-4)^2+72", "((((20/100)-4)^2)+72)", Power(20.0f/100-4,2)+72);
    EXPRESSION_TEST(Failures, Environment, "(20/100-4)^2+12", "((((20/100)-4)^2)+12)", Power(20.0f/100-4,2)+12);
    EXPRESSION_TEST(Failures, Environment, "((20/100-4)^2+72)/((20/100-4)^2+12)-1", "((((((20/100)-4)^2)+72)/((((20/100)-4)^2)+12))-1)", (Power(20.0f/100-4,2)+72)/(Power(20.0f/100-4,2)+12)-1);
    EXPRESSION_TEST(Failures, Environment, "0.5*10>((20/100-4)^2+72)/((20/100-4)^2+12)-1", "((0.5*10)>((((((20/100)-4)^2)+72)/((((20/100)-4)^2)+12))-1))", 0.5*10>(Power(20.0f/100-4,2)+72)/(Power(20.0f/100-4,2)+12)-1);
    EXPRESSION_TEST(Failures, Environment, "0.3*10>((20/100-4)^2+72)/((20/100-4)^2+12)-1", "((0.3*10)>((((((20/100)-4)^2)+72)/((((20/100)-4)^2)+12))-1))", 0.3*10>(Power(20.0f/100-4,2)+72)/(Power(20.0f/100-4,2)+12)-1);
    EXPRESSION_TEST(Failures, Environment, "0.2*10>((20/100-4)^2+72)/((20/100-4)^2+12)-1", "((0.2*10)>((((((20/100)-4)^2)+72)/((((20/100)-4)^2)+12))-1))", 0.2*10>(Power(20.0f/100-4,2)+72)/(Power(20.0f/100-4,2)+12)-1);
    return Failures;
}

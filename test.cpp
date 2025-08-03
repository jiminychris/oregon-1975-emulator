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

string_builder *AppendExpectingHeader(string_builder *Builder, string_reference Expected)
{
    Append(Builder, "Expecting: ");
    Append(Builder, Expected);
    Append(Builder, '=');
    return Builder;
}

string_builder *AppendExpectingFooter(string_builder *Builder)
{
    Append(Builder, " . . . ");
    return Builder;
}

string_builder *AppendPass(string_builder *Builder)
{
    Append(Builder, "\033[32mPASS\033[0m");
    Newline(Builder);
    return Builder;
}

string_builder *AppendFailHeader(string_builder *Builder, string_reference Actual)
{
    Append(Builder, "\033[31mFAIL");
    Newline(Builder);
    Append(Builder, "Actual:    ");
    Append(Builder, Actual);
    Append(Builder, '=');
    return Builder;
}

string_builder *AppendFailFooter(string_builder *Builder)
{
    Append(Builder, "\033[0m");
    Newline(Builder);
    return Builder;
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
    STRING_BUILDER(Builder, 1024);
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
    STRING_BUILDER(Builder, 1024);
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
    STRING_BUILDER(Builder, 1024);
    STRING_BUILDER(ExpectedValueBuilder, 1024);
    RenderString({1, &ExpectedValue}, &ExpectedValueBuilder);
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
    STRING_BUILDER(Builder, 1024);
    STRING_BUILDER(ExpectedValueBuilder, 1024);
    RenderString(StringReference(ExpectedValue), &ExpectedValueBuilder);
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

string_builder *AppendWrappedString(string_builder *Builder, const char *String)
{
    Append(Builder, '"');
    Append(Builder, String);
    Append(Builder, '"');
    return Builder;
}

#define APPEND_WRAPPED_STRING(Builder, Type, String)   \
    Append(Builder, Type);                            \
    Append(Builder, "(\"");                             \
    Append(Builder, String);                                            \
    Append(Builder, "\")");                                               \
    return Builder
string_builder *AppendWrappedString(string_builder *Builder, string_reference String)
{
    APPEND_WRAPPED_STRING(Builder, "STRING_REFERENCE", String);
}

string_builder *AppendWrappedString(string_builder *Builder, buffer String)
{
    APPEND_WRAPPED_STRING(Builder, "BUFFER", String);
}

#define ExpectEquals(Failures, A, B) {                              \
        STRING_BUILDER(Builder, 1024);                               \
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
        STRING_BUILDER(Builder, 1024);          \
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

#define EXPRESSION_TEST(Failures, Environment, ExpressionString, ExpectedString, ExpectedValue) Failures += ExpressionTest(Environment, STRING_REFERENCE(ExpressionString), STRING_REFERENCE(ExpectedString), ExpectedValue)
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
    EXPRESSION_TEST(Failures, Environment, "4^3^2", "(4^(3^2))", pow(4, pow(3, 2)));
    EXPRESSION_TEST(Failures, Environment, "2-1", "(2-1)", 2-1);
    EXPRESSION_TEST(Failures, Environment, "1-2", "(1-2)", 1-2);
    EXPRESSION_TEST(Failures, Environment, "-1", "(-1)", -1);
    EXPRESSION_TEST(Failures, Environment, "--1", "(-(-1))", -(-1));
    EXPRESSION_TEST(Failures, Environment, "-3^2", "(-(3^2))", -pow(3, 2));
    EXPRESSION_TEST(Failures, Environment, "1+2*3", "(1+(2*3))", 1+2*3);
    EXPRESSION_TEST(Failures, Environment, "(1+2)*3", "((1+2)*3)", (1+2)*3);
    EXPRESSION_TEST(Failures, Environment, "0.5*10", "(0.5*10)", 0.5*10);
    EXPRESSION_TEST(Failures, Environment, "(20/100-4)^2+72", "((((20/100)-4)^2)+72)", pow(20.0f/100-4,2)+72);
    EXPRESSION_TEST(Failures, Environment, "(20/100-4)^2+12", "((((20/100)-4)^2)+12)", pow(20.0f/100-4,2)+12);
    EXPRESSION_TEST(Failures, Environment, "((20/100-4)^2+72)/((20/100-4)^2+12)-1", "((((((20/100)-4)^2)+72)/((((20/100)-4)^2)+12))-1)", (pow(20.0f/100-4,2)+72)/(pow(20.0f/100-4,2)+12)-1);
    EXPRESSION_TEST(Failures, Environment, "0.5*10>((20/100-4)^2+72)/((20/100-4)^2+12)-1", "((0.5*10)>((((((20/100)-4)^2)+72)/((((20/100)-4)^2)+12))-1))", 0.5*10>(pow(20.0f/100-4,2)+72)/(pow(20.0f/100-4,2)+12)-1);
    EXPRESSION_TEST(Failures, Environment, "0.3*10>((20/100-4)^2+72)/((20/100-4)^2+12)-1", "((0.3*10)>((((((20/100)-4)^2)+72)/((((20/100)-4)^2)+12))-1))", 0.3*10>(pow(20.0f/100-4,2)+72)/(pow(20.0f/100-4,2)+12)-1);
    EXPRESSION_TEST(Failures, Environment, "0.2*10>((20/100-4)^2+72)/((20/100-4)^2+12)-1", "((0.2*10)>((((((20/100)-4)^2)+72)/((((20/100)-4)^2)+12))-1))", 0.2*10>(pow(20.0f/100-4,2)+72)/(pow(20.0f/100-4,2)+12)-1);
    return Failures;
}

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include <time.h>
#include <poll.h>


#define RUN_TESTS 1
#define PRINT_SUCCESSFUL_TESTS 0
#define DEBUG_LEXER 0
#define DEBUG_STATEMENT 0
#define DEBUG_EVALUATE_STATEMENT 0
#define DEBUG_EVAL_IF 0
#define DEBUG_STRING_INPUT 0
#define DEBUG_TIMED_INPUT 0

#define ArrayCount(Array) (sizeof(Array) / sizeof(Array[0]))
#define Assert(Expr) {if(!(Expr)) int __AssertInt = *((volatile int *)0);}

s32 IsPrintable(char Char)
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

struct string_reference
{
    u32 Length;
    char *Memory;
};

#define STRING_REFERENCE(StringLiteral) (string_reference){sizeof (StringLiteral) - 1, (char*)(StringLiteral)}
#define BUFFER(StringLiteral) (buffer){sizeof (StringLiteral) - 1, (char*)(StringLiteral), (char*)(StringLiteral) + sizeof (StringLiteral) - 1, (char*)(StringLiteral) + sizeof (StringLiteral) - 1}

enum operator_precedence
{
    operator_precedence_None,
    operator_precedence_Or,
    operator_precedence_And,
    operator_precedence_Not,
    operator_precedence_Comparison,
    operator_precedence_AddSub,
    operator_precedence_MulDiv,
    operator_precedence_Unary,
    operator_precedence_Exponent,
};

enum precedence_direction
{
    precedence_direction_RightToLeft = -1,
    precedence_direction_LeftToRight = 1,
};

precedence_direction PrecedenceDirections[] =
{
    precedence_direction_LeftToRight,
    precedence_direction_LeftToRight,
    precedence_direction_LeftToRight,
    precedence_direction_LeftToRight,
    precedence_direction_LeftToRight,
    precedence_direction_LeftToRight,
    precedence_direction_LeftToRight,
    precedence_direction_LeftToRight,
    precedence_direction_RightToLeft,
};

#define CREATE_ENUM(Prefix, Name, Precedence) Prefix##_##Name,
#define CREATE_STRING(Prefix, Name, Precedence) STRING_REFERENCE(#Name),
#define CREATE_PRECEDENCE(Prefix, Name, Precedence) operator_precedence_##Precedence,

#define TOKEN_TYPES(x)    \
    x(token_type, UNKNOWN, None)                     \
    x(token_type, EOF, None)                              \
    x(token_type, NEWLINE, None)                               \
    x(token_type, SEMICOLON, None)                                  \
    x(token_type, COMMA, None)                                           \
    x(token_type, NEGATE, Unary)                                               \
    x(token_type, MINUS, AddSub)                                               \
    x(token_type, PLUS, AddSub)                                                \
    x(token_type, STAR, MulDiv)                                                \
    x(token_type, SLASH, MulDiv)                                               \
    x(token_type, CARET, Exponent)                                               \
    x(token_type, POUND, None)                                               \
    x(token_type, OPAREN, None)                                              \
    x(token_type, CPAREN, None)                                              \
    x(token_type, OBRACKET, None)                                            \
    x(token_type, CBRACKET, None)                                            \
    x(token_type, EQ, Comparison)                                                  \
    x(token_type, LT, Comparison)                                                  \
    x(token_type, LTE, Comparison)                                                 \
    x(token_type, GT, Comparison)                                                  \
    x(token_type, GTE, Comparison)                                                 \
    x(token_type, OF, None)                                                  \
    x(token_type, OR, Or)                                                  \
    x(token_type, AND, And)                                                 \
    x(token_type, NOT, Not)                                                 \
    x(token_type, INTEGER, None)                                             \
    x(token_type, BOOLEAN, None)                                             \
    x(token_type, REAL, None)                                                \
    x(token_type, CHAR, None)                                                \
    x(token_type, ID, None)                                                  \
    x(token_type, STRING, None)                                              \
    x(token_type, NOOP, None)                                                \
    x(token_type, REM, None)                                                 \
    x(token_type, PRINT, None)                                               \
    x(token_type, LIN, None)                                                 \
    x(token_type, TAB, None)                                                 \
    x(token_type, INT, None)                                                 \
    x(token_type, RND, None)                                                 \
    x(token_type, TIM, None)                                                 \
    x(token_type, DIM, None)                                                 \
    x(token_type, INPUT, None)                                               \
    x(token_type, IF, None)                                                  \
    x(token_type, THEN, None)                                                \
    x(token_type, END, None)                                                 \
    x(token_type, LET, None)                                                 \
    x(token_type, DATA, None)                                                \
    x(token_type, ENTER, None)                                               \
    x(token_type, GOSUB, None)                                               \
    x(token_type, GOTO, None)                                                \
    x(token_type, READ, None)                                                \
    x(token_type, RESTORE, None)                                             \
    x(token_type, RETURN, None)                                              \
    x(token_type, CONS, None)                                                \
    x(token_type, STOP, None)

enum token_type
{
    TOKEN_TYPES(CREATE_ENUM)
};

const string_reference TokenTypeNames[] =
{
    TOKEN_TYPES(CREATE_STRING)
};

const operator_precedence TokenTypePrecedences[] =
{
    TOKEN_TYPES(CREATE_PRECEDENCE)
};

#define BASIC_COMMANDS(x) \
    x(basic_command, NOOP, None)     \
    x(basic_command, REM, None)           \
    x(basic_command, PRINT, None)              \
    x(basic_command, DIM, None)                     \
    x(basic_command, INPUT, None)                        \
    x(basic_command, IF, None)                                \
    x(basic_command, END, None)                                    \
    x(basic_command, LET, None)                                         \
    x(basic_command, DATA, None)                                             \
    x(basic_command, ENTER, None)                                            \
    x(basic_command, GOSUB, None)                                            \
    x(basic_command, GOTO, None)                                             \
    x(basic_command, READ, None)                                             \
    x(basic_command, RESTORE, None)                                          \
    x(basic_command, RETURN, None)                                           \
    x(basic_command, STOP, None)

enum basic_command
{
    BASIC_COMMANDS(CREATE_ENUM)
};

const string_reference BasicCommandNames[] =
{
    BASIC_COMMANDS(CREATE_STRING)
};

struct timed_input
{
    s32 TimeoutMilliseconds;
    s32 Polled;
    s32 TimedOut;
};

struct lexeme
{
    token_type Type;
    u32 LineNumber;
    u32 BasicLineNumber;
    s32 Integer;
    char Character;
    r32 Real;
    u8 IsString;
    string_reference String;
    lexeme *Left;
    lexeme *Right;
};

struct lexeme_stack
{
    size_t Count;
    lexeme *Stack[64];
};

struct basic_line
{
    lexeme *Lexeme;
};

struct memory_arena
{
    void *Memory;
    size_t Allocated;
    size_t Size;
};

struct string_builder
{
    memory_arena Arena;
    char Memory[1024];
};

struct parser
{
    size_t Size;
    size_t Position;
    u8 *Contents;
    basic_line Lines[65536];
    lexeme Lexemes[65536];
    lexeme_stack DataStatements;
    size_t NaturalLexemeCount;
    size_t LexemeCount;
    size_t LexemePosition;
    memory_arena StringArena;
    u32 SourceLineNumber;
    u32 BasicLineNumber;
};

struct environment
{
    s32 IsInteractive;
    lexeme *Goto;
    string_reference VariableNames[64];
    lexeme VariableValues[64];
    size_t VariableCount;
    basic_line Stack[64];
    s32 DataEntries[64];
    size_t DataPosition;
    size_t DataCount;
    size_t StackLength;
    parser Parser;
};

struct temporary_memory
{
    size_t AllocatedBefore;
};

struct program
{
    u64 DataOffset;
    u64 FooterOffset;
    u64 ExecutableFileSize;
    s32 CreateExecutable;
    char *SourcePath;
    char *NewExecutablePath;
    char *ExecutablePath;
    char *ExecutableName;
};

struct buffer
{
    size_t Size;
    char *Contents;
    char *At;
    char *End;
};

temporary_memory BeginTemporaryMemory(memory_arena *Arena)
{
    temporary_memory Result;
    Result.AllocatedBefore = Arena->Allocated;
    return Result;
}

void EndTemporaryMemory(memory_arena *Arena, temporary_memory TemporaryMemory)
{
    Arena->Allocated = TemporaryMemory.AllocatedBefore;
}

string_reference BeginString(memory_arena *Arena)
{
    string_reference Result = {};
    Result.Memory = (char*)Arena->Memory + Arena->Allocated;
    return Result;
}

size_t AllocateString(memory_arena *Arena, size_t Size, char **Dest)
{
    size_t AllocatedBefore = Arena->Allocated;
    size_t AllocatedTarget = AllocatedBefore + Size;
    size_t AllocatedActual = Min(AllocatedTarget, Arena->Size);
    Arena->Allocated = AllocatedActual;
    *Dest = (char*)Arena->Memory + AllocatedBefore;
    return AllocatedActual - AllocatedBefore;
}

string_reference StringReference(buffer Buffer)
{
    string_reference Result;
    Result.Memory = Buffer.Contents;
    Result.Length = Buffer.At - Buffer.Contents;
    return Result;
}

size_t Append(memory_arena *Arena, char Char, u32 Count = 1)
{
    size_t Result = 0;
    while (Count-- && Arena->Allocated < Arena->Size)
    {
        Result++;
        ((char*)Arena->Memory)[Arena->Allocated++] = Char;
    }
    return Result;
}

size_t Append(memory_arena *Arena, signed char Char, u32 Count = 1)
{
    return Append(Arena, (char)Char, Count);
}

size_t Append(memory_arena *Arena, unsigned char Char, u32 Count = 1)
{
    return Append(Arena, (char)Char, Count);
}

size_t Newline(memory_arena *Arena, u32 Count = 1)
{
    return Append(Arena, '\n', Count);
}

size_t Space(memory_arena *Arena, u32 Count = 1)
{
    return Append(Arena, ' ', Count);
}

size_t Tab(memory_arena *Arena, u32 Count = 1)
{
    return Append(Arena, '\t', Count);
}

size_t Append(memory_arena *Arena, const char *String)
{
    size_t Result = 0;
    char *At = (char *)String;
    while (Arena->Allocated < Arena->Size && *At)
    {
        ((char*)Arena->Memory)[Arena->Allocated++] = *At++;
        Result++;
    }
    return Result;
}

size_t Append(memory_arena *Arena, string_reference String)
{
    size_t Result = 0;
    char *At = String.Memory;
    s32 Remaining = String.Length;
    while (Arena->Allocated < Arena->Size && Remaining--)
    {
        ((char*)Arena->Memory)[Arena->Allocated++] = *At++;
        Result++;
    }
    return Result;
}

size_t Append(memory_arena *Arena, buffer Buffer)
{
    return Append(Arena, StringReference(Buffer));
}

size_t Append(memory_arena *Arena, r32 Real)
{
    char RealString[256];
    snprintf(RealString, sizeof RealString, "%g", Real);
    return Append(Arena, RealString);
}

size_t Append(memory_arena *Arena, r64 Real)
{
    char RealString[256];
    snprintf(RealString, sizeof RealString, "%g", Real);
    return Append(Arena, RealString);
}

size_t Append(memory_arena *Arena, s32 Integer)
{
    size_t Result = 0;
    if (Integer == 0)
    {
        Result += Append(Arena, '0');
    }
    else
    {
        if (Integer < 0)
        {
            Result += Append(Arena, '-');
            Integer *= -1;
        }
        string_reference Digits = BeginString(Arena);
        while (Integer)
        {
            Digits.Length += Append(Arena, (char)('0' + (Integer % 10)));
            Integer /= 10;
        }
        Result += Digits.Length;
        char *Head = Digits.Memory;
        char *Tail = Head + Digits.Length - 1;
        while (Head < Tail)
        {
            char Temp = *Tail;
            *Tail-- = *Head;
            *Head++ = Temp;
        }
    }
    return Result;
}

s32 StringLength(const char *String)
{
    s32 Result = 0;
    while (*String++)
    {
        Result += 1;
    }
    return Result;
}

string_reference StringReference(const char *String)
{
    string_reference Result;
    Result.Length = StringLength(String);
    Result.Memory = (char *)String;
    return Result;
}

string_reference StringReference(string_builder *Builder)
{
    string_reference Result;
    Result.Memory = Builder->Memory;
    Result.Length = Builder->Arena.Allocated;
    return Result;
}

void Reset(string_builder *Builder)
{
    Builder->Arena.Size = sizeof Builder->Memory;
    Builder->Arena.Allocated = 0;
    Builder->Arena.Memory = Builder->Memory;
}

string_builder *Append(string_builder *Builder, const char *String)
{
    Append(&Builder->Arena, String);
    return Builder;
}

string_builder *Append(string_builder *Builder, string_reference String)
{
    Append(&Builder->Arena, String);
    return Builder;
}

string_builder *Append(string_builder *Builder, buffer Buffer)
{
    Append(&Builder->Arena, Buffer);
    return Builder;
}

string_builder *Append(string_builder *Builder, char Char, u32 Count = 1)
{
    Append(&Builder->Arena, Char, Count);
    return Builder;
}

string_builder *Append(string_builder *Builder, unsigned char Char, u32 Count = 1)
{
    Append(&Builder->Arena, (char)Char, Count);
    return Builder;
}

string_builder *Append(string_builder *Builder, signed char Char, u32 Count = 1)
{
    Append(&Builder->Arena, (char)Char, Count);
    return Builder;
}

string_builder *Newline(string_builder *Builder, u32 Count = 1)
{
    Newline(&Builder->Arena, Count);
    return Builder;
}

string_builder *Space(string_builder *Builder, u32 Count = 1)
{
    Space(&Builder->Arena, Count);
    return Builder;
}

string_builder *Tab(string_builder *Builder, u32 Count = 1)
{
    Tab(&Builder->Arena, Count);
    return Builder;
}

string_builder *Append(string_builder *Builder, s32 Integer)
{
    Append(&Builder->Arena, Integer);
    return Builder;
}

string_builder *Append(string_builder *Builder, r32 Real)
{
    Append(&Builder->Arena, Real);
    return Builder;
}

string_builder *Append(string_builder *Builder, r64 Real)
{
    Append(&Builder->Arena, Real);
    return Builder;
}

ssize_t Print(string_reference String)
{
    return Write(STDOUT_FILENO, String.Memory, String.Length);
}

ssize_t Print(string_builder *Builder)
{
    return Write(STDOUT_FILENO, Builder->Memory, Builder->Arena.Allocated);
}

void DebugLexer(parser *Parser)
{
    string_builder Builder;
    for (s32 LexIndex = 0; LexIndex < Parser->LexemeCount; ++LexIndex)
    {
        Reset(&Builder);
        lexeme *Lexeme = Parser->Lexemes + LexIndex;
        Append(&Builder, TokenTypeNames[Lexeme->Type]);
        Space(&Builder);
        switch (Lexeme->Type)
        {
            case token_type_REM:
            {
                Append(&Builder, Lexeme->String);
            } break;
            case token_type_INTEGER:
            {
                Append(&Builder, Lexeme->Integer);
            } break;
            case token_type_REAL:
            {
                Append(&Builder, Lexeme->Real);
            } break;
            case token_type_CHAR:
            {
                Append(&Builder, (s32)Lexeme->Character);
            } break;
            case token_type_STRING:
            {
                Append(&Builder, Lexeme->String);
            } break;
            case token_type_ID:
            {
                Append(&Builder, Lexeme->String);
                if (Lexeme->IsString)
                {
                    Append(&Builder, '$');
                }
            } break;
            case token_type_UNKNOWN:
            {
                if (Lexeme->String.Memory)
                {
                    Append(&Builder, Lexeme->String);
                }
                else
                {
                    Append(&Builder, Lexeme->Character);
                }
            } break;
            default:
            {
            } break;
        }
        Newline(&Builder);
        Print(&Builder);
    }
}

u8 IsNumber(token_type Type)
{
    return Type == token_type_INTEGER || Type == token_type_REAL;
}

lexeme *Expression(parser *Parser);
lexeme *EvaluateExpression(environment *Environment, lexeme *Expr, lexeme *Value);

lexeme LexemeEOF = {token_type_EOF};
lexeme LexemeUnknown = {token_type_UNKNOWN};

lexeme *PeekLex(parser *Parser, u32 Lookahead = 0)
{
    u32 Position = Parser->LexemePosition + Lookahead;
    lexeme *Lexeme = &LexemeEOF;
    if (Position < Parser->NaturalLexemeCount)
    {
        Lexeme = Parser->Lexemes + Position;
    }
    return Lexeme;
}

lexeme *GetLex(parser *Parser)
{
    lexeme *Lexeme = &LexemeEOF;
    if (Parser->LexemePosition < Parser->NaturalLexemeCount)
    {
        Lexeme = Parser->Lexemes + Parser->LexemePosition++;
    }
    Lexeme->BasicLineNumber = Parser->BasicLineNumber;
    return Lexeme;
}

void UngetLex(parser *Parser)
{
    if (Parser->LexemePosition > 0)
    {
        Parser->LexemePosition--;
    }
}

lexeme *PreviousLexeme(parser *Parser)
{
    lexeme *Lexeme = &LexemeUnknown;
    if (Parser->LexemeCount)
    {
        Lexeme = Parser->Lexemes + Parser->LexemeCount - 1;
    }
    return Lexeme;
}

void UngetChar(parser *Parser)
{
    if (Parser->Position > 0)
    {
        Parser->Position--;
    }
}

signed char GetChar(environment *Environment, timed_input *TimedInput = 0)
{
    pollfd FileDescriptor;
    FileDescriptor.fd = STDIN_FILENO;
    FileDescriptor.events = POLLIN;
    timed_input Default = {};
    Default.TimeoutMilliseconds = -1;
    if (!TimedInput)
    {
        TimedInput = &Default;
    }
    TimedInput->TimedOut = 0;

    signed char Char = ReadCharacter();
    TimedInput->Polled = (Char == EOF);
    if (TimedInput->Polled)
    {
        Environment->IsInteractive = 1;
        TimedInput->TimedOut = Poll(&FileDescriptor, 1, TimedInput->TimeoutMilliseconds) <= 0;
        if (!TimedInput->TimedOut)
        {
            Char = ReadCharacter();
        }
    }
    else if (!Environment->IsInteractive)
    {
        WriteCharacter(Char);
    }
#if DEBUG_TIMED_INPUT
    string_builder Builder;
    Reset(&Builder);
    Append(&Builder, "Char: ");
    Append(&Builder, Char);
    Append(&Builder, '(');
    Append(&Builder, (s32)Char);
    Append(&Builder, ")");
    Newline(&Builder);
    Print(&Builder);
#endif
    return Char;
}

signed char GetChar(parser *Parser)
{
    signed char Result = -1;
    if (Parser->Position < Parser->Size)
    {
        Result = Parser->Contents[Parser->Position++];
    }
    return Result;
}

signed char PeekChar(parser *Parser)
{
    signed char Result = -1;
    if (Parser->Position < Parser->Size)
    {
        Result = Parser->Contents[Parser->Position];
    }
    return Result;
}

u32 IsEOF(parser *Parser)
{
    return PeekChar(Parser) < 0;
}

u32 IsUpperOrDigit(s8 Char)
{
    return isdigit(Char) || isupper(Char);
}

u32 IsWhitespace(s8 Char)
{
    return Char == ' ' || Char == '\n' || Char == '\r' || Char == '\t';
}

u32 IsIntralineWhitespace(s8 Char)
{
    return Char == ' ' || Char == '\t';
}

size_t SkipIntralineWhitespace(parser *Parser)
{
    size_t Start = Parser->Position;
    while (IsIntralineWhitespace(PeekChar(Parser)))
    {
        Parser->Position++;
    }
    return Parser->Position - Start;
}

void SkipToEndOfLine(parser *Parser)
{
    while (!IsEOF(Parser) && GetChar(Parser) != '\n');
}

void Warn(const char *Format, ...)
{
    va_list args;
    va_start(args, Format);

    fprintf(stderr, "WARNING: ");
    vfprintf(stderr, Format, args);
    fprintf(stderr, "\n");

    va_end(args);
}

void Warn(parser *Parser, const char *Format, ...)
{
    va_list args;
    va_start(args, Format);

    fprintf(stderr, "WARNING: Line %u: ", Parser->SourceLineNumber);
    vfprintf(stderr, Format, args);
    fprintf(stderr, "\n");

    va_end(args);
}

void Panic(const char *Format, ...)
{
    va_list args;
    va_start(args, Format);

    fprintf(stderr, "ERROR: ");
    vfprintf(stderr, Format, args);
    fprintf(stderr, "\n");

    va_end(args);
    exit(1);
}

void Panic(parser *Parser, const char *Format, ...)
{
    va_list args;
    va_start(args, Format);

    fprintf(stderr, "ERROR: Line %u: ", Parser->SourceLineNumber);
    vfprintf(stderr, Format, args);
    fprintf(stderr, "\n");

    va_end(args);
    exit(1);
}

void Panic(lexeme *Lexeme, const char *Format, ...)
{
    va_list args;
    va_start(args, Format);

    fprintf(stderr, "ERROR: Line %u: ", Lexeme->LineNumber);
    vfprintf(stderr, Format, args);
    fprintf(stderr, "\n");

    va_end(args);
    exit(1);
}

void PrintUsageAndExit(program *Program)
{
    string_builder Builder;
    Newline(Append(Append(Append(&Builder, "Usage: "), Program->ExecutableName), " <src>"));
    Newline(Append(Append(Space(&Builder, 7), Program->ExecutableName), " <src> -x [-o <out>]"));
    Newline(Append(Append(Space(&Builder, 7), Program->ExecutableName), " [--help]"));
    Newline(Append(&Builder, "Execute a BASIC program."));
    Newline(&Builder);
    Newline(Append(Tab(&Builder), "-h, --help  display this help and exit"));
    Newline(Append(Tab(&Builder), "-o <out>    store the executable BASIC program at <out> (used with -x)"));
    Newline(Append(Tab(&Builder), "-x          create an executable version of the given BASIC program <src>"));
    Print(&Builder);
    exit(1);
}

buffer NewBuffer(char *Contents, size_t Size)
{
    buffer Result;
    Result.Size = Size;
    Result.At = Result.Contents = Contents;
    Result.End = Contents + Size;
    return Result;
}

void Reset(buffer *Buffer)
{
    Buffer->At = Buffer->Contents;
}

s32 Full(buffer Buffer)
{
    return Buffer.End <= Buffer.At;
}

s32 Equals(string_reference A, string_reference B)
{
    s32 BufferCountdown = A.Length;
    char *AAt = A.Memory;
    char *BAt = B.Memory;
    u8 Result = A.Length == B.Length;
    while (Result && BufferCountdown--)
    {
        Result = (*AAt++ == *BAt++);
    }
    return Result;
}

lexeme *FindVariable(environment *Environment, lexeme *Lexeme)
{
    lexeme *Result = 0;
    for (s32 VariableIndex = 0; VariableIndex < Environment->VariableCount; ++VariableIndex)
    {
        if (Equals(Environment->VariableNames[VariableIndex], Lexeme->String))
        {
            Result = Environment->VariableValues + VariableIndex;
        }
    }
    return Result;
}

lexeme *LookupVariable(environment *Environment, lexeme *Lexeme)
{
    lexeme *Result = FindVariable(Environment, Lexeme);
    if (!Result)
    {
        Panic(Lexeme, "Cannot find variable %.*s", Lexeme->String.Length, Lexeme->String.Memory);
    }
    return Result;
}

lexeme *DeclareVariable(environment *Environment, lexeme *Lexeme)
{
    lexeme *Variable = FindVariable(Environment, Lexeme);
    if (Variable)
    {
        Panic(Lexeme, "Double declaration of %.*s", Lexeme->String.Length, Lexeme->String.Memory);
    }
    if (ArrayCount(Environment->VariableValues) <= Environment->VariableCount)
    {
        Panic(Lexeme, "Unable to allocate memory for a new variable.");
    }
    Environment->VariableNames[Environment->VariableCount] = Lexeme->String;
    lexeme *Result = Environment->VariableValues + Environment->VariableCount;
    Environment->VariableCount += 1;
    Result->Type = Lexeme->IsString ? token_type_STRING : token_type_INTEGER;
    return Result;
}

lexeme *LookupOrDeclareVariable(environment *Environment, lexeme *Lexeme)
{
    lexeme *Variable = FindVariable(Environment, Lexeme);
    if (!Variable)
    {
        Variable = DeclareVariable(Environment, Lexeme);
    }
    return Variable;
}

lexeme *PushLexeme(parser *Parser, token_type Type)
{
    lexeme *Lexeme = Parser->Lexemes + Parser->LexemeCount++;
    Lexeme->Type = Type;
    Lexeme->LineNumber = Parser->SourceLineNumber;
    Lexeme->Left = Lexeme->Right = 0;
    return Lexeme;
}

lexeme *LexNumber(parser *Parser)
{
    s32 Integer = 0;
    lexeme *Lexeme;
    while (isdigit(PeekChar(Parser)))
    {
        Integer = 10 * Integer + GetChar(Parser) - '0';
    }
    if (PeekChar(Parser) == '.')
    {
        GetChar(Parser);
        r32 Real = Integer;
        r32 Multiplier = 0.1f;
        while (isdigit(PeekChar(Parser)))
        {
            Real += Multiplier * (GetChar(Parser) - '0');
            Multiplier *= 0.1f;
        }
        Lexeme = PushLexeme(Parser, token_type_REAL);
        Lexeme->Real = Real;
    }
    else
    {
        Lexeme = PushLexeme(Parser, token_type_INTEGER);
        Lexeme->Integer = Integer;
    }

    return Lexeme;
}

lexeme *LexCharacter(parser *Parser)
{
    memory_arena *Arena = &Parser->StringArena;
    lexeme *Lexeme = PreviousLexeme(Parser);
    char Char = 0;
    while (isdigit(PeekChar(Parser)))
    {
        Char = 10 * Char + GetChar(Parser) - '0';
    }

    if (Lexeme && Lexeme->Type == token_type_STRING)
    {
        Lexeme->String.Length += Append(Arena, Char);
    }
    else if (Lexeme && Lexeme->Type == token_type_CHAR)
    {
        Lexeme->Type = token_type_STRING;
        Lexeme->String = BeginString(Arena);
        Lexeme->String.Length += Append(Arena, Lexeme->Character);
        Lexeme->String.Length += Append(Arena, Char);
    }
    else
    {
        Lexeme = PushLexeme(Parser, token_type_CHAR);
        Lexeme->Character = Char;
    }
    return Lexeme;
}

u8 Equals(string_reference Buffer, const char *String)
{
    u8 Result = 1;
    s32 BufferCountdown = Buffer.Length;
    char *BufferAt = Buffer.Memory;
    const char *StringAt = String;
    while (Result && BufferCountdown-- && *StringAt)
    {
        Result = (*BufferAt++ == *StringAt++);
    }
    Result = Result && (BufferCountdown < 0 && *StringAt == 0);
    return Result;
}

s32 Equals(buffer Buffer, const char *String)
{
    return Equals(StringReference(Buffer), String);
}

u8 Equals(const char *A, const char *B)
{
    u8 Result = 1;
    const char *AAt= A;
    const char *BAt = B;
    while (Result && *A && *B)
    {
        Result = (*A++ == *B++);
    }
    Result = Result && (*A == *B);
    return Result;
}

lexeme *LexString(parser *Parser)
{
    memory_arena *Arena = &Parser->StringArena;
    lexeme *Lexeme = PreviousLexeme(Parser);
    if (Lexeme->Type == token_type_STRING)
    {
    }
    else if (Lexeme->Type == token_type_CHAR)
    {
        Lexeme->Type = token_type_STRING;
        Lexeme->String = BeginString(Arena);
        Lexeme->String.Length += Append(Arena, Lexeme->Character);
    }
    else
    {
        Lexeme = PushLexeme(Parser, token_type_STRING);
        Lexeme->String = BeginString(Arena);
    }
    // TODO: Escaped quotes and separators.
    while (!IsEOF(Parser) && PeekChar(Parser) != '"')
    {
        Lexeme->String.Length += Append(Arena, GetChar(Parser));
    }
    GetChar(Parser);
    return Lexeme;
}

lexeme *LexCommand(parser *Parser, string_reference String, token_type Type, const char *Name)
{
    lexeme *Lexeme = 0;
    if (Equals(String, Name))
    {
        Lexeme = PushLexeme(Parser, Type);
    }
    return Lexeme;
}

#define LEX_COMMAND(Parser, Buffer, Name) LexCommand(Parser, Buffer, token_type_##Name, #Name)

lexeme *LexAlpha(parser *Parser)
{
    memory_arena *Arena = &Parser->StringArena;
    string_builder Builder;
    Reset(&Builder);
    signed char Char = PeekChar(Parser);

    while (isalpha(Char) || isdigit(Char) || Char == '_')
    {
        Append(&Builder, GetChar(Parser));
        Char = PeekChar(Parser);
    }
    string_reference String = StringReference(&Builder);
    lexeme *Lexeme = 0;
    if      ((Lexeme = LEX_COMMAND(Parser, String, REM)))
    {
        Lexeme->String = BeginString(Arena);
        while (!IsEOF(Parser) && PeekChar(Parser) != '\n')
        {
            Lexeme->String.Length += Append(Arena, GetChar(Parser));
        }
    }
    else if ((Lexeme = LEX_COMMAND(Parser, String, PRINT))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, String, LIN))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, String, TAB))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, String, INT))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, String, RND))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, String, TIM))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, String, DIM))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, String, INPUT))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, String, IF))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, String, THEN))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, String, END))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, String, LET))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, String, DATA))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, String, OF))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, String, OR))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, String, AND))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, String, NOT))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, String, ENTER))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, String, GOSUB))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, String, GOTO))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, String, READ))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, String, RESTORE))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, String, RETURN))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, String, STOP))) {}
    else
    {
        Lexeme = PushLexeme(Parser, token_type_ID);
        Lexeme->String = BeginString(Arena);
        for (s32 i = 0; i < String.Length; ++i)
        {
            Lexeme->String.Length += Append(Arena, String.Memory[i]);
        }
        if (PeekChar(Parser) == '$')
        {
            Lexeme->String.Length += Append(Arena, GetChar(Parser));
            Lexeme->IsString = 1;
        }
    }
    return Lexeme;
}

void Lex(parser *Parser)
{
    signed char Char;
    SkipIntralineWhitespace(Parser);
    while (!IsEOF(Parser))
    {
        Char = GetChar(Parser);
        switch (Char)
        {
            case '\n':
            {
                PushLexeme(Parser, token_type_NEWLINE);
                Parser->SourceLineNumber++;
            } break;
            case '(':
            {
                PushLexeme(Parser, token_type_OPAREN);
            } break;
            case ')':
            {
                PushLexeme(Parser, token_type_CPAREN);
            } break;
            case '[':
            {
                PushLexeme(Parser, token_type_OBRACKET);
            } break;
            case ']':
            {
                PushLexeme(Parser, token_type_CBRACKET);
            } break;
            case '=':
            {
                PushLexeme(Parser, token_type_EQ);
            } break;
            case ';':
            {
                PushLexeme(Parser, token_type_SEMICOLON);
            } break;
            case '\'':
            {
                LexCharacter(Parser);
            } break;
            case ',':
            {
                PushLexeme(Parser, token_type_COMMA);
            } break;
            case '-':
            {
                PushLexeme(Parser, token_type_MINUS);
            } break;
            case '+':
            {
                PushLexeme(Parser, token_type_PLUS);
            } break;
            case '*':
            {
                PushLexeme(Parser, token_type_STAR);
            } break;
            case '/':
            {
                PushLexeme(Parser, token_type_SLASH);
            } break;
            case '^':
            {
                PushLexeme(Parser, token_type_CARET);
            } break;
            case '#':
            {
                PushLexeme(Parser, token_type_POUND);
            } break;
            case '<':
            {
                if (PeekChar(Parser) == '=')
                {
                    GetChar(Parser);
                    PushLexeme(Parser, token_type_LTE);
                }
                else
                {
                    PushLexeme(Parser, token_type_LT);
                }
            } break;
            case '>':
            {
                if (PeekChar(Parser) == '=')
                {
                    GetChar(Parser);
                    PushLexeme(Parser, token_type_GTE);
                }
                else
                {
                    PushLexeme(Parser, token_type_GT);
                }
            } break;
            case '"':
            {
                LexString(Parser);
            } break;
            default:
            {
                if (isdigit(Char) || Char == '.')
                {
                    UngetChar(Parser);
                    LexNumber(Parser);
                }
                else if (isalpha(Char))
                {
                    UngetChar(Parser);
                    LexAlpha(Parser);
                }
                else
                {
                    lexeme *Lexeme = PushLexeme(Parser, token_type_UNKNOWN);
                    Lexeme->Character = Char;
                }
            }
        }
        SkipIntralineWhitespace(Parser);
    }
    if (PreviousLexeme(Parser)->Type != token_type_NEWLINE)
    {
        PushLexeme(Parser, token_type_NEWLINE);
    }
    Parser->NaturalLexemeCount = Parser->LexemeCount;
#if DEBUG_LEXER
    DebugLexer(Parser);
#endif
}

lexeme *Match(parser *Parser, token_type Type)
{
    lexeme *Lexeme = GetLex(Parser);
    if (Lexeme->Type != Type)
    {
        Panic(Lexeme, "Expected %s, not %s", TokenTypeNames[Type].Memory, TokenTypeNames[Lexeme->Type].Memory);
    }
    return Lexeme;
}

lexeme *Int(parser *Parser)
{
    lexeme *Int = Match(Parser, token_type_INT);
    Match(Parser, token_type_OPAREN);
    Int->Left = Expression(Parser);
    Match(Parser, token_type_CPAREN);
    return Int;
}

lexeme *Rnd(parser *Parser)
{
    lexeme *Rnd = Match(Parser, token_type_RND);
    Match(Parser, token_type_OPAREN);
    Rnd->Left = Expression(Parser);
    Match(Parser, token_type_CPAREN);
    return Rnd;
}

lexeme *Tim(parser *Parser)
{
    lexeme *Tim = Match(Parser, token_type_TIM);
    Match(Parser, token_type_OPAREN);
    Tim->Left = Expression(Parser);
    Match(Parser, token_type_CPAREN);
    return Tim;
}

lexeme *Lin(parser *Parser)
{
    lexeme *Lin = Match(Parser, token_type_LIN);
    Match(Parser, token_type_OPAREN);
    Lin->Left = Expression(Parser);
    Match(Parser, token_type_CPAREN);
    return Lin;
}

lexeme *Tab(parser *Parser)
{
    lexeme *Tab = Match(Parser, token_type_TAB);
    Match(Parser, token_type_OPAREN);
    Tab->Left = Expression(Parser);
    Match(Parser, token_type_CPAREN);
    return Tab;
}

lexeme *Push(lexeme_stack *Stack, lexeme *Lexeme)
{
    if (ArrayCount(Stack->Stack) <= Stack->Count)
    {
        Panic(Lexeme, "Stack overflow");
    }
    Stack->Stack[Stack->Count++] = Lexeme;
    return Lexeme;
}

lexeme *Pop(lexeme_stack *Stack, lexeme **Lexeme)
{
    if (Stack->Count < 1)
    {
        Panic(*Lexeme, "Stack underflow");
    }
    *Lexeme = Stack->Stack[--Stack->Count];
    return *Lexeme;
}

u8 IsPotentialUnaryOperator(lexeme *Lexeme)
{
    return (
        Lexeme->Type == token_type_MINUS ||
        Lexeme->Type == token_type_NOT
        );
}

u8 IsUnaryOperator(lexeme *Lexeme)
{
    return (
        Lexeme->Type == token_type_NEGATE ||
        Lexeme->Type == token_type_NOT
        );
}

u8 IsBinaryOperator(lexeme *Lexeme)
{
    return (
        Lexeme->Type == token_type_PLUS ||
        Lexeme->Type == token_type_MINUS ||
        Lexeme->Type == token_type_STAR ||
        Lexeme->Type == token_type_SLASH ||
        Lexeme->Type == token_type_CARET ||
        Lexeme->Type == token_type_LT ||
        Lexeme->Type == token_type_LTE ||
        Lexeme->Type == token_type_GT ||
        Lexeme->Type == token_type_GTE ||
        Lexeme->Type == token_type_OR ||
        Lexeme->Type == token_type_AND ||
        Lexeme->Type == token_type_EQ
        );
}

lexeme *SimpleExpression(parser *Parser, lexeme_stack *OperatorStack)
{
    lexeme Sentinel;
    lexeme *Node = &Sentinel;
    lexeme *Peek = PeekLex(Parser);
    while (IsPotentialUnaryOperator(Peek))
    {
        Node = Node->Right = Match(Parser, Peek->Type);
        Node->Left = 0;
        if (Node->Type == token_type_MINUS)
        {
            Node->Type = token_type_NEGATE;
        }
        Push(OperatorStack, Node);
        Peek = PeekLex(Parser);
    }
    switch (Peek->Type)
    {
        case token_type_CHAR:
        case token_type_STRING:
        case token_type_INTEGER:
        case token_type_REAL:
        case token_type_ID:
        {
            Node->Right = GetLex(Parser);
        } break;
        case token_type_OPAREN:
        {
            Match(Parser, token_type_OPAREN);
            Node->Right = Expression(Parser);
            Match(Parser, token_type_CPAREN);
        } break;
        case token_type_LIN:
        {
            Node->Right = Lin(Parser);
        } break;
        case token_type_TAB:
        {
            Node->Right = Tab(Parser);
        } break;
        case token_type_INT:
        {
            Node->Right = Int(Parser);
        } break;
        case token_type_RND:
        {
            Node->Right = Rnd(Parser);
        } break;
        case token_type_TIM:
        {
            Node->Right = Tim(Parser);
        } break;
        default:
        {
            Panic(Peek, "Invalid expression LHS %s\n", TokenTypeNames[Peek->Type].Memory);
        }
    }
    return Sentinel.Right;
}

u8 IsLeftToRight(lexeme *A, lexeme *B)
{
    operator_precedence Precedence = TokenTypePrecedences[B->Type];
    s32 Direction = TokenTypePrecedences[A->Type] - Precedence;
    if (!Direction)
    {
        Direction = PrecedenceDirections[Precedence];
    }
    return Direction > 0;
}

lexeme *Expression(parser *Parser)
{
    lexeme Sentinel = {};
    lexeme_stack OperatorStack;
    OperatorStack.Count = 0;
    lexeme *Operator;
    lexeme *PreviousOperator = &Sentinel;
    Push(&OperatorStack, &Sentinel);
    Sentinel.Right = SimpleExpression(Parser, &OperatorStack);
    while (IsBinaryOperator(PeekLex(Parser)))
    {
        Operator = GetLex(Parser);
        while (IsLeftToRight(Pop(&OperatorStack, &PreviousOperator), Operator));
        Push(&OperatorStack, PreviousOperator);
        Operator->Left = PreviousOperator->Right;
        Operator->Right = SimpleExpression(Parser, &OperatorStack);
        PreviousOperator->Right = Operator;
        Push(&OperatorStack, Operator);
    }
    return Sentinel.Right;
}

lexeme *PrintArgs(parser *Parser)
{
    lexeme Sentinel = {};
    lexeme *Parent = &Sentinel;
    while (PeekLex(Parser)->Type != token_type_NEWLINE)
    {
        Parent->Right = Expression(Parser);
        lexeme *Peek = PeekLex(Parser);
        if (Peek->Type == token_type_SEMICOLON || Peek->Type == token_type_COMMA)
        {
            lexeme *LHS = Parent->Right;
            Parent = Parent->Right = Match(Parser, Peek->Type);
            Parent->Left = LHS;
        }
        else
        {
            break;
        }
    }
    return Sentinel.Right;
}

lexeme *PrintStatement(parser *Parser)
{
    lexeme *Print = Match(Parser, token_type_PRINT);
    Print->Left = PrintArgs(Parser);
    return Print;
}

lexeme *DimStatement(parser *Parser)
{
    lexeme *Dim = Match(Parser, token_type_DIM);
    lexeme *Id = Match(Parser, token_type_ID);
    if (!Id->IsString)
    {
        Panic(Id, "Can only DIM string variables, not ones like %.*s\n", Id->String.Length, Id->String.Memory);
    }
    Match(Parser, token_type_OBRACKET);
    lexeme *Integer = Match(Parser, token_type_INTEGER);
    Match(Parser, token_type_CBRACKET);
    Dim->String = Id->String;
    Dim->IsString = Id->IsString;
    Dim->Integer = Integer->Integer;
    return Dim;
}

lexeme *InputStatement(parser *Parser)
{
    lexeme *Input = Match(Parser, token_type_INPUT);
    lexeme *Id = Match(Parser, token_type_ID);
    Input->String = Id->String;
    Input->IsString = Id->IsString;
    return Input;
}

lexeme *IfStatement(parser *Parser)
{
    lexeme *If = Match(Parser, token_type_IF);
    If->Left = Expression(Parser);
    Match(Parser, token_type_THEN);
    If->Integer = Match(Parser, token_type_INTEGER)->Integer;
    return If;
}

lexeme *GotoStatement(parser *Parser)
{
    lexeme *Goto = Match(Parser, token_type_GOTO);
    if (PeekLex(Parser, 1)->Type == token_type_NEWLINE)
    {
        Goto->Integer = Match(Parser, token_type_INTEGER)->Integer;
    }
    else
    {
        lexeme *Cons = Goto->Left = PushLexeme(Parser, token_type_CONS);
        Cons->Left = Expression(Parser);
        Match(Parser, token_type_OF);
        Cons = Cons->Right = Match(Parser, token_type_INTEGER);
        while (PeekLex(Parser)->Type == token_type_COMMA)
        {
            Match(Parser, token_type_COMMA);
            Cons = Cons->Right = Match(Parser, token_type_INTEGER);
        }
    }
    return Goto;
}

lexeme *GosubStatement(parser *Parser)
{
    lexeme *Gosub = Match(Parser, token_type_GOSUB);
    Gosub->Integer = Match(Parser, token_type_INTEGER)->Integer;
    return Gosub;
}

lexeme *ReadStatement(parser *Parser)
{
    lexeme *Read = Match(Parser, token_type_READ);
    Read->Left = Match(Parser, token_type_ID);
    return Read;
}

lexeme *DataStatement(parser *Parser)
{
    lexeme *Data = Match(Parser, token_type_DATA);
    Push(&Parser->DataStatements, Data);
    lexeme *Node = Data->Left = Match(Parser, token_type_INTEGER);
    while (PeekLex(Parser)->Type == token_type_COMMA)
    {
        Match(Parser, token_type_COMMA);
        Node = Node->Right = Match(Parser, token_type_INTEGER);
    }
    return Data;
}

lexeme *EnterStatement(parser *Parser)
{
    lexeme *Enter = Match(Parser, token_type_ENTER);
    lexeme **ConsPtr = &Enter->Left;
    lexeme *Cons;
    if (PeekLex(Parser)->Type == token_type_POUND)
    {
        Cons = PushLexeme(Parser, token_type_CONS);
        *ConsPtr = Cons;
        Cons->Left = Match(Parser, token_type_POUND);
        Cons->Left->Right = Match(Parser, token_type_ID);
        ConsPtr = &Cons->Right;
    }
    if (PeekLex(Parser)->Type == token_type_COMMA)
    {
        Match(Parser, token_type_COMMA);
        Cons = PushLexeme(Parser, token_type_CONS);
        *ConsPtr = Cons;
        Cons->Left = Expression(Parser);
        ConsPtr = &Cons->Right;

        Match(Parser, token_type_COMMA);
        Cons = PushLexeme(Parser, token_type_CONS);
        *ConsPtr = Cons;
        Cons->Left = Match(Parser, token_type_ID);
        ConsPtr = &Cons->Right;

        Match(Parser, token_type_COMMA);
        Cons = PushLexeme(Parser, token_type_CONS);
        *ConsPtr = Cons;
        Cons->Left = Match(Parser, token_type_ID);
        ConsPtr = &Cons->Right;
    }
    return Enter;
}

lexeme *LetStatement(parser *Parser, lexeme *Let = 0)
{
    if (!Let)
    {
        Let = Match(Parser, token_type_LET);
    }
    lexeme *Id = Let->Left = Match(Parser, token_type_ID);
    Match(Parser, token_type_EQ);
    while (PeekLex(Parser)->Type == token_type_ID && PeekLex(Parser, 1)->Type == token_type_EQ)
    {
        Id = Id->Left = Match(Parser, token_type_ID);
        Match(Parser, token_type_EQ);
    }
    Id->Right = Expression(Parser);
    return Let;
}

lexeme *Statement(parser *Parser)
{
    lexeme *LineNumber = Match(Parser, token_type_INTEGER);
    Parser->BasicLineNumber = LineNumber->BasicLineNumber = LineNumber->Integer;
    lexeme *Command = 0;
    lexeme *Peek = PeekLex(Parser);
#if DEBUG_STATEMENT
    printf("%d %.*s\n", LineNumber->Integer, Peek->String.Length, Peek->String.Memory);
#endif
    switch (Peek->Type)
    {
        case token_type_REM:
        {
            GetLex(Parser);
        } break;
        case token_type_PRINT:
        {
            Command = PrintStatement(Parser);
        } break;
        case token_type_DIM:
        {
            Command = DimStatement(Parser);
        } break;
        case token_type_INPUT:
        {
            Command = InputStatement(Parser);
        } break;
        case token_type_IF:
        {
            Command = IfStatement(Parser);
        } break;
        case token_type_LET:
        {
            Command = LetStatement(Parser);
        } break;
        case token_type_GOTO:
        {
            Command = GotoStatement(Parser);
        } break;
        case token_type_GOSUB:
        {
            Command = GosubStatement(Parser);
        } break;
        case token_type_READ:
        {
            Command = ReadStatement(Parser);
        } break;
        case token_type_DATA:
        {
            Command = DataStatement(Parser);
        } break;
        case token_type_ENTER:
        {
            Command = EnterStatement(Parser);
        } break;
        case token_type_RETURN:
        case token_type_STOP:
        case token_type_END:
        case token_type_RESTORE:
        {
            Command = Match(Parser, Peek->Type);
        } break;
        default:
        {
            Command = LetStatement(Parser, PushLexeme(Parser, token_type_LET));
            Command->LineNumber = Peek->LineNumber;
        } break;
    }
    if (Command)
    {
        Parser->Lines[LineNumber->Integer].Lexeme = Command;
    }
    Match(Parser, token_type_NEWLINE);
    return Command;
}

lexeme StatementList(parser *Parser)
{
    lexeme Result;
    lexeme *PreviousStatement = &Result;
    lexeme *Peek = PeekLex(Parser);
    while (Peek->Type != token_type_EOF)
    {
        if (Peek->Type == token_type_NEWLINE)
        {
            Match(Parser, token_type_NEWLINE);
        }
        else
        {
            lexeme *Command = Statement(Parser);
            if (Command)
            {
                PreviousStatement = PreviousStatement->Right = Command;
            }
        }
        Peek = PeekLex(Parser);
    }
    return Result;
}

lexeme *EvaluateEquality(environment *Environment, lexeme *LHS, lexeme *RHS, lexeme *Output)
{
    lexeme Result;
    Result.Type = token_type_INTEGER;
    if (LHS->Type == RHS->Type)
    {
        switch (LHS->Type)
        {
            case token_type_STRING:
            {
                Result.Integer = Equals(LHS->String, RHS->String);
            } break;
            case token_type_INTEGER:
            {
                Result.Integer = LHS->Integer == RHS->Integer;
            } break;
            default:
            {
                Panic(LHS, "Cannot compare values of type %s", TokenTypeNames[LHS->Type].Memory);
            } break;
        }
    }
    else
    {
        Panic(LHS, "Cannot compare values with different types %s and %s", TokenTypeNames[LHS->Type].Memory, TokenTypeNames[RHS->Type].Memory);
    }
    *Output = Result;
    return Output;
}

lexeme *NumberOperation(environment *Environment, lexeme *Operator, lexeme *Output)
{
    lexeme LHS, RHS;
    LHS.Type = token_type_INTEGER;
    LHS.Integer = 0;
    if (Operator->Left)
    {
        EvaluateExpression(Environment, Operator->Left, &LHS);
    }
    EvaluateExpression(Environment, Operator->Right, &RHS);
    if (!IsNumber(LHS.Type) || !IsNumber(RHS.Type))
    {
        Panic(Operator, "Cannot numerically operate on types %s and %s", TokenTypeNames[LHS.Type].Memory, TokenTypeNames[RHS.Type].Memory);
    }
    else if (LHS.Type == token_type_INTEGER && RHS.Type == token_type_INTEGER)
    {
        Output->Type = token_type_INTEGER;
        switch (Operator->Type)
        {
            case token_type_PLUS:
            {
                Output->Integer = LHS.Integer + RHS.Integer;
            } break;
            case token_type_MINUS:
            {
                Output->Integer = LHS.Integer - RHS.Integer;
            } break;
            case token_type_STAR:
            {
                Output->Integer = LHS.Integer * RHS.Integer;
            } break;
            case token_type_SLASH:
            {
                Output->Type = token_type_REAL;
                Output->Real = (r32)LHS.Integer / (r32)RHS.Integer;
            } break;
            case token_type_CARET:
            {
                Output->Type = token_type_REAL;
                Output->Real = pow((r32)LHS.Integer, (r32)RHS.Integer);
            } break;
            default:
            {
                Panic(Operator, "No number operation defined for type %s", TokenTypeNames[Operator->Type].Memory);
            } break;
        }
    }
    else
    {
        r32 LHSReal = (LHS.Type == token_type_REAL) ? LHS.Real : (r32)LHS.Integer;
        r32 RHSReal = (RHS.Type == token_type_REAL) ? RHS.Real : (r32)RHS.Integer;
        Output->Type = token_type_REAL;
        switch (Operator->Type)
        {
            case token_type_PLUS:
            {
                Output->Real = LHSReal + RHSReal;
            } break;
            case token_type_MINUS:
            {
                Output->Real = LHSReal - RHSReal;
            } break;
            case token_type_STAR:
            {
                Output->Real = LHSReal * RHSReal;
            } break;
            case token_type_SLASH:
            {
                Output->Real = LHSReal / RHSReal;
            } break;
            case token_type_CARET:
            {
                Output->Real = pow(LHSReal, RHSReal);
            } break;
            default:
            {
                Panic(Operator, "No number operation defined for type %s", TokenTypeNames[Operator->Type].Memory);
            } break;
        }
    }

    return Output;
}

lexeme *NumberComparison(environment *Environment, lexeme *Operator, lexeme *Output)
{
    lexeme LHS, RHS;
    EvaluateExpression(Environment, Operator->Left, &LHS);
    EvaluateExpression(Environment, Operator->Right, &RHS);
    Output->Type = token_type_INTEGER;

    if (!IsNumber(LHS.Type) || !IsNumber(RHS.Type))
    {
        Panic(Operator, "Cannot compare types %s and %s", TokenTypeNames[LHS.Type].Memory, TokenTypeNames[RHS.Type].Memory);
    }
    else if (LHS.Type == token_type_INTEGER && RHS.Type == token_type_INTEGER)
    {
        switch (Operator->Type)
        {
            case token_type_GT:
            {
                Output->Integer = LHS.Integer > RHS.Integer;
            } break;
            case token_type_LT:
            {
                Output->Integer = LHS.Integer < RHS.Integer;
            } break;
            case token_type_GTE:
            {
                Output->Integer = LHS.Integer >= RHS.Integer;
            } break;
            case token_type_LTE:
            {
                Output->Integer = LHS.Integer <= RHS.Integer;
            } break;
            default:
            {
                Panic(Operator, "No comparison defined for type %s", TokenTypeNames[Operator->Type].Memory);
            } break;
        }
    }
    else
    {
        r32 LHSReal = (LHS.Type == token_type_REAL) ? LHS.Real : (r32)LHS.Integer;
        r32 RHSReal = (RHS.Type == token_type_REAL) ? RHS.Real : (r32)RHS.Integer;
        switch (Operator->Type)
        {
            case token_type_GT:
            {
                Output->Integer = LHSReal > RHSReal;
            } break;
            case token_type_LT:
            {
                Output->Integer = LHSReal < RHSReal;
            } break;
            case token_type_GTE:
            {
                Output->Integer = LHSReal >= RHSReal;
            } break;
            case token_type_LTE:
            {
                Output->Integer = LHSReal <= RHSReal;
            } break;
            default:
            {
                Panic(Operator, "No comparison defined for type %s", TokenTypeNames[Operator->Type].Memory);
            } break;
        }
    }

    return Output;
}

lexeme *ToBoolean(environment *Environment, lexeme *Value, lexeme *Output)
{
    lexeme Boolean;
    Boolean.Type = token_type_BOOLEAN;
    switch (Value->Type)
    {
        case token_type_STRING:
        {
            Boolean.Integer = !!Value->String.Length;
        } break;
        case token_type_BOOLEAN:
        case token_type_INTEGER:
        {
            Boolean.Integer = !!Value->Integer;
        } break;
#if 0
        case token_type_REAL:
        {
            Boolean.Integer = Value->Real != 0.0f;
        } break;
#endif
        default:
        {
            Panic(Value, "Cannot convert %s to boolean", TokenTypeNames[Value->Type].Memory);
        } break;
    }
    *Output = Boolean;
    return Output;
}

enum integer_cast_method
{
    integer_cast_method_Floor,
    integer_cast_method_Ceiling,
    integer_cast_method_Round,
};

lexeme *ToInteger(environment *Environment, lexeme *Value, lexeme *Output, integer_cast_method Cast = integer_cast_method_Floor)
{
    lexeme Integer;
    Integer.Type = token_type_INTEGER;
    switch (Value->Type)
    {
        case token_type_STRING:
        {
            Integer.Integer = Value->String.Length;
        } break;
        case token_type_INTEGER:
        {
            Integer.Integer = Value->Integer;
        } break;
        case token_type_REAL:
        {
            switch (Cast)
            {
                case integer_cast_method_Floor:
                {
                    Integer.Integer = (s32)Value->Real;
                } break;
                case integer_cast_method_Ceiling:
                {
                    Integer.Integer = (s32)(Value->Real + 1.0f);
                } break;
                case integer_cast_method_Round:
                {
                    Integer.Integer = (s32)(Value->Real + 0.5f);
                } break;
            }
        } break;
        default:
        {
            Panic(Value, "Cannot convert %s to integer", TokenTypeNames[Value->Type].Memory);
        } break;
    }
    *Output = Integer;
    return Output;
}

lexeme *EvaluateExpression(environment *Environment, lexeme *Expr, lexeme *Value)
{
    memory_arena *StringArena = &Environment->Parser.StringArena;
    lexeme LHS;
    lexeme RHS;
    switch (Expr->Type)
    {
        case token_type_LIN:
        {
            EvaluateExpression(Environment, Expr->Left, Value);
            ToInteger(Environment, Value, Value);
            Value->Type = token_type_STRING;
            Value->String = BeginString(StringArena);
            while (Value->Integer--)
            {
                Value->String.Length += Append(StringArena, '\n');
            }
        } break;
        case token_type_TAB:
        {
            EvaluateExpression(Environment, Expr->Left, Value);
            ToInteger(Environment, Value, Value);
            Value->Type = token_type_STRING;
            Value->String = BeginString(StringArena);
            while (Value->Integer--)
            {
                Value->String.Length += Append(StringArena, ' ');
            }
        } break;
        case token_type_RND:
        {
            Value->Type = token_type_REAL;
            Value->Real = ((r32)rand() / RAND_MAX) * 0.999f;
        } break;
        case token_type_INT:
        {
            EvaluateExpression(Environment, Expr->Left, Value);
            ToInteger(Environment, Value, Value);
        } break;
        case token_type_CHAR:
        case token_type_STRING:
        case token_type_INTEGER:
        case token_type_REAL:
        {
            *Value = *Expr;
        } break;
        case token_type_ID:
        {
            *Value = *LookupVariable(Environment, Expr);
        } break;
        case token_type_EQ:
        {
            EvaluateExpression(Environment, Expr->Left, &LHS);
            EvaluateExpression(Environment, Expr->Right, &RHS);
            EvaluateEquality(Environment, &LHS, &RHS, Value);
        } break;
        case token_type_GTE:
        case token_type_GT:
        case token_type_LTE:
        case token_type_LT:
        {
            NumberComparison(Environment, Expr, Value);
        } break;
        case token_type_OR:
        {
            s32 Integer = ToBoolean(Environment, EvaluateExpression(Environment, Expr->Left, &LHS), Value)->Integer || ToBoolean(Environment, EvaluateExpression(Environment, Expr->Right, &RHS), Value)->Integer;
            Assert(Integer == Value->Integer);
        } break;
        case token_type_AND:
        {
            s32 Integer = ToBoolean(Environment, EvaluateExpression(Environment, Expr->Left, &LHS), Value)->Integer && ToBoolean(Environment, EvaluateExpression(Environment, Expr->Right, &RHS), Value)->Integer;
            Assert(Integer == Value->Integer);
        } break;
        case token_type_NOT:
        {
            ToBoolean(Environment, EvaluateExpression(Environment, Expr->Right, &LHS), Value);
            Value->Integer = !Value->Integer;
        } break;
        case token_type_PLUS:
        case token_type_MINUS:
        case token_type_STAR:
        case token_type_SLASH:
        case token_type_CARET:
        {
            NumberOperation(Environment, Expr, Value);
        } break;
        case token_type_NEGATE:
        {
            EvaluateExpression(Environment, Expr->Right, Value);
            switch (Value->Type)
            {
                case token_type_INTEGER:
                {
                    Value->Integer = -Value->Integer;
                } break;
                case token_type_REAL:
                {
                    Value->Real = -Value->Real;
                } break;
                default:
                {
                    Panic(Expr, "Cannot negate %s", TokenTypeNames[Value->Type].Memory);
                } break;
            }
        } break;
        default:
        {
            Panic(Expr, "Cannot evaluate %s", TokenTypeNames[Expr->Type].Memory);
        } break;
    }
    return Value;
}

lexeme *ToString(memory_arena *Arena, lexeme *Value, lexeme *String)
{
    lexeme Temp;
    Temp.Type = token_type_STRING;
    Temp.String = BeginString(Arena);
    switch (Value->Type)
    {
        case token_type_CHAR:
        {
            Temp.String.Length = Append(Arena, Value->Character);
            *String = Temp;
        } break;
        case token_type_STRING:
        {
            *String = *Value;
        } break;
        case token_type_BOOLEAN:
        {
            Temp.String.Length = Append(Arena, (char*)(Value->Integer ? "true" : "false"));
            *String = Temp;
        } break;
        case token_type_REAL:
        {
            Temp.String.Length = Append(Arena, Value->Real);
            *String = Temp;
        } break;
        case token_type_INTEGER:
        {
            Temp.String.Length = Append(Arena, Value->Integer);
            *String = Temp;
        } break;
        default:
        {
            Panic(Value, "Cannot convert %s to string", TokenTypeNames[Value->Type].Memory);
        } break;
    }
    return String;
}

lexeme *ToString(environment *Environment, lexeme *Value, lexeme *String)
{
    return ToString(&Environment->Parser.StringArena, Value, String);
}

size_t RenderString(string_reference String, memory_arena *Arena)
{
    size_t Result = 0;
    u8 Quoted = 0;
    s32 BufferCountdown = String.Length;
    char *At = String.Memory;
    while (BufferCountdown)
    {
        if (IsPrintable(*At))
        {
            Result += Append(Arena, '"');
            do
            {
                Result += Append(Arena, *At++);
            } while (--BufferCountdown && IsPrintable(*At));
            Result += Append(Arena, '"');
        }
        else
        {
            do
            {
                Result += Append(Arena, '\'');
                Result += Append(Arena, (s32)*At++);
            } while (--BufferCountdown && !IsPrintable(*At));
        }
    }
    return Result;
}

size_t RenderExpression(lexeme *Expr, memory_arena *Arena)
{
    size_t Result = 0;
    size_t StackCount = 0;
    lexeme *Stack[64];
    s32 RightBalanceStack[64];
    s32 Parens;
    s32 NextRightBalance = 0;
    s32 LeftBalance;
    s32 RightBalance;
    lexeme *NextExpr = Expr;
    while (NextExpr || StackCount)
    {
        LeftBalance = RightBalance = 0;
        if (NextExpr)
        {
            Expr = NextExpr;
            RightBalance = NextRightBalance;
            while (Expr->Left)
            {
                if (sizeof Stack <= StackCount)
                {
                    Panic(Expr, "stack overflow");
                }
                RightBalanceStack[StackCount] = NextRightBalance;
                RightBalance = NextRightBalance = 0;
                Stack[StackCount++] = Expr;
                LeftBalance--;
                Expr = Expr->Left;
            }
        }
        else
        {
            Expr = Stack[--StackCount];
            RightBalance = RightBalanceStack[StackCount];
            NextRightBalance = RightBalance + 1;
        }
        NextExpr = Expr->Right;
        if (IsUnaryOperator(Expr))
        {
            LeftBalance--;
            NextRightBalance = RightBalance + 1;
        }

        Parens = LeftBalance;
        while (Parens++)
        {
            Result += Append(Arena, '(');
        }
        switch (Expr->Type)
        {
            case token_type_PLUS:
            {
                Result += Append(Arena, '+');
            } break;
            case token_type_NEWLINE:
            {
                Result += Append(Arena, '@');
            } break;
            case token_type_MINUS:
            case token_type_NEGATE:
            {
                Result += Append(Arena, '-');
            } break;
            case token_type_STAR:
            {
                Result += Append(Arena, '*');
            } break;
            case token_type_SLASH:
            {
                Result += Append(Arena, '/');
            } break;
            case token_type_INTEGER:
            {
                Result += Append(Arena, Expr->Integer);
            } break;
            case token_type_REAL:
            {
                Result += Append(Arena, Expr->Real);
            } break;
            case token_type_CHAR:
            {
                Result += Append(Arena, '\'');
                Result += Append(Arena, (s32)Expr->Character);
            } break;
            case token_type_STRING:
            case token_type_ID:
            {
                Result += RenderString(Expr->String, Arena);
            } break;
            case token_type_EQ:
            {
                Result += Append(Arena, '=');
            } break;
            case token_type_LT:
            {
                Result += Append(Arena, '<');
            } break;
            case token_type_LTE:
            {
                Result += Append(Arena, '<');
                Result += Append(Arena, '=');
            } break;
            case token_type_GTE:
            {
                Result += Append(Arena, '>');
                Result += Append(Arena, '=');
            } break;
            case token_type_GT:
            {
                Result += Append(Arena, '>');
            } break;
            case token_type_CARET:
            {
                Result += Append(Arena, '^');
            } break;
            default:
            {
                s32 IsUnary = IsUnaryOperator(Expr);
                s32 IsBinary = IsBinaryOperator(Expr);
                if (IsBinary)
                {
                    Result += Append(Arena, ' ');
                }
                Result += Append(Arena, TokenTypeNames[Expr->Type]);
                if (IsBinary || IsUnary)
                {
                    Result += Append(Arena, ' ');
                }
            } break;
        }
        Parens = RightBalance;
        if (!NextExpr && !LeftBalance)
        {
            while (Parens--)
            {
                Result += Append(Arena, ')');
            }
        }
    }
    return Result;
}

void DebugExpression(lexeme *Expr)
{
    char Memory[1024];
    memory_arena Arena = {};
    Arena.Memory = Memory;
    Arena.Size = sizeof Memory;
    RenderExpression(Expr, &Arena);
    printf("DEBUG: %.*s\n", (s32)Arena.Allocated, (char *)Arena.Memory);
}

void Debug(lexeme *Lexeme)
{
    char Memory[1024];
    memory_arena Arena = {};
    Arena.Memory = Memory;
    Arena.Size = sizeof Memory;
    lexeme String;
    ToString(&Arena, Lexeme, &String);
    printf("DEBUG: %.*s\n", String.String.Length, String.String.Memory);
}

void EvaluatePrint(environment *Environment, lexeme *Print)
{
    lexeme *Args = Print->Left;
    lexeme *Expr = 0;
    s32 NextPrefixWidth, PrefixWidth;
    char NextPrefix, Prefix;
    Prefix = 0;
    u8 SuppressNewline = 0;
    s32 PrintCursor = 0;
    u8 IsTab = 0;
    while (Args)
    {
        IsTab = SuppressNewline = NextPrefix = 0;
        NextPrefixWidth = 1;
        switch (Args->Type)
        {
            case token_type_COMMA:
            {
                Expr = Args->Left;
                IsTab = 1;
            } break;
            case token_type_SEMICOLON:
            {
                Expr = Args->Left;
                SuppressNewline = 1;
            } break;
            case token_type_LIN:
            {
                Expr = Args;
                SuppressNewline = 1;
            } break;
            default:
            {
                Expr = Args;
            } break;
        }
        if (Expr)
        {
            lexeme Value;
            EvaluateExpression(Environment, Expr, &Value);
            const char *LeftPad = "";
            s32 MinWidth = 0;
            if (Value.Type == token_type_INTEGER)
            {
                MinWidth = 5;
                if (0 <= Value.Integer)
                {
                    LeftPad = " ";
                }
            }
            temporary_memory TemporaryMemory = BeginTemporaryMemory(&Environment->Parser.StringArena);
            ToString(Environment, &Value, &Value);
            if (Value.Type != token_type_STRING)
            {
                Panic(&Value, "Cannot print non-string value %s", TokenTypeNames[Value.Type].Memory);
            }
            if (Prefix)
            {
                s32 Before = PrintCursor;
                PrintCursor += printf("%*c", PrefixWidth, Prefix);
            }
            PrintCursor += printf("%s%-*.*s", LeftPad, MinWidth, Value.String.Length, Value.String.Memory);
            EndTemporaryMemory(&Environment->Parser.StringArena, TemporaryMemory);
        }
        if (IsTab)
        {
            if (56 <= PrintCursor)
            {
                // TODO
            }
            s32 CurrentWidth = PrintCursor % 15;
            NextPrefixWidth = 15 - CurrentWidth;
            NextPrefix = ' ';
        }
        PrefixWidth = NextPrefixWidth;
        Prefix = NextPrefix;
        Args = Args->Right;
    }
    if (!SuppressNewline)
    {
        printf("\n");
    }
    fflush(stdout);
}

void EvaluateDim(environment *Environment, lexeme *Lexeme)
{
    lexeme *Value = DeclareVariable(Environment, Lexeme);
    Value->IsString = 1;
    Value->Integer = Lexeme->Integer;
    size_t BytesAllocated = AllocateString(&Environment->Parser.StringArena, Value->Integer, &Value->String.Memory);
    if (Value->Integer != BytesAllocated)
    {
        Panic(Lexeme, "Not enough memory to allocate string; allocated %d of %d bytes", BytesAllocated, Value->Integer);
    }
}

u64 GetTimeMilliseconds()
{
    timespec Now;
    clock_gettime(CLOCK_REALTIME, &Now);
    u64 Result = (u64)Now.tv_sec * 1000.0 + (u64)Now.tv_nsec * 0.000001;
    return Result;
}

s32 StringInput(environment *Environment, lexeme *Id, r32 AllowedSeconds = -1, lexeme *SecondsElapsed = 0)
{
    lexeme *Variable = FindVariable(Environment, Id);
    if (!Variable)
    {
        Panic(Id, "Attempted to INPUT an unDIM'd string %.*s", Id->String.Length, Id->String.Memory);
    }
    if (!Id->IsString || !Variable->IsString)
    {
        Panic(Id, "Cannot store string in non-string variable %.*s", Id->String.Length, Id->String.Memory);
    }
    s32 Char = 0;
    buffer Buffer = NewBuffer(Variable->String.Memory, Variable->Integer);
    s32 ElapsedMilliseconds = 0;
    u64 StartTimeMilliseconds = GetTimeMilliseconds();
    timed_input TimedInput;
    s32 AllowedMilliseconds = TimedInput.TimeoutMilliseconds = AllowedSeconds * 1000;
    s32 MinTimeoutMilliseconds = AllowedMilliseconds < 0 ? AllowedMilliseconds : 0;

    do
    {
        Char = GetChar(Environment, &TimedInput);
#if DEBUG_TIMED_INPUT
        string_builder Builder;
        Reset(&Builder);
        Append(&Builder, "TimedOut: ");
        Append(&Builder, TimedInput.TimedOut);
        Newline(&Builder);
        Append(&Builder, "Polled: ");
        Append(&Builder, TimedInput.Polled);
        Newline(&Builder);
        Print(&Builder);
#endif
        if (TimedInput.TimedOut)
        {
            Buffer.At = Buffer.Contents;
            ElapsedMilliseconds = AllowedMilliseconds;
        }
        else
        {
            if (TimedInput.Polled)
            {
                ElapsedMilliseconds = GetTimeMilliseconds() - StartTimeMilliseconds;
                TimedInput.TimeoutMilliseconds = Max(MinTimeoutMilliseconds, AllowedMilliseconds - ElapsedMilliseconds);
            }
            if (Char != '\n' && !Full(Buffer))
            {
                *Buffer.At++ = Char;
            }
        }
    } while (!TimedInput.TimedOut && Char != '\n');

    if (SecondsElapsed)
    {
        if (TimedInput.TimedOut)
        {
            SecondsElapsed->Type = token_type_INTEGER;
            SecondsElapsed->Integer = -256;
        }
        else
        {
            SecondsElapsed->Type = token_type_REAL;
            SecondsElapsed->Real = ElapsedMilliseconds * 0.001f;
        }
    }

    Variable->Type = token_type_STRING;
    s32 Length = Buffer.At - Buffer.Contents;
    Variable->String.Length = Length;
#if DEBUG_STRING_INPUT
    printf("(%d)%.*s\n", Variable->String.Length, Variable->String.Length, Variable->String.Memory);
#endif
    return Length;
}

void EvaluateInput(environment *Environment, lexeme *Lexeme)
{
    signed char Char;
    s32 Again = 0;
    do
    {
        printf("?%s", Again ? "?" : "");
        fflush(stdout);
        if (Lexeme->IsString)
        {
            Again = !StringInput(Environment, Lexeme);
        }
        else
        {
            Again = 1;
            lexeme *Variable = LookupOrDeclareVariable(Environment, Lexeme);
            s32 Integer = 0;
            s32 Multiplier = 1;
            Char = GetChar(Environment);
            while (!(Char == '-' || Char == '\n' || isdigit(Char)))
            {
                Char = GetChar(Environment);
            }
            if (Char == '-')
            {
                Multiplier = -1;
                Char = GetChar(Environment);
            }
            while (isdigit(Char))
            {
                Again = 0;
                Integer = 10 * Integer + Char - '0';
                Char = GetChar(Environment);
            }
            if (Again && Multiplier < 0)
            {
                printf("BAD INPUT, RETYPE FROM ITEM 1\n");
            }
            else if (Char != '\n')
            {
                printf("EXTRA INPUT - WARNING ONLY\n");
            }
            while (Char != '\n')
            {
                Char = GetChar(Environment);
            }
            Variable->Type = token_type_INTEGER;
            Variable->Integer = Multiplier * Integer;
        }
    } while (Again);
}

void EvaluateIf(environment *Environment, lexeme *Lexeme)
{
    lexeme Value;
#if DEBUG_EVAL_IF
    DebugExpression(Lexeme->Left);
#endif
    EvaluateExpression(Environment, Lexeme->Left, &Value);
    ToBoolean(Environment, &Value, &Value);
#if DEBUG_EVAL_IF
    Debug(&Value);
#endif
    if (Value.Integer)
    {
        Environment->Goto = Environment->Parser.Lines[Lexeme->Integer].Lexeme;
    }
}

void EvaluateEnter(environment *Environment, lexeme *Lexeme)
{
    lexeme *Cons = Lexeme->Left;
    Assert(Cons);
    lexeme *Id, *StringId, *Variable, *Expr;
    lexeme Value;
    if (Cons->Left->Type == token_type_POUND)
    {
        Id = Cons->Left->Right;
        Assert(Id->Type == token_type_ID);
        Variable = LookupOrDeclareVariable(Environment, Id);
        Variable->Type = token_type_INTEGER;
        Variable->Integer = 0;
        Cons = Cons->Right;
    }
    if (Cons)
    {
        Expr = Cons->Left;
        Id = Cons->Right->Left;
        StringId = Cons->Right->Right->Left;

        ToInteger(Environment, EvaluateExpression(Environment, Expr, &Value), &Value, integer_cast_method_Round);
        if (255 < Value.Integer || Value.Integer < 1)
        {
            Panic(Expr, "The allotted time must be between 1 and 255");
        }

        Variable = LookupOrDeclareVariable(Environment, Id);
        StringInput(Environment, StringId, Value.Integer, Variable);
    }
}

void EvaluateGoto(environment *Environment, lexeme *Lexeme)
{
    lexeme *Goto = Lexeme;
    if (Lexeme->Left)
    {
        lexeme *Cons = Goto = Lexeme->Left;
        lexeme Choice;
        EvaluateExpression(Environment, Cons->Left, &Choice);
        s32 ChoiceIndex = Choice.Integer;
        if (ChoiceIndex < 1)
        {
            Panic(Lexeme, "Cannot GOTO/OF with branch choice %d", ChoiceIndex);
        }
        while (ChoiceIndex)
        {
            Goto = Goto->Right;
            if (!Goto)
            {
                Panic(Lexeme, "Cannot GOTO/OF with branch choice %d with only %d choice(s)", Choice.Integer, Choice.Integer - ChoiceIndex);
            }
            ChoiceIndex--;
        }
    }
    Environment->Goto = Environment->Parser.Lines[Goto->Integer].Lexeme;
}

void PushStack(environment *Environment, lexeme *Return)
{
    if (ArrayCount(Environment->Stack) <= Environment->StackLength)
    {
        Panic(Return, "Stack overflow");
    }
    Environment->Stack[Environment->StackLength++] = {Return};
}

lexeme *PopStack(environment *Environment, lexeme *Return)
{
    if (!Environment->StackLength)
    {
        Panic(Return, "Stack underflow");
    }
    return Environment->Stack[--Environment->StackLength].Lexeme;
}

void EvaluateGosub(environment *Environment, lexeme *Lexeme)
{
    PushStack(Environment, Lexeme->Right);
    EvaluateGoto(Environment, Lexeme);
}

void EvaluateReturn(environment *Environment, lexeme *Lexeme)
{
    Environment->Goto = PopStack(Environment, Lexeme);
}

void EvaluateLetAssignment(environment *Environment, lexeme *Id, lexeme *Value)
{
    lexeme *Variable = LookupOrDeclareVariable(Environment, Id);
    if (Id->Right)
    {
        EvaluateExpression(Environment, Id->Right, Value);
    }
    else
    {
        EvaluateLetAssignment(Environment, Id->Left, Value);
    }
    if (Id->IsString)
    {
        Assert(Variable->IsString);
        lexeme String;
        temporary_memory TemporaryMemory = BeginTemporaryMemory(&Environment->Parser.StringArena);
        ToString(Environment, Value, &String);
        Variable->String.Length = 0;
        s32 BufferCountdown = Variable->String.Length = Min(Variable->Integer, String.String.Length);
        char *Source = String.String.Memory;
        char *Dest = Variable->String.Memory;
        while (BufferCountdown--)
        {
            *Dest++ = *Source++;
        }
        EndTemporaryMemory(&Environment->Parser.StringArena, TemporaryMemory);
    }
    else
    {
        *Variable = *Value;
    }
}

void EvaluateLet(environment *Environment, lexeme *Lexeme)
{
    lexeme Sentinel;
    EvaluateLetAssignment(Environment, Lexeme->Left, &Sentinel);
}

void EvaluateRestore(environment *Environment, lexeme *Lexeme)
{
    Environment->DataPosition = 0;
}

void EvaluateRead(environment *Environment, lexeme *Lexeme)
{
    if (Environment->DataCount <= Environment->DataPosition)
    {
        Panic(Lexeme, "Read past the end of the data values");
    }
    lexeme *Id = Lexeme->Left;
    lexeme *Variable = LookupOrDeclareVariable(Environment, Id);
    s32 Value = 0;
    Variable->Type = token_type_INTEGER;
    Variable->Integer = Environment->DataEntries[Environment->DataPosition++];
}

void Evaluate(environment *Environment, lexeme *Lexeme)
{
    parser *Parser = &Environment->Parser;
    lexeme_stack *DataStatements = &Parser->DataStatements;
    for (s32 DataStatementIndex = 0; DataStatementIndex < DataStatements->Count; ++DataStatementIndex)
    {
        lexeme *Integer = DataStatements->Stack[DataStatementIndex]->Left;
        while (Integer)
        {
            if (sizeof Environment->DataEntries <= Environment->DataCount)
            {
                Panic(Lexeme, "Too many data entries");
            }
            Environment->DataEntries[Environment->DataCount++] = Integer->Integer;
            Integer = Integer->Right;
        }
    }
    u8 Running = 1;
    while (Lexeme && Running)
    {
#if DEBUG_EVALUATE_STATEMENT
        printf("%s\n", TokenTypeNames[Lexeme->Type].Memory);
#endif
        Environment->Goto = 0;
        switch (Lexeme->Type)
        {
            case token_type_PRINT:
            {
                EvaluatePrint(Environment, Lexeme);
            } break;
            case token_type_DIM:
            {
                EvaluateDim(Environment, Lexeme);
            } break;
            case token_type_INPUT:
            {
                EvaluateInput(Environment, Lexeme);
            } break;
            case token_type_IF:
            {
                EvaluateIf(Environment, Lexeme);
            } break;
            case token_type_GOTO:
            {
                EvaluateGoto(Environment, Lexeme);
            } break;
            case token_type_GOSUB:
            {
                EvaluateGosub(Environment, Lexeme);
            } break;
            case token_type_RETURN:
            {
                EvaluateReturn(Environment, Lexeme);
            } break;
            case token_type_LET:
            {
                EvaluateLet(Environment, Lexeme);
            } break;
            case token_type_RESTORE:
            {
                EvaluateRestore(Environment, Lexeme);
            } break;
            case token_type_READ:
            {
                EvaluateRead(Environment, Lexeme);
            } break;
            case token_type_DATA:
            {
            } break;
            case token_type_ENTER:
            {
                EvaluateEnter(Environment, Lexeme);
            } break;
            case token_type_STOP:
            case token_type_END:
            {
                Running = 0;
            } break;
            default:
            {
                Panic(Lexeme, "Can't evaluate %s", TokenTypeNames[Lexeme->Type].Memory);
            } break;
        }
        if (Environment->Goto)
        {
            Lexeme = Environment->Goto;
        }
        else
        {
            Lexeme = Lexeme->Right;
        }
    }
}

void Reset(environment *Environment)
{
    parser *Parser = &Environment->Parser;
    memory_arena *Arena = &Parser->StringArena;
    Arena->Allocated = 0;
    Parser->Position = 0;
    Parser->NaturalLexemeCount = Parser->LexemeCount = Parser->LexemePosition = 0;
    Parser->SourceLineNumber = 1;
    Parser->DataStatements.Count = 0;
    Environment->Goto = 0;
    Environment->VariableCount = 0;
    Environment->StackLength = 0;
    Environment->DataCount = Environment->DataPosition = 0;
}

#pragma pack(push, 1)
union magic_number
{
    u32 Integer;
    char String[4];
};
struct cbas_footer
{
    magic_number MagicNumber;
    u32 Version;
    u64 DataOffset;
};
#pragma pack(pop)

#if RUN_TESTS
#include "test.cpp"
#endif

int main(int ArgCount, char *Args[])
{
    FileControl(STDIN_FILENO, F_SETFL, FileControl(STDIN_FILENO, F_GETFL) | O_NONBLOCK);

    char StringMemory[65536];
    environment Environment;
    parser *Parser = &Environment.Parser;
    Parser->StringArena.Memory = StringMemory;
    Parser->StringArena.Size = sizeof StringMemory;
#if RUN_TESTS
    int TestFailures = Test(&Environment);
    Assert(!TestFailures);
#endif
    Reset(&Environment);
    program Program = {};
    Program.ExecutableName = Program.ExecutablePath = Args[0];
    for (char *At = Program.ExecutablePath; *At; At++)
    {
        if (*At == '/')
        {
            Program.ExecutableName = ++At;
        }
    }
    int Executable = Open(Program.ExecutablePath, O_RDONLY);
    if (Executable < 0)
    {
        Panic("Cannot open executable file %s", Program.ExecutablePath);
    }

    Program.ExecutableFileSize = Seek(Executable, 0, SEEK_END);
    cbas_footer CbasFooter;
    magic_number CbasMagicNumber = {.String = {'C','B','A','S'}};
    Program.DataOffset = Program.FooterOffset = Program.ExecutableFileSize - sizeof CbasFooter;

    Seek(Executable, Program.FooterOffset, SEEK_SET);
    if (Read(Executable, &CbasFooter, sizeof CbasFooter) <= 0 ||
        CbasFooter.MagicNumber.Integer != CbasMagicNumber.Integer)
    {
        Warn("Missing CBAS Magic Number. Defaulting to CLI mode.");
        Program.DataOffset = Program.FooterOffset = Program.ExecutableFileSize;
    }
    else
    {
        switch (CbasFooter.Version)
        {
            case 0:
            {
                // CLI Mode.
            } break;

            case 1:
            {
                if (CbasFooter.DataOffset <= Program.FooterOffset)
                {
                    Program.DataOffset = CbasFooter.DataOffset;
                }
                else
                {
                    Panic("CBAS Data offset is larger than the footer offset.");
                }
            } break;

            default:
            {
                Panic("Invalid CBAS version %u.", CbasFooter.Version);
            } break;
        }
    }
    u64 DataSize = Program.FooterOffset - Program.DataOffset;
    if (CbasFooter.Version)
    {
        Seek(Executable, Program.DataOffset, SEEK_SET);
        Parser->Size = DataSize;
        Parser->Contents = (u8 *)malloc(Parser->Size);
        Read(Executable, Parser->Contents, Parser->Size);
    }
    else
    {
        if (ArgCount < 2)
        {
            PrintUsageAndExit(&Program);
        }
        s32 ArgIndex = 1;
        do
        {
            char *Option = Args[ArgIndex++];
            if (Equals(Option, "-h") || Equals(Option, "--help"))
            {
                PrintUsageAndExit(&Program);
            }
            else if (Equals(Option, "-x"))
            {
                Program.CreateExecutable = 1;
            }
            else if (Equals(Option, "-o"))
            {
                char *Value = 0;
                if (ArgIndex < ArgCount)
                {
                    Value = Args[ArgIndex++];
                    if (Value[0] == '-')
                    {
                        ArgIndex--;
                        Value = 0;
                    }
                }
                if (Value)
                {
                    Program.NewExecutablePath = Value;
                }
                else
                {
                    Warn("-o option requires an <out> value");
                }
            }
            else if (Option[0] == '-')
            {
                Warn("Ignoring unknown option %s", Option);
            }
            else if (!Program.SourcePath)
            {
                Program.SourcePath = Option;
            }
            else
            {
                Warn("Too many positional parameters encountered at %s", Option);
            }
        } while (ArgIndex < ArgCount);

        if (!Program.SourcePath)
        {
            PrintUsageAndExit(&Program);
        }
        int SourceCodeFile = Open(Program.SourcePath, O_RDONLY);
        if (SourceCodeFile < 0)
        {
            Panic("Cannot open source code file %s", Program.SourcePath);
        }

        if (Program.CreateExecutable)
        {
            char RelativeExecutableName[256];
            char *NewExecutablePath;
            if (Program.NewExecutablePath)
            {
                NewExecutablePath = Program.NewExecutablePath;
            }
            else
            {
                NewExecutablePath = RelativeExecutableName;
                char *Dest = RelativeExecutableName;
                char *End = Dest + sizeof RelativeExecutableName - 1;
                char *Source = Program.SourcePath;
                do
                {
                    if (*Source == '/')
                    {
                        Source++;
                        Dest = RelativeExecutableName;
                    }
                    *Dest++ = *Source++;
                } while (Dest < End && *Source && *Source != '.');
                *Dest = 0;
            }
            int NewExecutable = Create(NewExecutablePath, 0);
            if (NewExecutable < 0)
            {
                Panic("Cannot open file %s for writing", NewExecutablePath);
            }

            u8 Buffer[1024*1024];
            size_t BytesRemaining = Program.DataOffset;
            size_t BytesRead, BytesWritten;
            Seek(Executable, 0, SEEK_SET);
            do
            {
                BytesRead = Read(Executable, Buffer, sizeof Buffer);
                BytesWritten = Write(NewExecutable, Buffer, Min(BytesRead, BytesRemaining));
                BytesRemaining -= BytesWritten;
            } while (BytesRemaining);
            do
            {
                BytesRead = Read(SourceCodeFile, Buffer, sizeof Buffer);
                Write(NewExecutable, Buffer, BytesRead);
            } while (BytesRead);
            CbasFooter.MagicNumber = CbasMagicNumber;
            CbasFooter.Version = 1;
            CbasFooter.DataOffset = Program.DataOffset;

            Write(NewExecutable, &CbasFooter, sizeof CbasFooter);
            ChangeMode(NewExecutable, 0755);
            Close(NewExecutable);
            return 0;
        }
        else
        {
            if (Program.NewExecutablePath)
            {
                Warn("-o option is only used with -x");
            }
            Parser->Size = Seek(SourceCodeFile, 0, SEEK_END);
            Seek(SourceCodeFile, 0, SEEK_SET);
            Parser->Contents = (u8 *)malloc(Parser->Size);
            Read(SourceCodeFile, Parser->Contents, Parser->Size);
        }
        Close(SourceCodeFile);
    }
    Close(Executable);

    Lex(Parser);
    lexeme EntryPoint = StatementList(Parser);
    Evaluate(&Environment, EntryPoint.Right);
    return 0;
}

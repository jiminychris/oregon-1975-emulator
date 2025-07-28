#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <unistd.h>

#include "primitives.h"

#define RUN_TESTS 1
#define PRINT_SUCCESSFUL_TESTS 0
#define STOP_AFTER_TESTS 0
#define DEBUG_LEXER 0
#define DEBUG_STATEMENT 0
#define DEBUG_EVALUATE_STATEMENT 0
#define DEBUG_EVAL_IF 0
#define DEBUG_STRING_INPUT 0

#define ArrayCount(Array) (sizeof(Array) / sizeof(Array[0]))
#define Assert(Expr) {if(!(Expr)) int __AssertInt = *((volatile int *)0);}

s32 Min(s32 A, s32 B)
{
    return A < B ? A : B;
}

s32 Max(s32 A, s32 B)
{
    return A > B ? A : B;
}

struct string_reference
{
    u32 Length;
    char *Memory;
};

#define STRING_REFERENCE(StringLiteral) {sizeof (StringLiteral) - 1, (char*)(StringLiteral)}

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
#define CREATE_STRING(Prefix, Name, Precedence) #Name,
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

const char *TokenTypeNames[] =
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

const char *BasicCommandNames[] =
{
    BASIC_COMMANDS(CREATE_STRING)
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

void DebugLexer(parser *Parser)
{
    for (s32 LexIndex = 0; LexIndex < Parser->LexemeCount; ++LexIndex)
    {
        lexeme *Lexeme = Parser->Lexemes + LexIndex;
        printf("%s ", TokenTypeNames[Lexeme->Type]);
        switch (Lexeme->Type)
        {
            case token_type_REM:
            {
                printf("%.*s", Lexeme->String.Length, Lexeme->String.Memory);
            } break;
            case token_type_INTEGER:
            {
                printf("%d", Lexeme->Integer);
            } break;
            case token_type_REAL:
            {
                printf("%f", Lexeme->Real);
            } break;
            case token_type_CHAR:
            {
                printf("%d", Lexeme->Character);
            } break;
            case token_type_STRING:
            {
                printf("%.*s", Lexeme->String.Length, Lexeme->String.Memory);
            } break;
            case token_type_ID:
            {
                if (Lexeme->IsString)
                {
                    printf("%.*s$", Lexeme->String.Length, Lexeme->String.Memory);
                }
                else
                {
                    printf("%.*s", Lexeme->String.Length, Lexeme->String.Memory);
                }
            } break;
            case token_type_UNKNOWN:
            {
                if (Lexeme->String.Memory)
                {
                    printf("%.*s", Lexeme->String.Length, Lexeme->String.Memory);
                }
                else
                {
                    printf("%c", Lexeme->Character);
                }
            } break;
            default:
            {
            } break;
        }
        printf("\n");
    }
}

u8 IsNumber(token_type Type)
{
    return Type == token_type_INTEGER || Type == token_type_REAL;
}

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

lexeme *Expression(parser *Parser);
lexeme *EvaluateExpression(environment *Environment, lexeme *Expr, lexeme *Value);

lexeme LexemeEOF = {token_type_EOF};

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
    lexeme *Lexeme = 0;
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

s8 GetChar(parser *Parser)
{
    s8 Result = -1;
    if (Parser->Position < Parser->Size)
    {
        Result = Parser->Contents[Parser->Position++];
    }
    return Result;
}

s8 PeekChar(parser *Parser)
{
    s8 Result = -1;
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

void Warn(parser *Parser, const char *Format, ...)
{
    va_list args;
    va_start(args, Format);

    fprintf(stderr, "Line %u: ", Parser->SourceLineNumber);
    vfprintf(stderr, Format, args);
    fprintf(stderr, "\n");
}

void Panic(parser *Parser, const char *Format, ...)
{
    va_list args;
    va_start(args, Format);

    fprintf(stderr, "Line %u: ", Parser->SourceLineNumber);
    vfprintf(stderr, Format, args);
    fprintf(stderr, "\n");

    exit(1);
}

void Panic(lexeme *Lexeme, const char *Format, ...)
{
    va_list args;
    va_start(args, Format);

    fprintf(stderr, "Line %u: ", Lexeme->LineNumber);
    vfprintf(stderr, Format, args);
    fprintf(stderr, "\n");

    exit(1);
}

struct buffer
{
    size_t Size;
    char *Contents;
    char *At;
    char *End;
};

buffer NewBuffer(char *Contents, size_t Size)
{
    buffer Result;
    Result.Size = Size;
    Result.At = Result.Contents = Contents;
    Result.End = Contents + Size;
    return Result;
}

#define NEW_BUFFER(BufferName, BufferSize)                          \
    char __##BufferName[(BufferSize)];                              \
    buffer _##BufferName = NewBuffer(__##BufferName, (BufferSize)); \
    buffer *BufferName = &_##BufferName;

void Reset(buffer *Buffer)
{
    Buffer->At = Buffer->Contents;
}

s32 Full(buffer *Buffer)
{
    return Buffer->End <= Buffer->At;
}

s32 Equals(buffer *Buffer, const char *String)
{
    return 0 == strncmp(String, Buffer->Contents, Buffer->At - Buffer->Contents);
}
s32 Equals(string_reference A, string_reference B)
{
    return (A.Length == B.Length) && (0 == strncmp(A.Memory, B.Memory, A.Length));
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

#if 0
void ParseVariableName(parser *Parser, basic_line *Line)
{
    Line->Name = Parser->Strings[Parser->StringCount++];
    buffer _StringBuffer = NewBuffer(Line->Name, sizeof Parser->Strings[0]);
    buffer *StringBuffer = &_StringBuffer;
    while (!Full(StringBuffer) && IsUpperOrDigit(PeekChar(Parser)))
    {
        *StringBuffer->At++ = GetChar(Parser);
    }
    if (!Full(StringBuffer) && PeekChar(Parser) == '$')
    {
        Line->IsString = 1;
        *StringBuffer->At++ = GetChar(Parser);
    }
    Line->NameLength = StringBuffer->At - StringBuffer->Contents;
}
void ParseString(parser *Parser, basic_line *Line)
{
    // TODO: Escaped quotes and separators.
    char Char;
    if ((Char = GetChar(Parser)) != '"')
    {
        Panic(Parser, "Expected '\"' when parsing string instead of '%c'.", Char);
    }
    Line->String = Parser->Strings[Parser->StringCount++];
    buffer _StringBuffer = NewBuffer(Line->String, sizeof Parser->Strings[0]);
    buffer *StringBuffer = &_StringBuffer;
    while (!Full(StringBuffer) && !IsEOF(Parser) && PeekChar(Parser) != '"')
    {
        *StringBuffer->At++ = GetChar(Parser);
    }
    Line->Length = StringBuffer->At - StringBuffer->Contents;
    GetChar(Parser);
}
#endif

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

size_t StringAppend(memory_arena *Arena, char Char)
{
    size_t Result = 0;
    if (Arena->Allocated < Arena->Size)
    {
        Result = 1;
        ((char*)Arena->Memory)[Arena->Allocated++] = Char;
    }
    return Result;
}

size_t StringAppend(memory_arena *Arena, char *String)
{
    size_t Result = 0;
    char *At = String;
    while (Arena->Allocated < Arena->Size && *At)
    {
        ((char*)Arena->Memory)[Arena->Allocated++] = *At++;
        Result++;
    }
    return Result;
}

size_t StringAppend(memory_arena *Arena, string_reference String)
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

s32 StringAppend(parser *Parser, char Char)
{
    return StringAppend(&Parser->StringArena, Char);
}

size_t RealAppend(memory_arena *Arena, r32 Real)
{
    char RealString[256];
    snprintf(RealString, sizeof RealString, "%g", Real);
    return StringAppend(Arena, RealString);
}

size_t IntegerAppend(memory_arena *Arena, s32 Integer)
{
    size_t Result = 0;
    if (Integer == 0)
    {
        Result += StringAppend(Arena, '0');
    }
    else
    {
        if (Integer < 0)
        {
            Result += StringAppend(Arena, '-');
            Integer *= -1;
        }
        string_reference Digits = BeginString(Arena);
        while (Integer)
        {
            Digits.Length += StringAppend(Arena, '0' + (Integer % 10));
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

lexeme *LexCharacter(parser *Parser)
{
    lexeme *Lexeme = PreviousLexeme(Parser);
    s32 Integer = 0;
    while (isdigit(PeekChar(Parser)))
    {
        Integer = 10 * Integer + GetChar(Parser) - '0';
    }

    if (Lexeme && Lexeme->Type == token_type_STRING)
    {
        Lexeme->String.Length += StringAppend(Parser, Integer);
    }
    else if (Lexeme && Lexeme->Type == token_type_CHAR)
    {
        Lexeme->Type = token_type_STRING;
        Lexeme->String.Memory = (char*)Parser->StringArena.Memory + Parser->StringArena.Allocated;
        Lexeme->String.Length += StringAppend(Parser, Lexeme->Character);
        Lexeme->String.Length += StringAppend(Parser, Integer);
    }
    else
    {
        Lexeme = PushLexeme(Parser, token_type_CHAR);
        Lexeme->Character = Integer;
    }
    return Lexeme;
}

struct temp_buffer
{
    char Start[255];
    u8 Length;
};

u8 Full(temp_buffer *Buffer)
{
    return sizeof Buffer->Start <= Buffer->Length;
}

void Append(temp_buffer *Buffer, char Char)
{
    if (!Full(Buffer))
    {
        Buffer->Start[Buffer->Length++] = Char;
    }
}

u8 Equals(temp_buffer *Buffer, const char *String)
{
    return strncmp(Buffer->Start, String, Buffer->Length) == 0 && strlen(String) <= Buffer->Length;
}

lexeme *LexString(parser *Parser)
{
    lexeme *Lexeme = PreviousLexeme(Parser);
    if (Lexeme && Lexeme->Type == token_type_STRING)
    {
    }
    else if (Lexeme && Lexeme->Type == token_type_CHAR)
    {
        Lexeme->Type = token_type_STRING;
        Lexeme->String.Memory = (char*)Parser->StringArena.Memory + Parser->StringArena.Allocated;
        Lexeme->String.Length += StringAppend(Parser, Lexeme->Character);
    }
    else
    {
        Lexeme = PushLexeme(Parser, token_type_STRING);
        Lexeme->String.Memory = (char*)Parser->StringArena.Memory + Parser->StringArena.Allocated;
    }
    // TODO: Escaped quotes and separators.
    while (!IsEOF(Parser) && PeekChar(Parser) != '"')
    {
        Lexeme->String.Length += StringAppend(Parser, GetChar(Parser));
    }
    GetChar(Parser);
    return Lexeme;
}

lexeme *LexCommand(parser *Parser, temp_buffer *Buffer, token_type Type, const char *Name)
{
    lexeme *Lexeme = 0;
    if (Equals(Buffer, Name))
    {
        Lexeme = PushLexeme(Parser, Type);
    }
    return Lexeme;
}

#define LEX_COMMAND(Parser, Buffer, Name) LexCommand(Parser, Buffer, token_type_##Name, #Name)

lexeme *LexAlpha(parser *Parser)
{
    temp_buffer Buffer_;
    temp_buffer *Buffer = &Buffer_;
    char Char = PeekChar(Parser);
    
    while (isalpha(Char) || isdigit(Char) || Char == '_')
    {
        Append(Buffer, GetChar(Parser));
        Char = PeekChar(Parser);
    }
    lexeme *Lexeme = 0;
    if      ((Lexeme = LEX_COMMAND(Parser, Buffer, REM)))
    {
        Lexeme->String.Memory = (char*)Parser->StringArena.Memory + Parser->StringArena.Allocated;
        while (!IsEOF(Parser) && PeekChar(Parser) != '\n')
        {
            Lexeme->String.Length += StringAppend(Parser, GetChar(Parser));
        }
    }
    else if ((Lexeme = LEX_COMMAND(Parser, Buffer, PRINT))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, Buffer, LIN))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, Buffer, TAB))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, Buffer, INT))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, Buffer, RND))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, Buffer, TIM))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, Buffer, DIM))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, Buffer, INPUT))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, Buffer, IF))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, Buffer, THEN))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, Buffer, END))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, Buffer, LET))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, Buffer, DATA))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, Buffer, OF))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, Buffer, OR))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, Buffer, AND))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, Buffer, NOT))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, Buffer, ENTER))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, Buffer, GOSUB))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, Buffer, GOTO))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, Buffer, READ))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, Buffer, RESTORE))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, Buffer, RETURN))) {}
    else if ((Lexeme = LEX_COMMAND(Parser, Buffer, STOP))) {}
    else
    {
        Lexeme = PushLexeme(Parser, token_type_ID);
        Lexeme->String.Memory = (char*)Parser->StringArena.Memory + Parser->StringArena.Allocated;
        for (s32 i = 0; i < Buffer->Length; ++i)
        {
            Lexeme->String.Length += StringAppend(Parser, Buffer->Start[i]);
        }
        if (PeekChar(Parser) == '$')
        {
            Lexeme->String.Length += StringAppend(Parser, GetChar(Parser));
            Lexeme->IsString = 1;
        }
    }
    return Lexeme;
}

void Lex(parser *Parser)
{
    char Char;
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
}

lexeme *Match(parser *Parser, token_type Type)
{
    lexeme *Lexeme = GetLex(Parser);
    if (Lexeme->Type != Type)
    {
        Panic(Lexeme, "Expected %s, not %s", TokenTypeNames[Type], TokenTypeNames[Lexeme->Type]);
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
            Panic(Peek, "Invalid expression LHS %s\n", TokenTypeNames[Peek->Type]);
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
    printf("%d %.*s\n", LineNumber->Integer, Peek->String.Length, Peek->String.Memory);fflush(stdout);
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
    while (PeekLex(Parser)->Type != token_type_EOF)
    {
        lexeme *Command = Statement(Parser);
        if (Command)
        {
            PreviousStatement = PreviousStatement->Right = Command;
        }
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
                Panic(LHS, "Cannot compare values of type %s", TokenTypeNames[LHS->Type]);
            } break;
        }
    }
    else
    {
        Panic(LHS, "Cannot compare values with different types %s and %s", TokenTypeNames[LHS->Type], TokenTypeNames[RHS->Type]);
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
        Panic(Operator, "Cannot numerically operate on types %s and %s", TokenTypeNames[LHS.Type], TokenTypeNames[RHS.Type]);
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
                Output->Integer = LHS.Integer / RHS.Integer;
            } break;
            case token_type_CARET:
            {
                Output->Type = token_type_REAL;
                Output->Real = pow((r32)LHS.Integer, (r32)RHS.Integer);
            } break;
            default:
            {
                Panic(Operator, "No number operation defined for type %s", TokenTypeNames[Operator->Type]);
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
                Panic(Operator, "No number operation defined for type %s", TokenTypeNames[Operator->Type]);
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
        Panic(Operator, "Cannot compare types %s and %s", TokenTypeNames[LHS.Type], TokenTypeNames[RHS.Type]);
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
                Panic(Operator, "No comparison defined for type %s", TokenTypeNames[Operator->Type]);
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
                Panic(Operator, "No comparison defined for type %s", TokenTypeNames[Operator->Type]);
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
            Panic(Value, "Cannot convert %s to boolean", TokenTypeNames[Value->Type]);
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
            Panic(Value, "Cannot convert %s to integer", TokenTypeNames[Value->Type]);
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
                Value->String.Length += StringAppend(StringArena, '\n');
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
                Value->String.Length += StringAppend(StringArena, ' ');
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
                    Panic(Expr, "Cannot negate %s", TokenTypeNames[Value->Type]);
                } break;
            }
        } break;
        default:
        {
            Panic(Expr, "Cannot evaluate %s", TokenTypeNames[Expr->Type]);
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
        case token_type_STRING:
        {
            *String = *Value;
        } break;
        case token_type_BOOLEAN:
        {
            Temp.String.Length = StringAppend(Arena, (char*)(Value->Integer ? "true" : "false"));
            *String = Temp;
        } break;
        case token_type_REAL:
        {
            Temp.String.Length = RealAppend(Arena, Value->Real);
            *String = Temp;
        } break;
        case token_type_INTEGER:
        {
            Temp.String.Length = IntegerAppend(Arena, Value->Integer);
            *String = Temp;
        } break;
        default:
        {
            Panic(Value, "Cannot convert %s to string", TokenTypeNames[Value->Type]);
        } break;
    }
    return String;
}

lexeme *ToString(environment *Environment, lexeme *Value, lexeme *String)
{
    return ToString(&Environment->Parser.StringArena, Value, String);
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
            Result += StringAppend(Arena, '(');
        }
        switch (Expr->Type)
        {
            case token_type_PLUS:
            {
                Result += StringAppend(Arena, '+');
            } break;
            case token_type_NEWLINE:
            {
                Result += StringAppend(Arena, '@');
            } break;
            case token_type_MINUS:
            case token_type_NEGATE:
            {
                Result += StringAppend(Arena, '-');
            } break;
            case token_type_STAR:
            {
                Result += StringAppend(Arena, '*');
            } break;
            case token_type_SLASH:
            {
                Result += StringAppend(Arena, '/');
            } break;
            case token_type_INTEGER:
            {
                Result += IntegerAppend(Arena, Expr->Integer);
            } break;
            case token_type_REAL:
            {
                Result += RealAppend(Arena, Expr->Real);
            } break;
            case token_type_STRING:
            case token_type_ID:
            {
                Result += StringAppend(Arena, Expr->String);
            } break;
            case token_type_EQ:
            {
                Result += StringAppend(Arena, '=');
            } break;
            case token_type_LT:
            {
                Result += StringAppend(Arena, '<');
            } break;
            case token_type_LTE:
            {
                Result += StringAppend(Arena, '<');
                Result += StringAppend(Arena, '=');
            } break;
            case token_type_GTE:
            {
                Result += StringAppend(Arena, '>');
                Result += StringAppend(Arena, '=');
            } break;
            case token_type_GT:
            {
                Result += StringAppend(Arena, '>');
            } break;
            case token_type_CARET:
            {
                Result += StringAppend(Arena, '^');
            } break;
            default:
            {
                s32 IsUnary = IsUnaryOperator(Expr);
                s32 IsBinary = IsBinaryOperator(Expr);
                if (IsBinary)
                {
                    Result += StringAppend(Arena, ' ');
                }
                Result += StringAppend(Arena, (char*)TokenTypeNames[Expr->Type]);
                if (IsBinary || IsUnary)
                {
                    Result += StringAppend(Arena, ' ');
                }
            } break;
        }
        Parens = RightBalance;
        if (!NextExpr && !LeftBalance)
        {
            while (Parens--)
            {
                Result += StringAppend(Arena, ')');
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
    char NextPrefix, Prefix;
    Prefix = NextPrefix = 0;
    u8 SuppressNewline = 0;
    while (Args)
    {
        SuppressNewline = NextPrefix = 0;
        switch (Args->Type)
        {
            case token_type_COMMA:
            {
                Expr = Args->Left;
                SuppressNewline = NextPrefix = '\t';
            } break;
            case token_type_SEMICOLON:
            {
                Expr = Args->Left;
                SuppressNewline = NextPrefix = '\t';
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
            temporary_memory TemporaryMemory = BeginTemporaryMemory(&Environment->Parser.StringArena);
            ToString(Environment, &Value, &Value);
            if (Value.Type != token_type_STRING)
            {
                Panic(&Value, "Cannot print non-string value %s", TokenTypeNames[Value.Type]);
            }
            if (Prefix)
            {
                putchar(Prefix);
            }
            printf("%.*s", Value.String.Length, Value.String.Memory);
            EndTemporaryMemory(&Environment->Parser.StringArena, TemporaryMemory);
        }
        Prefix = NextPrefix;
        Args = Args->Right;
    }
    if (!SuppressNewline)
    {
        printf("\n");
    }
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

void StringInput(environment *Environment, lexeme *Id, r32 TimeAllowed = -1, lexeme *SecondsElapsed = 0)
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
    char Char;
    buffer Buffer = NewBuffer(Variable->String.Memory, Variable->Integer);
    r32 Elapsed = 0;
    time_t StartTime, EndTime;
    time(&StartTime);
    EndTime = StartTime;
    if (TimeAllowed < 0)
    {
        s32 BytesRead = 0;
        do
        {
            if (BytesRead > 0 && !Full(&Buffer))
            {
                *Buffer.At++ = Char;
            }
            usleep(50);
            BytesRead = read(STDIN_FILENO, &Char, 1);
        } while (Char != '\n');
        time(&EndTime);
        Elapsed = difftime(EndTime, StartTime);
    }
    else
    {
        s32 BytesRead = 0;
        do
        {
            if (BytesRead > 0 && !Full(&Buffer))
            {
                *Buffer.At++ = Char;
            }
            time(&EndTime);
            Elapsed = difftime(EndTime, StartTime);
            usleep(50);
            BytesRead = read(STDIN_FILENO, &Char, 1);
        } while (Elapsed < TimeAllowed && Char != '\n');
    }
    if (SecondsElapsed)
    {
        SecondsElapsed->Type = token_type_REAL;
        SecondsElapsed->Real = Elapsed;
    }
    
    Variable->Type = token_type_STRING;
    Variable->String.Length = Buffer.At - Buffer.Contents;
#if DEBUG_STRING_INPUT
    printf("(%d)%.*s\n", Variable->String.Length, Variable->String.Length, Variable->String.Memory);
#endif
}

void EvaluateInput(environment *Environment, lexeme *Lexeme)
{
    printf("?");
    char Char;
    if (Lexeme->IsString)
    {
        StringInput(Environment, Lexeme);
    }
    else
    {
        lexeme *Variable = LookupOrDeclareVariable(Environment, Lexeme);
        s32 Integer = 0;
        s32 Multiplier = 1;
        Char = getchar();
        while (IsIntralineWhitespace(Char))
        {
            Char = getchar();
        }
        if (Char == '-')
        {
            Multiplier = -1;
            Char = getchar();
        }
        while (Char != '\n')
        {
            if (isdigit(Char))
            {
                Integer = 10 * Integer + Char - '0';
            }
            Char = getchar();
        }
        Variable->Type = token_type_INTEGER;
        Variable->Integer = Multiplier * Integer;
    }
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
            Panic(Expr, "The alloted time must be between 1 and 255");
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
        Variable->String.Length = Min(Variable->Integer, String.String.Length);
        strncpy(Variable->String.Memory, String.String.Memory, Variable->String.Length);
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
        printf("%s\n", TokenTypeNames[Lexeme->Type]);
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
                Panic(Lexeme, "Can't evaluate %s", TokenTypeNames[Lexeme->Type]);
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

#define EPSILON 0.000001f

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

#define EXPRESSION_TEST(Environment, ExpressionString, ExpectedString, ExpectedValue) ExpressionTest(Environment, STRING_REFERENCE(ExpressionString), STRING_REFERENCE(ExpectedString), ExpectedValue)

int Test(environment *Environment)
{
    int Result = 0;
    Result += EXPRESSION_TEST(Environment, "0", "0", 0);
    Result += EXPRESSION_TEST(Environment, "1", "1", 1);
    Result += EXPRESSION_TEST(Environment, "NOT 1", "(NOT 1)", !1);
    Result += EXPRESSION_TEST(Environment, "NOT 0", "(NOT 0)", !0);
    Result += EXPRESSION_TEST(Environment, "NOT -1", "(NOT (-1))", !-1);
    Result += EXPRESSION_TEST(Environment, "0 OR 0", "(0 OR 0)", 0 || 0);
    Result += EXPRESSION_TEST(Environment, "0 OR 1", "(0 OR 1)", 0 || 1);
    Result += EXPRESSION_TEST(Environment, "1 OR 0", "(1 OR 0)", 1 || 0);
    Result += EXPRESSION_TEST(Environment, "1 OR 1", "(1 OR 1)", 1 || 1);
    Result += EXPRESSION_TEST(Environment, "0 AND 0", "(0 AND 0)", 0 && 0);
    Result += EXPRESSION_TEST(Environment, "0 AND 1", "(0 AND 1)", 0 && 1);
    Result += EXPRESSION_TEST(Environment, "1 AND 0", "(1 AND 0)", 1 && 0);
    Result += EXPRESSION_TEST(Environment, "1 AND 1", "(1 AND 1)", 1 && 1);
    Result += EXPRESSION_TEST(Environment, "1 AND 1 OR 1", "((1 AND 1) OR 1)", 1 && 1 || 1);
    Result += EXPRESSION_TEST(Environment, "1 OR 1 AND 1", "(1 OR (1 AND 1))", 1 || 1 && 1);
    Result += EXPRESSION_TEST(Environment, "1+2*3+4", "((1+(2*3))+4)", 1+2*3+4);
    Result += EXPRESSION_TEST(Environment, "1*2+3*4", "((1*2)+(3*4))", 1*2+3*4);
    Result += EXPRESSION_TEST(Environment, "1*2=3*4", "((1*2)=(3*4))", 1*2==3*4);
    Result += EXPRESSION_TEST(Environment, "4^3^2", "(4^(3^2))", pow(4, pow(3, 2)));
    Result += EXPRESSION_TEST(Environment, "2-1", "(2-1)", 2-1);
    Result += EXPRESSION_TEST(Environment, "1-2", "(1-2)", 1-2);
    Result += EXPRESSION_TEST(Environment, "-1", "(-1)", -1);
    Result += EXPRESSION_TEST(Environment, "--1", "(-(-1))", -(-1));
    Result += EXPRESSION_TEST(Environment, "-3^2", "(-(3^2))", -pow(3, 2));
    Result += EXPRESSION_TEST(Environment, "1+2*3", "(1+(2*3))", 1+2*3);
    Result += EXPRESSION_TEST(Environment, "(1+2)*3", "((1+2)*3)", (1+2)*3);
    Result += EXPRESSION_TEST(Environment, "0.5*10", "(0.5*10)", 0.5*10);
    Result += EXPRESSION_TEST(Environment, "(20/100-4)^2+72", "((((20/100)-4)^2)+72)", pow(20/100-4,2)+72);
    Result += EXPRESSION_TEST(Environment, "(20/100-4)^2+12", "((((20/100)-4)^2)+12)", pow(20/100-4,2)+12);
    Result += EXPRESSION_TEST(Environment, "((20/100-4)^2+72)/((20/100-4)^2+12)-1", "((((((20/100)-4)^2)+72)/((((20/100)-4)^2)+12))-1)", (pow(20/100-4,2)+72)/(pow(20/100-4,2)+12)-1);
    Result += EXPRESSION_TEST(Environment, "0.5*10>((20/100-4)^2+72)/((20/100-4)^2+12)-1", "((0.5*10)>((((((20/100)-4)^2)+72)/((((20/100)-4)^2)+12))-1))", 0.5*10>(pow(20/100-4,2)+72)/(pow(20/100-4,2)+12)-1);
    Result += EXPRESSION_TEST(Environment, "0.3*10>((20/100-4)^2+72)/((20/100-4)^2+12)-1", "((0.3*10)>((((((20/100)-4)^2)+72)/((((20/100)-4)^2)+12))-1))", 0.3*10>(pow(20/100-4,2)+72)/(pow(20/100-4,2)+12)-1);
    Result += EXPRESSION_TEST(Environment, "0.2*10>((20/100-4)^2+72)/((20/100-4)^2+12)-1", "((0.2*10)>((((((20/100)-4)^2)+72)/((((20/100)-4)^2)+12))-1))", 0.2*10>(pow(20/100-4,2)+72)/(pow(20/100-4,2)+12)-1);
    return Result;
}

int main(int ArgCount, char *Args[])
{
    int flags = fcntl(STDIN_FILENO, F_GETFL, 0);
    fcntl(STDIN_FILENO, F_SETFL, flags | O_NONBLOCK);
    setvbuf(stdout, NULL, _IONBF, 0);
    char StringMemory[65536];
    environment Environment;
    parser *Parser = &Environment.Parser;
    Parser->StringArena.Memory = StringMemory;
    Parser->StringArena.Size = sizeof StringMemory;
#if RUN_TESTS
    int TestResult = Test(&Environment);
#if STOP_AFTER_TESTS
    return TestResult;
#endif
    Assert(!TestResult);
#endif
    Reset(&Environment);
    {
        FILE *Executable = fopen(Args[0], "rb");
        u64 DataFileStart;
        fseek(Executable, -sizeof DataFileStart, SEEK_END);
        size_t DataFileEnd = ftell(Executable);
        fread(&DataFileStart, sizeof DataFileStart, 1, Executable);
        fseek(Executable, DataFileStart, SEEK_SET);

        Parser->Size = DataFileEnd - DataFileStart;
        Parser->Contents = (u8 *)malloc(Parser->Size);
        fread(Parser->Contents, Parser->Size, 1, Executable);
        fclose(Executable);
    }
    Lex(Parser);
#if DEBUG_LEXER
    DebugLexer(Parser);
#endif
    lexeme EntryPoint = StatementList(Parser);
    Evaluate(&Environment, EntryPoint.Right);
#if 0
    s8 Char;
    NEW_BUFFER(CommandBuffer, 16);
    basic_line *Line;
    while (!IsEOF(Parser))
    {
        Parser->SourceLineNumber += 1;
        Reset(CommandBuffer);
        SkipWhitespace(Parser);
        if (!isdigit(Char = PeekChar(Parser)))
        {
            Panic(Parser, "Expected line number. Got '%c'", Char);
        }
        u32 LineNumber = ParseInteger(Parser);
        Line = Parser->Lines + LineNumber;
        if (SkipIntralineWhitespace(Parser) < 1)
        {
            Panic(Parser, "Expected space after line number.");
        }
        while (!Full(CommandBuffer) && !IsEOF(Parser) && !IsWhitespace(PeekChar(Parser)))
        {
            *CommandBuffer->At++ = GetChar(Parser);
        }
        SkipIntralineWhitespace(Parser);
        if (CHECK_COMMAND(Line, CommandBuffer, REM))
        {
        }
        else if (CHECK_COMMAND(Line, CommandBuffer, PRINT))
        {
            Char = PeekChar(Parser);
            if (Char == '\n')
            {
                Line->Length = 1;
            }
            else if (Char == '"')
            {
                ParseString(Parser, Line);
            }
            else
            {
                Reset(CommandBuffer);
                while (!Full(CommandBuffer) && !IsEOF(Parser) && isupper(PeekChar(Parser)))
                {
                    *CommandBuffer->At++ = GetChar(Parser);
                }
                if (Equals(CommandBuffer, "LIN"))
                {
                    if (GetChar(Parser) != '(')
                    {
                        Panic(Parser, "Expected open paren after PRINT LIN");
                    }
                    Line->Length = ParseInteger(Parser);
                    if (GetChar(Parser) != ')')
                    {
                        Panic(Parser, "Expected close paren after PRINT LIN(%d", Line->Length);
                    }
                }
            }
        }
        else if (CHECK_COMMAND(Line, CommandBuffer, DIM))
        {
            if (!isupper(Char = PeekChar(Parser)))
            {
                Panic(Parser, "Expected a variable name after DIM instead of '%c'", Char);
            }
            ParseVariableName(Parser, Line);
            if ((Char = GetChar(Parser) != '['))
            {
                Panic(Parser, "Expected '[' after DIM %.*s instead of '%'", Line->NameLength, Line->Name, Char);
            }
            Line->Dim = ParseInteger(Parser);
            if (Line->Dim < 1)
            {
                Panic(Parser, "DIM must be greater than zero (%d)", Line->Dim);
            }
            if ((Char = GetChar(Parser) != ']'))
            {
                Panic(Parser, "Expected ']' after DIM %.*s[%d instead of '%'", Line->NameLength, Line->Name, Line->Dim, Char);
            }
        }
        else if (CHECK_COMMAND(Line, CommandBuffer, INPUT))
        {
            if (!isupper(Char = PeekChar(Parser)))
            {
                Panic(Parser, "Expected a variable name after INPUT instead of '%c'", Char);
            }
            ParseVariableName(Parser, Line);
        }
        else if (CHECK_COMMAND(Line, CommandBuffer, IF))
        {
            if (isupper(Char = PeekChar(Parser)))
            {
                ParseVariableName(Parser, Line);
                SkipIntralineWhitespace(Parser);
                switch ((Char = GetChar(Parser)))
                {
                    case '=':
                    {
                        Line->Operator = basic_operator_EQ;
                    } break;
                    case '>':
                    {
                        if (PeekChar(Parser) == '=')
                        {
                            GetChar(Parser);
                            Line->Operator = basic_operator_GTE;
                        }
                        else
                        {
                            Line->Operator = basic_operator_GT;
                        }
                    } break;
                    case '<':
                    {
                        if (PeekChar(Parser) == '=')
                        {
                            GetChar(Parser);
                            Line->Operator = basic_operator_LTE;
                        }
                        else
                        {
                            Line->Operator = basic_operator_LT;
                        }
                    } break;
                    default:
                    {
                        goto skip;
                    } break;
                }
                SkipIntralineWhitespace(Parser);
                if (Line->IsString)
                {
                    ParseString(Parser, Line);
                }
                else
                {
                    Line->Integer = ParseInteger(Parser);
                }
                SkipIntralineWhitespace(Parser);
                Reset(CommandBuffer);
                while (!Full(CommandBuffer) && !IsEOF(Parser) && isupper(PeekChar(Parser)))
                {
                    *CommandBuffer->At++ = GetChar(Parser);
                }
                if (Equals(CommandBuffer, "THEN"))
                {
                    SkipIntralineWhitespace(Parser);
                    Line->Goto = ParseInteger(Parser);
                }
                else
                {
#if 0
                    Panic(Parser, "Expected THEN after IF instead of %.*s", CommandBuffer->At - CommandBuffer->Contents, CommandBuffer->Contents);
#else
                    goto skip;
#endif
                }
                if (LineNumber == 1325)
                {
                    printf("%.*s %d %d\n", Line->NameLength, Line->Name, Line->Integer, Line->Goto);
                }
            }
            else
            {
#if 0
                Panic(Parser, "Expected a variable name after IF instead of '%c'", Char);
#else
                goto skip;
#endif
            }
        }
        else if (CHECK_COMMAND(Line, CommandBuffer, END))
        {
        }
        else if (CHECK_COMMAND(Line, CommandBuffer, DATA))
        {
        }
        else if (CHECK_COMMAND(Line, CommandBuffer, ENTER))
        {
        }
        else if (CHECK_COMMAND(Line, CommandBuffer, GOSUB))
        {
        }
        else if (CHECK_COMMAND(Line, CommandBuffer, GOTO))
        {
            Line->Goto = ParseInteger(Parser);
        }
        else if (CHECK_COMMAND(Line, CommandBuffer, READ))
        {
        }
        else if (CHECK_COMMAND(Line, CommandBuffer, RESTORE))
        {
        }
        else if (CHECK_COMMAND(Line, CommandBuffer, RETURN))
        {
        }
        else if (CHECK_COMMAND(Line, CommandBuffer, STOP))
        {
        }
        else
        {
            if (CHECK_COMMAND(Line, CommandBuffer, LET))
            {
            }
# if 0
            Warn(Parser, "Unrecognized command \"%.*s\"", CommandBuffer->At - CommandBuffer->Contents, CommandBuffer->Contents);
#endif
        }
    skip:
        SkipToEndOfLine(Parser);
    }
    u32 NextProgramCounter;
    while(Environment->ProgramCounter < ArrayCount(Parser->Lines))
    {
        NextProgramCounter = Environment->ProgramCounter + 1;
        basic_line *Line = Parser->Lines + Environment->ProgramCounter;
        switch (Line->Command)
        {
            case basic_command_NOOP:
            case basic_command_REM:
            {
            } break;

            case basic_command_PRINT:
            {
                if (Line->String)
                {
                    printf("%.*s\n", Line->Length, Line->String);
                }
                else
                {
                    u32 NewlineCount = Line->Length;
                    while (NewlineCount--)
                    {
                        putchar('\n');
                    }
                }
            } break;

            case basic_command_DIM:
            {
                if (0 <= LookupString(Environment, Line->NameLength, Line->Name))
                {
                    Panic(Environment, "Attempted to DIM already DIM'd string %.*s", Line->NameLength, Line->Name);
                }
                u32 StringIndex = Environment->StringCount++;
                strncpy(Environment->StringNames[StringIndex], Line->Name, Line->NameLength);
                Environment->StringSizes[StringIndex] = Line->Dim;
                Environment->StringPointers[StringIndex] = Environment->StringMemoryAt;
                Environment->StringMemoryAt += Line->Dim;
            } break;

            case basic_command_INPUT:
            {
                printf("? ");
                if (Line->IsString)
                {
                    s32 StringIndex = LookupString(Environment, Line->NameLength, Line->Name);
                    if (StringIndex < 0)
                    {
                        Panic(Environment, "Attempted to INPUT an unDIM'd string %.*s", Line->NameLength, Line->Name);
                    }
                    buffer Buffer = NewBuffer(Environment->StringMemory + Environment->StringPointers[StringIndex], Environment->StringSizes[StringIndex]);
                    memset(Buffer.Contents, 0, Buffer.Size);
                    u8 TimeUp = 0;
                    time_t StartTime, EndTime;
                    time(&StartTime);
                    while (read(stdin, &Char, 1) || (!TimeUp && Char != '\n'))
                    {
                        if (!Full(&Buffer))
                        {
                            *Buffer.At++ = Char;
                        }
                        time(&EndTime);
                        TimeUp = TimeAllowed < difftime(EndTime, StartTime)
                        usleep(50);
                    }
                }
                else
                {
                    u32 IntegerIndex = LookupInteger(Environment, Line->NameLength, Line->Name);
                    s32 *IntegerValue = Environment->IntegerMemory + IntegerIndex;
                    *IntegerValue = 0;
                    u8 TimeUp = 0;
                    time_t StartTime, EndTime;
                    time(&StartTime);
                    while (read(stdin, &Char, 1) || (!TimeUp && Char != '\n'))
                    {
                        if (isdigit(Char))
                        {
                            *IntegerValue = 10 * *IntegerValue + Char - '0';
                        }
                        time(&EndTime);
                        TimeUp = TimeAllowed < difftime(EndTime, StartTime)
                        usleep(50);
                    }
                }
            } break;

            case basic_command_IF:
            {
                if (Line->IsString)
                {
                    s32 StringIndex = LookupString(Environment, Line->NameLength, Line->Name);
                    if (StringIndex < 0)
                    {
                        Panic(Environment, "Attempted to IF on an unDIM'd string %.*s", Line->NameLength, Line->Name);
                    }
                    char *String = Environment->StringMemory + Environment->StringPointers[StringIndex];
                    u32 StringLength = Environment->StringSizes[StringIndex];
                    switch(Line->Operator)
                    {
                        case basic_operator_EQ:
                        {
                            if (0 == strncmp(String, Line->String, Min(StringLength, Line->Length)))
                            {
                                NextProgramCounter = Line->Goto;
                            }
                        } break;
                        default:
                        {
                            Panic(Environment, "Unsupported operator for IF");
                        }
                    }
                }
                else
                {
                    s32 IntegerIndex = LookupInteger(Environment, Line->NameLength, Line->Name);
                    u32 IntegerValue = Environment->IntegerMemory[IntegerIndex];
                    switch (Line->Operator)
                    {
                        case basic_operator_EQ:
                        {
                            if (IntegerValue == Line->Integer)
                            {
                                NextProgramCounter = Line->Goto;
                            }
                        } break;
                        case basic_operator_LT:
                        {
                            if (IntegerValue < Line->Integer)
                            {
                                NextProgramCounter = Line->Goto;
                            }
                        } break;
                        case basic_operator_GT:
                        {
                            if (IntegerValue > Line->Integer)
                            {
                                NextProgramCounter = Line->Goto;
                            }
                        } break;
                        case basic_operator_LTE:
                        {
                            if (IntegerValue <= Line->Integer)
                            {
                                NextProgramCounter = Line->Goto;
                            }
                        } break;
                        case basic_operator_GTE:
                        {
                            if (IntegerValue >= Line->Integer)
                            {
                                NextProgramCounter = Line->Goto;
                            }
                        } break;
                    }
                    // implement integer if
                }
            } break;

            case basic_command_GOTO:
            {
                NextProgramCounter = Line->Goto;
            } break;

            default:
            {
            } break;
        }
        printf("NextProgramCounter: %d\n", NextProgramCounter);
        Environment->ProgramCounter = NextProgramCounter;
    }
    #endif
    return 0;
}

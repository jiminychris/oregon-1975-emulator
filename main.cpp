#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <stdarg.h>
#include <string.h>

#include "primitives.h"

#define DEBUG_LEXER 0
#define DEBUG_STATEMENT 0
#define DEBUG_EVALUATE_STATEMENT 0

#define ArrayCount(Array) (sizeof(Array) / sizeof(Array[0]))

u32 Min(u32 A, u32 B)
{
    return A < B ? A : B;
}

struct string_reference
{
    u32 Length;
    char *Memory;
};

#define CREATE_ENUM(Prefix, Name) Prefix##_##Name,
#define CREATE_STRING(Prefix, Name) #Name,

#define TOKEN_TYPES(x)    \
    x(token_type, UNKNOWN) \
    x(token_type, EOF) \
    x(token_type, NEWLINE) \
    x(token_type, SEMICOLON) \
    x(token_type, COMMA) \
    x(token_type, MINUS) \
    x(token_type, PLUS) \
    x(token_type, STAR) \
    x(token_type, SLASH) \
    x(token_type, CARET) \
    x(token_type, POUND) \
    x(token_type, OPAREN) \
    x(token_type, CPAREN) \
    x(token_type, OBRACKET) \
    x(token_type, CBRACKET) \
    x(token_type, EQ) \
    x(token_type, LT) \
    x(token_type, LTE) \
    x(token_type, GT) \
    x(token_type, GTE) \
    x(token_type, OF) \
    x(token_type, OR) \
    x(token_type, AND) \
    x(token_type, INTEGER) \
    x(token_type, REAL) \
    x(token_type, CHAR) \
    x(token_type, ID) \
    x(token_type, STRING) \
    x(token_type, NOOP) \
    x(token_type, REM)       \
    x(token_type, PRINT)     \
    x(token_type, LIN)     \
    x(token_type, INT)     \
    x(token_type, RND)     \
    x(token_type, TIM)     \
    x(token_type, DIM)       \
    x(token_type, INPUT)     \
    x(token_type, IF)        \
    x(token_type, THEN)        \
    x(token_type, END)       \
    x(token_type, LET)       \
    x(token_type, DATA)      \
    x(token_type, ENTER)     \
    x(token_type, GOSUB)     \
    x(token_type, GOTO)      \
    x(token_type, READ)      \
    x(token_type, RESTORE)   \
    x(token_type, RETURN)    \
    x(token_type, CONS)    \
    x(token_type, STOP)

enum token_type
{
    TOKEN_TYPES(CREATE_ENUM)
};

const char *TokenTypeNames[] =
{
    TOKEN_TYPES(CREATE_STRING)
};

#define BASIC_COMMANDS(x) \
    x(basic_command, NOOP) \
    x(basic_command, REM)       \
    x(basic_command, PRINT)     \
    x(basic_command, DIM)       \
    x(basic_command, INPUT)     \
    x(basic_command, IF)        \
    x(basic_command, END)       \
    x(basic_command, LET)       \
    x(basic_command, DATA)      \
    x(basic_command, ENTER)     \
    x(basic_command, GOSUB)     \
    x(basic_command, GOTO)      \
    x(basic_command, READ)      \
    x(basic_command, RESTORE)   \
    x(basic_command, RETURN)    \
    x(basic_command, STOP)

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
    u32 Integer;
    char Character;
    r32 Real;
    u8 IsString;
    string_reference String;
    lexeme *Left;
    lexeme *Right;
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
    parser Parser;
};

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

u32 Full(buffer *Buffer)
{
    return Buffer->End <= Buffer->At;
}

u32 Equals(buffer *Buffer, const char *String)
{
    return 0 == strncmp(String, Buffer->Contents, Buffer->At - Buffer->Contents);
}
u32 Equals(string_reference A, string_reference B)
{
    return (A.Length == B.Length) && (0 == strncmp(A.Memory, B.Memory, A.Length));
}
#if 0
u32 CheckCommand(basic_line *Line, buffer *Buffer, const char *String, basic_command Command)
{
    u32 Result = Equals(Buffer, String);
    if (Result)
    {
        Line->Command = Command;
    }
    return Result;
}

#define CHECK_COMMAND(Line, Buffer, Command) CheckCommand(Line, Buffer, #Command, basic_command_##Command)
#endif

lexeme *LookupVariable(environment *Environment, lexeme *Lexeme)
{
    lexeme *Result = 0;
    for (s32 VariableIndex = 0; VariableIndex < Environment->VariableCount; ++VariableIndex)
    {
        if (Equals(Environment->VariableNames[VariableIndex], Lexeme->String))
        {
            Result = Environment->VariableValues + VariableIndex;
        }
    }
    if (!Result && !Lexeme->IsString)
    {
        if (ArrayCount(Environment->VariableValues) <= Environment->VariableCount)
        {
            Panic(Lexeme, "Unable to allocate a new integer.");
        }
        Environment->VariableNames[Environment->VariableCount] = Lexeme->String;
        Result = Environment->VariableValues + Environment->VariableCount;
        Environment->VariableCount += 1;
        Result->Type = token_type_INTEGER;
    }
    return Result;
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

s32 StringAppend(memory_arena *Arena, char Char)
{
    s32 Result = 0;
    if (Arena->Allocated < Arena->Size)
    {
        Result = 1;
        ((char*)Arena->Memory)[Arena->Allocated++] = Char;
    }
    return Result;
}

s32 StringAppend(parser *Parser, char Char)
{
    return StringAppend(&Parser->StringArena, Char);
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

u8 IsUnaryOperator(lexeme *Lexeme)
{
    return (
        Lexeme->Type == token_type_MINUS
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

lexeme *Expression(parser *Parser)
{
    lexeme *Result = 0;
    lexeme *Peek = PeekLex(Parser);
    if (IsUnaryOperator(Peek))
    {
        Result = GetLex(Parser);
        Result->Right = Expression(Parser);
    }
    else
    {
        switch (Peek->Type)
        {
            case token_type_STRING:
            case token_type_INTEGER:
            case token_type_REAL:
            case token_type_ID:
            {
                Result = GetLex(Parser);
            } break;
            case token_type_OPAREN:
            {
                Match(Parser, token_type_OPAREN);
                Result = Expression(Parser);
                Match(Parser, token_type_CPAREN);
            } break;
            case token_type_LIN:
            {
                Result = Lin(Parser);
            } break;
            case token_type_INT:
            {
                Result = Int(Parser);
            } break;
            case token_type_RND:
            {
                Result = Rnd(Parser);
            } break;
            case token_type_TIM:
            {
                Result = Tim(Parser);
            } break;
            default:
            {
                Panic(Peek, "Invalid expression LHS %s\n", TokenTypeNames[Peek->Type]);
            }
        }

        if (IsBinaryOperator(PeekLex(Parser)))
        {
            lexeme *LHS = Result;
            Result = GetLex(Parser);
            Result->Left = LHS;
            Result->Right = Expression(Parser);
        }
    }

    return Result;
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
    Match(Parser, token_type_POUND);
    lexeme *Node = Enter->Left = Match(Parser, token_type_ID);
    while (PeekLex(Parser)->Type == token_type_COMMA)
    {
        Match(Parser, token_type_COMMA);
        Node = Node->Right = Match(Parser, token_type_ID);
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

lexeme *ToInteger(environment *Environment, lexeme *Value, lexeme *Output)
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
        case token_type_STRING:
        case token_type_INTEGER:
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
        {
            EvaluateExpression(Environment, Expr->Left, &LHS);
            EvaluateExpression(Environment, Expr->Right, &RHS);
            if (LHS.Type != token_type_INTEGER || RHS.Type != token_type_INTEGER)
            {
                Panic(Expr, "Can't compare non-integer values %s and %s", TokenTypeNames[LHS.Type], TokenTypeNames[RHS.Type]);
            }
            Value->Type = token_type_INTEGER;
            Value->Integer = LHS.Integer >= RHS.Integer;
        } break;
        case token_type_MINUS:
        {
            LHS.Type = token_type_INTEGER;
            LHS.Integer = 0;
            if (Expr->Left)
            {
                EvaluateExpression(Environment, Expr->Left, &LHS);
            }
            EvaluateExpression(Environment, Expr->Right, &RHS);
            if (LHS.Type != token_type_INTEGER || RHS.Type != token_type_INTEGER)
            {
                Panic(Expr, "Can't subtract non-integer values %s and %s", TokenTypeNames[LHS.Type], TokenTypeNames[RHS.Type]);
            }
            Value->Type = token_type_INTEGER;
            Value->Integer = LHS.Integer - RHS.Integer;
        } break;
        default:
        {
            Panic(Expr, "Cannot evaluate %s", TokenTypeNames[Expr->Type]);
        } break;
    }
    return Value;
}

lexeme *ToString(environment *Environment, lexeme *Value, lexeme *String)
{
    switch (Value->Type)
    {
        case token_type_STRING:
        {
            *String = *Value;
        } break;
        default:
        {
            Panic(Value, "Cannot convert %s to string", TokenTypeNames[Value->Type]);
        } break;
    }
    return String;
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
    if (LookupVariable(Environment, Lexeme))
    {
        Panic(Lexeme, "Attempted to DIM already DIM'd string %.*s", Lexeme->String.Length, Lexeme->String.Memory);
    }
    u32 VariableIndex = Environment->VariableCount++;
    Environment->VariableNames[VariableIndex] = Lexeme->String;
    lexeme *Value = Environment->VariableValues + VariableIndex;
    Value->Type = token_type_STRING;
    Value->IsString = 1;
    Value->Integer = Lexeme->Integer;
    size_t BytesAllocated = AllocateString(&Environment->Parser.StringArena, Value->Integer, &Value->String.Memory);
    if (Value->Integer != BytesAllocated)
    {
        Panic(Lexeme, "Not enough memory to allocate string; allocated %d of %d bytes", BytesAllocated, Value->Integer);
    }
}

void EvaluateInput(environment *Environment, lexeme *Lexeme)
{
    printf("? ");
    lexeme *Value = LookupVariable(Environment, Lexeme);
    char Char;
    if (Lexeme->IsString)
    {
        if (!Value)
        {
            Panic(Lexeme, "Attempted to INPUT an unDIM'd string %.*s", Lexeme->String.Length, Lexeme->String.Memory);
        }
        buffer Buffer = NewBuffer(Value->String.Memory, Value->Integer);
        while ((Char = getchar()) != '\n')
        {
            if (!Full(&Buffer))
            {
                *Buffer.At++ = Char;
            }
        }
        Value->Type = token_type_STRING;
        Value->String.Length = Buffer.At - Buffer.Contents;
    }
    else
    {
        s32 Integer = 0;
        while ((Char = getchar()) != '\n')
        {
            if (isdigit(Char))
            {
                Integer = 10 * Integer + Char - '0';
            }
        }
        Value->Type = token_type_INTEGER;
        Value->Integer = Integer;
    }
}

void EvaluateIf(environment *Environment, lexeme *Lexeme)
{
    lexeme Value;
    EvaluateExpression(Environment, Lexeme->Left, &Value);
    ToInteger(Environment, &Value, &Value);
    if (Value.Integer)
    {
        Environment->Goto = Environment->Parser.Lines[Lexeme->Integer].Lexeme;
    }
}

void EvaluateGoto(environment *Environment, lexeme *Lexeme)
{
    if (Lexeme->Left)
    {
        Panic(Lexeme, "goto not fully implemented");
    }
    else
    {
        Environment->Goto = Environment->Parser.Lines[Lexeme->Integer].Lexeme;
    }
}

void EvaluateLetAssignment(environment *Environment, lexeme *Id, lexeme *Value)
{
    lexeme *Variable = LookupVariable(Environment, Id);
    if (Id->Right)
    {
        EvaluateExpression(Environment, Id->Right, Variable);
    }
    else
    {
        EvaluateLetAssignment(Environment, Id->Left, Variable);
    }
    *Value = *Variable;
}

void EvaluateLet(environment *Environment, lexeme *Lexeme)
{
    lexeme Sentinel;
    EvaluateLetAssignment(Environment, Lexeme->Left, &Sentinel);
}

void Evaluate(environment *Environment, lexeme *Lexeme)
{
    while (Lexeme)
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
            case token_type_LET:
            {
                EvaluateLet(Environment, Lexeme);
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

int main(int ArgCount, char *Args[])
{
    char StringMemory[65536];
    environment Environment = {};
    Environment.Parser.StringArena.Memory = StringMemory;
    Environment.Parser.StringArena.Size = sizeof StringMemory;
    parser *Parser = &Environment.Parser;
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
    Parser->SourceLineNumber = 1;
    Lex(Parser);
#if DEBUG_LEXER
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
                    while ((Char = getchar()) != '\n')
                    {
                        if (!Full(&Buffer))
                        {
                            *Buffer.At++ = Char;
                        }
                    }
                }
                else
                {
                    u32 IntegerIndex = LookupInteger(Environment, Line->NameLength, Line->Name);
                    s32 *IntegerValue = Environment->IntegerMemory + IntegerIndex;
                    *IntegerValue = 0;
                    while ((Char = getchar()) != '\n')
                    {
                        if (isdigit(Char))
                        {
                            *IntegerValue = 10 * *IntegerValue + Char - '0';
                        }
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

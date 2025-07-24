#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <stdarg.h>
#include <string.h>

#include "primitives.h"

enum basic_command
{
    basic_command_REM,
    basic_command_PRINT,
    basic_command_DIM,
    basic_command_INPUT,
    basic_command_IF,
    basic_command_END,
    basic_command_LET,
    basic_command_DATA,
    basic_command_ENTER,
    basic_command_GOSUB,
    basic_command_GOTO,
    basic_command_READ,
    basic_command_RESTORE,
    basic_command_RETURN,
    basic_command_STOP,
};

struct basic_line
{
    u32 LineNumber;
    basic_command Command;
};

struct parser
{
    size_t Size;
    size_t Position;
    u8 *Contents;
    basic_line Lines[4096];
    u32 LineIndex;
};

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

u32 IsWhitespace(s8 Char)
{
    return Char == ' ' || Char == '\n' || Char == '\r' || Char == '\t';
}

u32 IsIntralineWhitespace(s8 Char)
{
    return Char == ' ' || Char == '\t';
}

size_t SkipWhitespace(parser *Parser)
{
    size_t Start = Parser->Position;
    while (IsWhitespace(PeekChar(Parser)))
    {
        Parser->Position++;
    }
    return Parser->Position - Start;
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

    fprintf(stderr, "Line %u: ", Parser->LineIndex + 1);
    vfprintf(stderr, Format, args);
    fprintf(stderr, "\n");
}

void Panic(parser *Parser, const char *Format, ...)
{
    va_list args;
    va_start(args, Format);

    fprintf(stderr, "Line %u: ", Parser->LineIndex + 1);
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

#define NEW_BUFFER(BufferName, BufferSize)               \
    char __##BufferName[(BufferSize)];                   \
    buffer _##BufferName;                                \
    buffer *BufferName = &_##BufferName;                 \
    _##BufferName.Size = (BufferSize);                   \
    _##BufferName.Contents = __##BufferName;              \
    _##BufferName.At = __##BufferName;                    \
    _##BufferName.End = __##BufferName + (BufferSize);

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

int main(int ArgCount, char *Args[])
{
    parser _Parser = {};
    parser *Parser = &_Parser;
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
    s8 Char;
    u32 LineNumberBuilder;
    NEW_BUFFER(CommandBuffer, 16);
    basic_line *Line;
    while (!IsEOF(Parser))
    {
        LineNumberBuilder = 0;
        Reset(CommandBuffer);
        Line = Parser->Lines + Parser->LineIndex;
        SkipWhitespace(Parser);
        if (!isdigit(Char = PeekChar(Parser)))
        {
            Panic(Parser, "Expected line number. Got '%c'", Char);
        }
        while (isdigit(Char = GetChar(Parser)))
        {
            LineNumberBuilder = 10 * LineNumberBuilder + Char - '0';
        }
        if (SkipIntralineWhitespace(Parser) < 1)
        {
            Panic(Parser, "Expected space after line number.");
        }
        while (!Full(CommandBuffer) && !IsEOF(Parser) && !IsWhitespace(PeekChar(Parser)))
        {
            *CommandBuffer->At++ = GetChar(Parser);
        }
        if (!(CHECK_COMMAND(Line, CommandBuffer, REM) ||
              CHECK_COMMAND(Line, CommandBuffer, PRINT) ||
              CHECK_COMMAND(Line, CommandBuffer, DIM) ||
              CHECK_COMMAND(Line, CommandBuffer, INPUT) ||
              CHECK_COMMAND(Line, CommandBuffer, IF) ||
              CHECK_COMMAND(Line, CommandBuffer, END) ||
              CHECK_COMMAND(Line, CommandBuffer, LET) ||
              CHECK_COMMAND(Line, CommandBuffer, DATA) ||
              CHECK_COMMAND(Line, CommandBuffer, ENTER) ||
              CHECK_COMMAND(Line, CommandBuffer, GOSUB) ||
              CHECK_COMMAND(Line, CommandBuffer, GOTO) ||
              CHECK_COMMAND(Line, CommandBuffer, READ) ||
              CHECK_COMMAND(Line, CommandBuffer, RESTORE) ||
              CHECK_COMMAND(Line, CommandBuffer, RETURN) ||
              CHECK_COMMAND(Line, CommandBuffer, STOP)))
        {
            Warn(Parser, "Unrecognized command \"%.*s\"", CommandBuffer->At - CommandBuffer->Contents, CommandBuffer->Contents);
        }
        Line->LineNumber = LineNumberBuilder;
        printf("%d\n", Line->LineNumber);
        SkipToEndOfLine(Parser);
        Parser->LineIndex += 1;
    }
    return 0;
}

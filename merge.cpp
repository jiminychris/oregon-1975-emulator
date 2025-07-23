#include <stdio.h>
#include "primitives.h"

/*
  char Executable[SizeExe];
  char DataFile[SizeData];
  u64  DataFileOffset; // = SizeExe
 */

int main(int ArgCount, char *Args[])
{
    if (ArgCount != 3)
    {
        fputs("Expected two files to be provided.", stderr);
        return 1;
    }
    FILE *ExeFile = fopen(Args[1], "ab");
    u64 DataFileOffset = ftell(ExeFile);

    u8 Buffer[1024*1024];
    size_t BytesRead;
    FILE *DataFile = fopen(Args[2], "rb");
    do
    {
        BytesRead = fread(Buffer, 1, sizeof Buffer, DataFile);
        fwrite(Buffer, BytesRead, 1, ExeFile);
    } while (BytesRead);
    fwrite(&DataFileOffset, sizeof DataFileOffset, 1, ExeFile);
    return 0;
}

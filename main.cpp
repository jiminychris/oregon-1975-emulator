#include <stdlib.h>
#include <stdio.h>

#include "primitives.h"

int main(int ArgCount, char *Args[])
{
    FILE *Executable = fopen(Args[0], "rb");
    u64 DataFileStart;
    fseek(Executable, -sizeof DataFileStart, SEEK_END);
    size_t DataFileEnd = ftell(Executable);
    fread(&DataFileStart, sizeof DataFileStart, 1, Executable);
    size_t DataFileSize = DataFileEnd - DataFileStart;
    u8 *DataFileContents = (u8 *)malloc(DataFileSize);
    fseek(Executable, DataFileStart, SEEK_SET);
    fread(DataFileContents, DataFileSize, 1, Executable);
    fclose(Executable);
    printf("%.*s\n", (int)DataFileSize, DataFileContents);
    return 0;
}

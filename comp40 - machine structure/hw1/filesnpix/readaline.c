#ifndef READALINE_C
#define READALINE_C
#include "readaline.h"
#include <stdlib.h>
#include <stdio.h>

int countChars(FILE *inputfd) {
        //get char at current seek position
        char c = fgetc(inputfd);

        //if char is EOF character -> return
        if (c == EOF)
                return 0;

        //loop until end of line or end of file while counting each char
        int count = 0;
        while ((c != '\n') && (c != EOF)) {
                count++;
                c = fgetc(inputfd);

                //check for read errors
                if (c < 0 && !(feof(inputfd))) {
                        fprintf(stderr, "ERROR: Unable to parse "
                                        "input from file.\n");
                        exit(1);
                }
        }
        
        //check last character in line, add to count to account for byte
        //usage of the newline character
        if (c == '\n')
                count++;

        //reset file seek pointer to the location it was before
        // (at the beginning of this function call)
        fseek(inputfd, -count, SEEK_CUR);

        return count;
}

size_t readaline(FILE *inputfd, char **datapp) {
        int i, numChars; size_t sizeBytes;

        //ERROR IN SUPPLIED ARGUMENTS
        if (inputfd == NULL) {
                fprintf(stderr, "ERROR: file pointer is NULL.\n");
                exit(1);
        }

        //save number of characters present in line
        numChars = countChars(inputfd);

        //calculate number of bytes needed
        sizeBytes = sizeof(char) * numChars;

        //EOF reached condition
        if (numChars == 0) {
                *datapp = NULL;
                return 0;
        }

        //allocate memory for the exact the size needed for line
        char *line = (char *)malloc(sizeBytes);

        //ERROR IN MALLOC
        if (line == NULL) {
                fprintf(stderr, "ERROR: memory allocation has failed.\n");
                exit(1);
        }

        //loop and save each char to array
        for (i = 0; i < numChars; i++) {
               line[i] = fgetc(inputfd);
        }

        //set *datapp to point to fist character
        *datapp = line;

        return sizeBytes;
}

/*
int main(int argc, char const *argv[])
{
        for (int i = 1; i < argc; i++) {
                FILE *fp = fopen(argv[i], "rb");
                if (fp == NULL) {
                        fprintf(stderr, "%s: %s %s %s\n",
                        argv[0], "Could not open file ",
                        argv[i], "for reading");
                        exit(1);
                }
                char * dp = NULL;
                size_t sizeOf;

                for(int j = 0; j < 10; j++){
                    sizeOf = readaline(fp, &dp);
                    printf("SIZE = %zu\n", sizeOf);
                    for (unsigned i = 0; i < sizeOf/(sizeof(char)); i++){
                        printf("character: %c address: %p\n", dp[i], &dp[i]);
                    }
                    free(dp);
                }
                fclose(fp);
        }
        return EXIT_SUCCESS;
}
*/
#endif
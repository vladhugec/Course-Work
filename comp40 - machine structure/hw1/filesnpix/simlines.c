#include "readaline.h"
#include "simlinesFuncs.c"
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

int main(int argc, const char *argv[]) {
        //intermediate variables
        char *dp = NULL;
        size_t byteCount;
        Table_T table;

        //Create Hanson Table, if error->exit status failure
        TRY
                table = Table_new(0, NULL, NULL);
        EXCEPT(Mem_Failed);
                fprintf(stderr, "Error creating table\n");
                exit(EXIT_FAILURE);
        END_TRY;

        // loop through arguments and format lines
        for (int i = 1; i < argc; i++) {
                FILE *fp = fopen(argv[i], "rb");
                if (fp == NULL) {
                        fprintf(stderr, "%s: %s %s %s\n", argv[0], 
                                "Could not open file", argv[i], "for reading");
                        exit(1);
                }

                // loop through every line
                int lineCount = 0;
                do{
                        //Read line and save byte count of line
                        byteCount = readaline(fp, &dp);
                        lineCount++;

                        TRY
                                parseLine(&byteCount, 
                                        &dp, argv[i], lineCount, &table);

                        EXCEPT(EMPTY_LINE);
                        EXCEPT(Mem_Failed);
                                fprintf(stderr, "Error inserting item \
                                         into table: %d\n",lineCount);
                        END_TRY;
                        //deallocate line invariants
                        free(dp);
                }
                while(byteCount > 0);

                //deallocate file invariants
                fclose(fp);
        }
        //Print all the matches and deallocated remainder of mem
        printMatches(table, true);
        Table_map(table, vfree, NULL);
        Table_free(&table);

        return EXIT_SUCCESS; 
}


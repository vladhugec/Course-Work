#ifndef SIMLINESFUNCS_C
#define SIMLINESFUNCS_C

#include "simlines.h"

// ERROR DECLARATIONS
Except_T EMPTY_LINE = { "ERROR: line is empty" };
Except_T ALREADY_FREE = {"No LL to free"};

lInfo *mInfoStruct(const char* fileArg, int lineCount) {
        lInfo *s = malloc(sizeof(lInfo));
        s->fileArg = fileArg;
        s->lineNum = lineCount;

        return s;
}

bool isWordChar(char c) {
        if (c >= 48 && c <= 57)
                return true;
        else if (c >= 65 && c <= 90)
                return true;
        else if (c == 95)
                return true;
        else if (c >= 97 && c <= 122)
                return true;
        return false;
}

char* formatLine(char *line, size_t *numChars) {
        char *newLine;
        int initNumChars = *numChars;
        int charCount = 0;

        //check if line is empty
        if (line == NULL || numChars == 0) {
               RAISE(EMPTY_LINE);
        }

        //allocate space for new word
        newLine = malloc(initNumChars);

        //loop through line and format non word characters
        for (unsigned i = 0; i < (initNumChars/sizeof(char)); i++) {
                if (isWordChar(line[i])) {
                        newLine[charCount] = line[i];
                        charCount++;
                }
                else {
                        if (i > 0 && isWordChar(line[i-1])) {
                                newLine[charCount] = 32;
                                charCount++;
                        }
                }
        }

        if (charCount == 0) {
                free(newLine);
                RAISE(EMPTY_LINE);
        }

        newLine = realloc(newLine, (sizeof(char) * (charCount+1)));
        newLine[charCount] = '\0';

        return newLine;
}

static void vfree(const void *key, void **value, void *cl) {
        if (value == NULL || *value == NULL){
                printf("ALREADY FREE");
                return;
        }
        (void)key;
        (void)cl;
        List_T list = *value;
        List_free(&list);
        FREE(list);
}

static void popAll(void **line, void *cl) {
        (void)cl;
        List_T lineN = *line;
        List_pop(lineN, NULL);
}

static void printStruct(void **line, void *cl) {
        lInfo *lineN = *line; (void)cl;

        printf("%-20s %7d\n", lineN->fileArg, lineN->lineNum);
}

static void findAndPrintMatch(const void *str, void **llp, void *firstMatchCl) {
        List_T listNode = *llp;
        bool *firstMatch = (bool*)firstMatchCl;

        //if the length of the list is greater than 2 -> IS A MATCH
        if (List_length(listNode) >= 2) {
                if (*firstMatch)
                        *firstMatch = false;
                else
                        printf("\n");

                printf("%s\n", (char *) str);
                List_map(listNode, printStruct, NULL);
        }
        //Popall associated nodes from list
        List_map(listNode, popAll, NULL);
}
                
void printMatches(Table_T Table, bool firstMatch) {
        Table_map(Table, findAndPrintMatch, &firstMatch);
}

void parseLine(size_t *byteCount, char **dp, const char *name, int lineCount,
                Table_T *table) {

        //we can free formatted line instantly because atom has a copy
        char *formatted_line = formatLine(*dp, byteCount);
        const char *atom_line = Atom_string(formatted_line);
        free(formatted_line);

        //printf("%s\n", atom_line);
        //create struct
        lInfo *lineInfo = mInfoStruct(name, lineCount);

        //get a pointer to the head of the list that was mapped to atom_line
        //this pointer should always be null to start
        List_T head = Table_get(*table, atom_line);
        List_T tail = List_list(lineInfo, NULL);

        if (head != NULL)
                List_append(head, tail);
        else
                Table_put(*table, atom_line, tail);
}

#endif
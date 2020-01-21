#ifndef SIMLINES_H
#define SIMLINES_H
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <atom.h>
#include <table.h>
#include <list.h>
#include <string.h>
#include <assert.h>
#include <mem.h>
#include <except.h>

// struct lInfo (Line Info Data)
// Saves char* to file name
// saves int, the line number of the line
typedef struct lInfo {
        const char* fileArg;
        int lineNum;
} lInfo;

// mInfoStruct(..) (Memory File Info Struct)
// 2 Arguments: the (char*) Filename
//            and the lineCount respectivly
// Returns: pointer (lInfo*) to allocated struct
lInfo *mInfoStruct(const char* fileArg, int lineCount);

// isWordChar(char c)
// returns true if the character was defined as a word character
// Returns false otherwise
bool isWordChar(char c);

// formatLine(..)
// 2 Arguments: poiter of type char* to any line of test
//              the (size_t type) number of chars present in that line
// Returns char* to a line formatted with proper spacing of characters
char* formatLine(char *line, size_t *numChars);

// vfree(...) [HANSON ASSOCIATED MAPPING FUNCTION FROM TEXTBOOK]
// 3 Arguments: void* key taken from Hanson Table
//              void** vale also take from Hanson Table
// Frees the associated Hanson Linked List from the Table
static void vfree(const void *key, void **value, void *cl);

// popAll(...) [MAPPING FUNCTION]
// 2 Arguments: void** to a Hanson List Node taken from Hanson Table
//              void* to the closure [UNUSED: a req. for hanson mapping]
// Frees the associated List Node from the Hanson List
static void popAll(void **line, void *cl);

// printStruct(..) [MAPPING FUNCTION]
// 2 Arguments: void** to a Hanson List Node taken from Hanson Table
//              void* to the closure [UNUSED]
// Formats and prints the Filename and Line number of a matched Line
static void printStruct(void **line, void *cl);

// findAndPrintMatch(..) [MAPPING FUNCTION]
// 3 Arguments: void* [Type is char*] to line saved by Hanson Atom
//              void** to a Hanson Linked List taken from Hanson Table
//              void* to the closure (Holds: bool firstmatch needed to
//                    print correct spacing)
// Calls printStruct(..) on each Match found and each loop 
//      frees the Node mem from the Hanson List
static void findAndPrintMatch(const void *str, void **llp, void *firstMatchCl);

// printMatches(..) [From MAIN, main print FUNC]
// 2 Arguments: Table_T Table, a Hanson table
//              bool first match representing whether or not a match has been
//                       found and printed
// Calls Table_map(..) on each unique line parsed and saved int table               
void printMatches(Table_T Table, bool firstMatch);

// parseLine(..) [From MAIN, main line parsing FUNC]
// 4 Arguments: a size_t byte count of number of bytes in current line
//              char** dp, pointer to the string being parsed
//              const char* to the name of the File where the line came from
//              int lineCount, this is the line number of the line
//              Table_T* table, pointer to Hanson Table where storage happens
// Calls Table_map(..) on each unique line parsed and saved int table
void parseLine(size_t *byteCount, char **dp, const char *name, int lineCount,
                Table_T *table);

#endif
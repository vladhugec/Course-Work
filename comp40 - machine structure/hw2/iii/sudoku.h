/*   sudoku.h  
BY: Vladimir Hugec and Jamie Wiess
HW2 Comp-40     9-25-18 
*/
#include "uarray2.h"
#include <stdlib.h>
#include <stdio.h>
#include <set.h>
#include <atom.h>
#include <mem.h>
#include <math.h>
#include <stdio.h>

const int MAXDIMEN = 9;

/* general declaration for apply() function used for mapping*/
typedef void (*MAPFUNC)(int, int, UArray2_T, void*, void*);

/*
  insertPixel(...)
  Parameters: int col - the column where we are inserting
              int row - the row where we are inserting
              tableMap - the table that we are mapping
              bit - (void)
              cl - closure = NULL
  Returns: void but inserts an integer from the greymap into the UArray2
*/
extern void insertPixel(int col, int row, UArray2_T tableMap, void *bit, void *cl);

extern UArray2_T readFromStdIn();

/*
  readFromPBM(...)
  Parameters: inputFP - file pointer to our opened file
  Returns: UArray2_T returns a full 2D array with greymap data
*/
extern UArray2_T readFromPBM(FILE *inputFP);


/*
  getFile(...)
  Parameters: argc - number of files to open
              argv - file names to open
  Returns: returns an open file in FILE form
*/
extern FILE * getFile(int argc, char ** argv);


/*
  checkTable(...)
  Parameters: table - greymap 2D representation
  Returns: void wrapper function that checks sudoku rules
*/
extern void checkTable(UArray2_T table);


/*
  checkMaxIntensity(...)
  Parameters: int val
  Returns: runs EXIT FAIL if intensity is greater than 9
*/
extern void checkMaxIntensity(int val);

/*
  checkRows(...)
  Parameters: table - greymap 2D representation
              elems - set representation of the data
  Returns: runs EXIT FAIL if repeat in row
*/
extern void checkRows(UArray2_T table, Set_T *elems);

/*
  checkColumns(...)
  Parameters: table - greymap 2D representation
              elems - set representation of the data
  Returns: runs EXIT FAIL if repeat in column
*/
extern void checkColumns(UArray2_T table, Set_T *elems);


/*
  checkBoxes(...)
  Parameters: table - greymap 2D representation
              elems - set representation of the data
  Returns: runs EXIT FAIL if repeat in boxes
*/
extern void checkBoxes(UArray2_T table, Set_T *elems);

/*
  checkCurrentLoc(...)
  Parameters: width - width index of current loc
              height - height index of current loc
              uArray2 - greymap 2D representation
              pos - pointer to position
              elems - set representation of the data
  Returns: runs EXIT FAIL if there exists a like element in set
*/
extern void checkCurrentLoc(int width, int height, UArray2_T uArray2, 
                                void*pos, void*elems);

/* runs EXIT FAIL sequence */
extern void repeatFound(Set_T *elems, UArray2_T *uA2);

/* formula for converting width, height index to 3x3 box index */
extern int evaluateBoxNum(int width, int height);

/* --------------------------------- */
/* MAP FUNCTIONS FOR BOXES IN SUDOKU */
/* --------------------------------- */

extern void UArray2_map_boxes(UArray2_T table, MAPFUNC apply, void *cl);

/* --------------------------------- */
/* Hanson Set Manipulation Functions */
/* --------------------------------- */

extern void deleteAllSetMembers(Set_T *sMembers);

extern void checkSet(Set_T *sMembers, int endSize);
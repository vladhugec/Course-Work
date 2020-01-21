/*   unblackedges.h  
BY: Vladimir Hugec and Jamie Wiess
HW2 Comp-40     9-25-18 
*/

#ifndef UNBLACKEDGES_H
#define UNBLACKEDGES_H

#include <bit2.h>
#include <stdio.h>
#include <stdlib.h>
#include <pnmrdr.h>
#include <seq.h>
#include <unistd.h>
#include <assert.h>

typedef struct Bit_Coordinates {
        int col, row;
} bitLoc;

/* constant declarations */
const int WHITE = 0;
const int BLACK = 1;
const int MAX_COL_WIDTH = 35;
const char *FILE_TYPE = "P1";
const char *OUTPUTtxt = "# File output with all black edges removed";
const int DOWN = 1;
const int UP = 2;
const int LEFT = 3;
const int RIGHT = 4;

/*
  getFile(...)
  Parameters: argc - number of files to open
              argv - file names to open
  Returns: returns an open file in FILE form
*/
extern FILE *openFile(int, char**);

/*
  pbmToBit2(...)
  Parameters: File - File we are using to fill the bit2
  Returns: Bit2_T contains the file's data
*/
extern Bit2_T pbmToBit2(FILE *);

/*
  buildEdgeSet(...)
  Parameters: Bit2_T - bitmap to be mapped
              Seq_T - Sequence to fill
  Returns: void mapping wrapper
*/
extern void buildEdgeSet(Bit2_T, Seq_T);

/*
  findBlackEdgePixels(...)
  Parameters: int col - current column
              int row - current row
              int bit - bit data
              Seq_T - black pixel set
  Returns: void adds black pixels to a sequence
*/
extern void findBlackEdgePixels(int, int, int, Seq_T);

/*
  processEdgePixels(...)
  Parameters: Bit2_T - bitmap we are working with
              Seq_T - Sequence of edge pixels
  Returns: void, switches black pixels to white
*/
extern void processEdges(Bit2_T, Seq_T);

/*
  addEdgeNeighbors(...)
  Parameters: Bit2_T - bitmap we are working with
              Seq_T - Sequence of edge pixels
              bitLoc* - pointer to bit to update
  Returns: void wrapper that checks each possible neighbor
*/
extern void addEdgeNeighbors(Bit2_T, Seq_T, bitLoc*);

/*
  printStdOut(...)
  Parameters: int col - columns voided
  			  int row - rows voided
  			  Bit2_T - bitmap we are working with 
              int bit - bit to be printed
              *cl - closure NULL
  Returns: void prints bit to stdout
*/
extern void printStdOut(int col, int row, Bit2_T bitMap, int bit, void *cl);

/*
  pbmWrite(...)
  Parameters: Bit2_T - bitmap we are working with
              outputFP - FILE we are writing to
  Returns: void pbm writing function
*/
extern void pbmWrite(FILE *outputFP, Bit2_T bitMap);

/*
  insertPixel(...)
  Parameters: int col - the column where we are inserting
              int row - the row where we are inserting
              bitmap - the table that we are working with
              bit - (void)
              cl - closure = NULL
  Returns: void but inserts new data into place of previous one using pointers
*/
extern void insertPixel(int, int, Bit2_T, int, void *);

/*
  makeEdgeSet(...)
  Parameters: int col - the column where we are inserting
              int row - the row where we are inserting
              bArray2 - 2D Array we are working with
              bit - current bit
              cl - closure = NULL
  Returns: void calls findBlackEdgePixels to be added to set (Helper Function)
*/
extern void makeEdgeSet(int col, int row, Bit2_T bArray2, int, void *cl);

/*
  isEdgePixel(...)
  Parameters: int col - the column where we are inserting
              int row - the row where we are inserting
              bMapWidth - total width of map
              bMapHeight - total height of map
  Returns: returns 1 if is edge pixel, 0 if not (Helper Function)
*/
extern int isEdgePixel(int col, int row, int bMapWidth, int bMapHeight);

/*
  isEdgePixel(...)
  Parameters: int col - the column where we are inserting
              int row - the row where we are inserting
              int direc - direction rep as integer
              bMapHeight - total height of map
  Returns: helper function for checking UP DOWN LEFT RIGHT
*/
extern int validatePos(int row, int col, int direc, Bit2_T bitMap);

/*
  checkNeighbors(...)
  Parameters: int col - the column where we are inserting
              int row - the row where we are inserting
              bitmap - the table that we are working with
              int direc - direction rep as integer
              Seq_T - Sequence of edge pixels
  Returns: helper function for checking neighbors
*/
extern void checkNeighbors(int col, int row, Bit2_T bitMap, Seq_T edges,
                        int direc);
                        
/*
  setPixel(...)
  Parameters: int col - the column where we are inserting
              int row - the row where we are inserting
  Returns: returns a pixel given row and col
*/
bitLoc* setPixel(int col, int row);

#endif
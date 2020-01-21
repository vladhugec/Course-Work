/*   uarray2.h  
BY: Vladimir Hugec and Jamie Wiess
HW2 Comp-40     9-25-18 
*/
#ifndef UARRAY2_H
#define UARRAY2_H

#include "uarray2rep.h"
#include <stdbool.h>

#define UA2 UArray2_T
typedef struct UA2 *UA2;
typedef void (*MAPFUNC_PTR)(int, int, UA2, void*, void*);

/*
  UArray2_new(...)
  Parameters: int width - the width of the array
              int height - the height of the array
  Returns: and empty Bit2_T 2D array of bits
*/
extern UArray2_T UArray2_new(int width, int height, int size);

/*
  UArray2_at(...)
  Parameters: UArray2 - the array we are working with
              int col - the col location we are receiving
              int row - the row location we are receiving
  Returns: a pointer to the element at the location we wanted
*/
extern void *UArray2_at(UA2 uArray2, int col, int row);
/* Formula for converting 2D col, row to 1D map */
extern int rowColToLoc(int colWidth, int col, int row);
/*COMMENT */

/*
  UArray2_length(...)
  Parameters: UA2 uArray2
  Returns: the length of the u2Array passed in
*/
extern int UArray2_length(UA2 uArray2);
/*
  UArray2_width(...)
  Parameters: UA2 uArray2
  Returns: the width of the u2Array passed in
*/
extern int UArray2_width(UA2 uArray2);
/*
  UArray2_height(...)
  Parameters: UA2 uArray2
  Returns: the height of the u2Array passed in
*/
extern int UArray2_height(UA2 uArray2);

/*
  UArray2_size(...)
  Parameters: UA2 uArray2
  Returns: the size of a single element in u2Array passed in
*/
extern int UArray2_size(UA2 uArray2);


//UArray2 MAPPING FUNCTIONS

/*
  Bit2_map_col_major(...)
  Parameters: uArray2 - the array we are mapping
              MAPFUNC_PTR apply - the function we are applying to every element
  Returns: void but updates the col with apply function
*/
extern void UArray2_map_col_major(UA2 uArray2, MAPFUNC_PTR apply, void *cl);
/*
  Bit2_map_row_major(...)
  Parameters: uArray2 - the array we are mapping
              MAPFUNC_PTR apply - the function we are applying to every element
  Returns: void but updates the row with apply function
*/
extern void UArray2_map_row_major(UA2 uArray2, MAPFUNC_PTR apply, void *cl);


//MEMORY FUNCTIONS

/*
  UArray2_free(...)
  Parameters: UA2 uArray2 - the array we are freeing
  Returns: void but Frees the data alocated for our arrays
*/
extern void UArray2_free(UA2 *uArray2);

#undef UA2
#endif
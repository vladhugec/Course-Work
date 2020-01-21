/*   bit2.h  
BY: Vladimir Hugec and Jamie Wiess
HW2 Comp-40     9-25-18 
*/
#ifndef BIT2_H
#define BIT2_H

#include "bit2rep.h"

#define B2 Bit2_T
typedef struct B2 *B2;
typedef void (*MAPFUNC2d)(int, int, B2, int, void*);

/*
  Bit2_new(...)
  Parameters: int width - the width of the array
              int height - the height of the array
  Returns: and empty Bit2_T 2D array of bits
*/
extern Bit2_T Bit2_new(int width, int height);

/*
  Bit2_width(...)
  Parameters: B2 b2Array
  Returns: the width of the b2Array passed in
*/
extern int Bit2_width(B2 b2Array); 

/*
  Bit2_height(...)
  Parameters: B2 b2Array
  Returns: the height of the b2Array passed in
*/
extern int Bit2_height(B2 b2Array);

/*
  Bit2_put(...)
  Parameters: B2 b2Array - the array we are placing an element into
              int width - the x coordinate of the location we are inserting
              int height - the y coordinate of the location we are inserting
              int value - the element value to be inserted
  Returns: nothing but inserts the element into the array by updating pointers
*/
extern int Bit2_put(B2 b2Array, int width, int height, int value);

/*
  Bit2_get(...)
  Parameters: B2 b2Array - the array we are getting an element from
              int width - the x coordinate of the location we are getting
              int height - the y coordinate of the location we are getting
  Returns: integer value of the index we pass in
*/
extern int Bit2_get(B2 b2Array, int width, int height);

/* Formula for converting 2D col, row to 1D map */
extern int rowColToLoc(B2 b2Array, int col, int row);

/*
  Bit2_map_col_major(...)
  Parameters: B2 b2Array - the array we are mapping
              MAPFUNC_PTR apply - the function we are applying to every element
  Returns: void but updates the col with apply function
*/
extern void Bit2_map_col_major(B2 b2Array, MAPFUNC2d apply, void *cl);

/*
  Bit2_map_row_major(...)
  Parameters: B2 b2Array - the array we are mapping
              MAPFUNC_PTR apply - the function we are applying to every element
  Returns: void but updates the row with apply function
*/
extern void Bit2_map_row_major(B2 b2Array, MAPFUNC2d apply, void *cl);

/*
  Bit2_free(...)
  Parameters: B2 b2Array - the array we are freeing
  Returns: void but Frees the data alocated for our arrays
*/
extern void Bit2_free(B2 *b2Array);

#undef B2
#endif
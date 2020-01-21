/*   uarray2rep.h  
BY: Vladimir Hugec and Jamie Wiess
HW2 Comp-40     9-25-18 
*/
#ifndef UARRAY2REP_H
#define UARRAY2REP_H

#include "uarray.h"

#define UA UArray2_T

struct UA {
        int width;   /* width of 2d-array (at least 0) */
        int height;  /* height of 2d-array (at least 0) */
        UArray_T uArray; /*single UArray represents 2d array */
};

/*
  UArray2_make(...)
  Parameters: struct UA* uA
              int height - the height of the array
              int width - the width of the array
              int size - the size of a single element of the array
  Returns: void function acts as a constructor and sets variables
*/
extern void UArray2_make(struct UA* uA, int width, int height,
                        int size);
#undef UA
#endif
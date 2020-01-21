/*   bit2rep.h  
BY: Vladimir Hugec and Jamie Wiess
HW2 Comp-40     9-25-18 
*/
#ifndef BIT2REP_H
#define BIT2REP_H

#include <bit.h>

#define B2 Bit2_T

struct B2 {
        int width;   /* width of 2d-array (at least 0) */
        int height;  /* height of 2d-array (at least 0) */
        Bit_T bArray;
};

/*
  Bit2_make(...)
  Parameters: struct B2* b2array - B2 array representation
              int height - the height of the array
              int width - the width of the array
              int size - the size of a single element of the array
              void *elems - pointer to the elements themselves
  Returns: void function acts as a constructor and sets variables
*/
extern void Bit2_make(struct B2* b2array, int width, int height);

#undef B2
#endif
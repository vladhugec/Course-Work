/* 
    UM_segData.h
    
    Data Loader and unloader for UM

    Vladimir Hugec and Mason Pollack
*/

#ifndef UM_SEGDATA_H
#define UM_SEGDATA_H

#include <seq.h>

/*make_array_of_registers
  makes an array of size 4bytes*8registers = 32bytes 
  where array[i] is a register $r[i]
  RETURNS int* to malloc'd array
*/
int* make_registerArray();

/*make_main_program_sequence
  makes a Hanson Sequence which stores other sequences (representing segments)
  initialized with 1 element which will be an empty program counter
  RETURNS Seq_T created.
*/
Seq_T make_mainSeq();

/*make_a_new_program_segment
  makes a Hanson Sequence which stores 32-bit words ("instructions")
  initialized with X number of elements all set to 0
  RETURNS Seq_T created.
*/
Seq_T make_newSegment(int x);

/* 
  get the first unmapped loc in sequence
  RETURNS int loc.
*/
int get_unmapped_loc(Seq_T last_unmapped);

/* 
  unmap the location, save the location in unmappedLocs
  void
*/
void unmapAndSaveLoc(Seq_T mainSeq, Seq_T unmappedLocs, int loc);

#endif
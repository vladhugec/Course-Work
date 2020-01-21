/* 
    UM_segData.h
    
    Data Loader and unloader for UM

    Vladimir Hugec and Mason Pollack
*/

#ifndef UM_SEGDATA_H
#define UM_SEGDATA_H

#include <seq.h>
#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

/*******************************/
/*     UM DATA Container       */
/*******************************/

typedef struct UM_DATA_STRUCT {
        Seq_T zeroSeg;
        Seq_T segments;
        Seq_T unmappedSegs;
        uint32_t pCounter;
        uint32_t* registers;
} *UM_data;

/*******************************/
/*    Initialize and Free      */
/*******************************/

/* make_UM_data
Inputs: none
Returns: UM_data struct
Does: allocates memory and inititalizes variables for the UM_data struct
*/
extern UM_data make_UM_data();

/* initialize_UM_data
Inputs: pointer to a file and UM_data struct
Returns: none
Does: intitializes the values in the struct by inputting a file into the seqs
*/
extern void initialize_UM_data(FILE *fp, UM_data data);


/* UM_data_free
Inputs: Um_data struct pointer
Returns: none
Does: frees the struct
*/
extern void UM_data_free(UM_data *data);

/*******************************/
/*          Getters            */
/*******************************/

/* UM_data_get_pCounter
Inputs: Um_data struct
Returns: uint32_t
Does: returns int program counter
*/
extern uint32_t UM_data_get_pCounter(UM_data data);

/* UM_data_get_next_unmapped_loc
Inputs: Um_data struct
Returns: int
Does: returns the next unmapped location in the segments array or -1 to addhi
*/
extern uint32_t UM_data_get_next_unmapped_loc(UM_data data);

/* UM_data_get_segments_length
Inputs: Um_data struct
Returns: int
Does: returns the lengths of the seq that holds all of the segments
*/
extern int UM_data_get_segments_length(UM_data data);

/* UM_data_get_instruction
Inputs: Um_data struct, uint32, uint32
Returns: uint32
Does: returns the instruction located at m[outer_i][inner_t]
*/
extern uint32_t UM_data_get_instruction(UM_data data, 
                                uint32_t outer_i, uint32_t inner_i);

extern uint32_t UM_data_get_zeroSeg_instruction(UM_data data, uint32_t inner_i);

/* UM_data_get_seg_instruction_count
Inputs: Um_data struct, int outer_u
Returns: int
Does: returns the number of instructions in a segment
*/
extern int UM_data_get_seg_instruction_count(UM_data data, int outer_i);

/* UM_data_get_regs
Inputs: Um_data struct
Returns: uint32_t pointer to the registers
Does: returns the array of registers from the struct
*/
extern uint32_t* UM_data_get_regs(UM_data data);


/*******************************/
/*          Setters            */
/*******************************/

/* UM_data_increment_pCounter
Inputs: Um_data struct
Returns: int
Does: increments the program counter */

//extern int UM_data_increment_pCounter(UM_data data);

/*UM_data_seg_put
Inputs: Um_data struct, Seq_T, loc
Returns:none
Does: puts a segment on the sequence of segments in the location loc (or addhi)
*/
extern void UM_data_seg_put(UM_data data, Seq_T new_seg, uint32_t loc);

/*UM_data_seg_addhi
Inputs: Um_data struct, new_seg
Returns: none
Does: addhis a new segment on the seq of segments
*/
extern void UM_data_seg_addhi(UM_data data, Seq_T new_seg);


/*UM_data_set_instruction
Inputs: Um_data struct, uint32, uint32, uint32
Returns: none
Does: sets m[outer_i][inner_i] = val
*/
extern void UM_data_set_instruction(UM_data data, uint32_t outer_i, 
                                uint32_t inner_i, uint32_t val);

/*UM_data_add_unmapped_loc
Inputs: Um_data struct, uint32
Returns: none
Does: adds a location to the unmapped segs seq
*/
extern void UM_data_add_unmapped_loc(UM_data data, uint32_t loc);

/*UM_data_duplicate_and_set_seg
Inputs: Um_data struct, uint32, uint32,
Returns: none
Does: copies a segment, sets it as the 0 segment and updates pCounter
*/
extern void UM_data_duplicate_and_set_seg(UM_data data, 
                                uint32_t outer_i, uint32_t inner_i);

#endif
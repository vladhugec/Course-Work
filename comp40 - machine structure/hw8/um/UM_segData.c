/* 
    UM_segData.c
    Implementation of UM_segData.h functions

    Data Loader and unloader for UM

    Vladimir Hugec and Mason Pollack
*/

#include "UM_segData.h"

/* BITPACK PASTED IN HERE */
/* -----------------------*/


#include "except.h"
#include "assert.h"

/* 
 * What makes things hellish is that C does not define the effects of
 * a 64-bit shift on a 64-bit value, and the Intel hardware computes
 * shifts mod 64, so that a 64-bit shift has the same effect as a
 * 0-bit shift.  The obvious workaround is to define new shift functions
 * that can shift by 64 bits.
 */

Except_T Bitpack_Overflow = { "Overflow packing bits" };

static inline uint64_t shl(uint64_t word, unsigned bits)
{
        assert(bits <= 64);
        if (bits == 64)
                return 0;
        else
                return word << bits;
}

/*
 * shift R logical
 */
static inline uint64_t shr(uint64_t word, unsigned bits)
{
        assert(bits <= 64);
        if (bits == 64)
                return 0;
        else
                return word >> bits;
}

/*
 * shift R arith
 */
static inline int64_t sra(uint64_t word, unsigned bits)
{
        assert(bits <= 64);
        if (bits == 64)
                bits = 63; /* will get all copies of sign bit, 
                            * which is correct for 64
                            */
	/* Warning: following uses non-portable >> on
	   signed value...see K&R 2nd edition page 206. */
        return ((int64_t) word) >> bits; 
}

/****************************************************************/
static inline bool Bitpack_fitsu(uint64_t n, unsigned width)
{
        if (width >= 64)
                return true;
        /* thanks to Jai Karve and John Bryan  */
        /* clever shortcut instead of 2 shifts */
        return shr(n, width) == 0; 
}

/****************************************************************/
static inline uint64_t Bitpack_newu(uint64_t word, unsigned width, unsigned lsb,
                      uint64_t value)
{
        unsigned hi = lsb + width; /* one beyond the most significant bit */
        assert(hi <= 64);
        if (!Bitpack_fitsu(value, width))
                RAISE(Bitpack_Overflow);
        return shl(shr(word, hi), hi)                 /* high part */
                | shr(shl(word, 64 - lsb), 64 - lsb)  /* low part  */
                | (value << lsb);                     /* new part  */
}


/*------------------------*/

/*******************************/
/*    Initialize and Free      */
/*******************************/

/* make_registerArray
Inputs: none
Returns: uint32_t pointer
Does: makes a register array that is used for the data struct
*/
static inline uint32_t* make_registerArray()
{
    uint32_t* array = malloc(32);

    for (int i = 0; i < 7; i++) {
        array[i] = 0;
    }

    return array;
}

UM_data make_UM_data()
{
        UM_data UM = malloc(sizeof(struct UM_DATA_STRUCT));
        UM->zeroSeg = Seq_new(0);
        UM->segments = Seq_new(0);
        UM->unmappedSegs = Seq_new(0);
        UM->registers = make_registerArray();
        UM->pCounter = 0;

        return UM;
}


void printBits(uint32_t word)
{
        uint32_t one = 1;
        uint32_t temp = 0;
        fprintf(stderr, "\n");
        for (int i = 0; i < 32; i++) {
                temp = word & one;
                word = word >> 1;
                fprintf(stderr, "%d", temp);
        }
        fprintf(stderr, "\n");
        
}

void initialize_UM_data(FILE *fp, UM_data data)
{
        int count = 0; int byte;
        uint32_t instruction = 0;
        Seq_T zero_seg = Seq_new(0);

        while (EOF != (byte = fgetc(fp))) {
                instruction = Bitpack_newu(instruction, 8, 8*(3-count), byte);
                                
                if (count == 3) {
                        Seq_addhi(zero_seg, (void*)(uintptr_t)instruction);
                        instruction = 0;
                        count = 0;
                } else {
                        count++;
                }
        }

        Seq_addlo(data->segments, (void*)zero_seg);
        data->zeroSeg = zero_seg;
        data->pCounter = 0;
}

void UM_data_free(UM_data *data)
{
        for (int i = 0; i < Seq_length((*data)->segments); i++) {
                Seq_T innerSeg = Seq_get((*data)->segments, i);
                Seq_free(&innerSeg);
        }

        Seq_free(&((*data)->segments));
        Seq_free(&((*data)->unmappedSegs));
        free(((*data)->registers));
        free(*data);
}

/*******************************/
/*          Getters            */
/*******************************/

uint32_t UM_data_get_pCounter(UM_data data) {
        return data->pCounter;
}

uint32_t* UM_data_get_regs(UM_data data)
{
        return data->registers;
}

int UM_data_get_segments_length(UM_data data)
{
        return Seq_length(data->segments);
}

uint32_t UM_data_get_instruction(UM_data data, 
                                uint32_t outer_i, uint32_t inner_i)
{
        Seq_T segment = Seq_get(data->segments, outer_i);
        void* instruction = Seq_get(segment, inner_i);

        return ((uint32_t)(uintptr_t)instruction);
}

uint32_t UM_data_get_zeroSeg_instruction(UM_data data, uint32_t inner_i)
{
        void* instruction = Seq_get(data->zeroSeg, inner_i);

        return ((uint32_t)(uintptr_t)instruction);
}

int UM_data_get_seg_instruction_count(UM_data data, int outer_i)
{
        Seq_T inner_seg = Seq_get(data->segments, outer_i);
        
        return Seq_length(inner_seg);
}

uint32_t UM_data_get_next_unmapped_loc(UM_data data)
{
    if (Seq_length(data->unmappedSegs) != 0){
        void* val = Seq_remlo(data->unmappedSegs);
        return (uint32_t)(uintptr_t)val;
    }
    return ~0;
}

/*******************************/
/*          Setters            */
/*******************************/

int UM_data_increment_pCounter(UM_data data)
{
        uint32_t full = ~0;

        if (data->pCounter == full) {
                return 0;
        }
        else if (data->pCounter == (unsigned)Seq_length(data->zeroSeg)) {
                return 0;
        }
        else {
                data->pCounter += 1;
        }

        return 1;
        
}

/*put a segment on segments*/
void UM_data_seg_put(UM_data data, Seq_T new_seg, uint32_t loc)
{
        if (loc < (unsigned)Seq_length(data->segments)) {
                Seq_put(data->segments, loc, (void*)new_seg);
        }
        else {
               UM_data_seg_addhi(data, (void*)new_seg);
        }
}

void UM_data_seg_addhi(UM_data data, Seq_T new_seg)
{
        Seq_addhi(data->segments, (void*)new_seg);
}

void UM_data_duplicate_and_set_seg(UM_data data, 
                                uint32_t outer_i, uint32_t inner_i)
{
        if (outer_i != 0) {
                int segment_len = UM_data_get_seg_instruction_count(data, 
                                                                outer_i);
                Seq_T new_seg = Seq_new(segment_len);

                for (int i = 0; i < segment_len; i ++) {
                        Seq_addhi(new_seg, (void*)(uintptr_t)
                                UM_data_get_instruction(data, outer_i, i));
                }
                Seq_put(data->segments, 0 , new_seg);
                data->zeroSeg = new_seg;
        }
        data->pCounter = inner_i;
}

void UM_data_set_instruction(UM_data data, 
                        uint32_t outer_i, uint32_t inner_i, uint32_t val)
{
        Seq_T segment = Seq_get(data->segments, outer_i);
        Seq_put(segment, inner_i, (void*)(uintptr_t)val);
}

void UM_data_add_unmapped_loc(UM_data data, uint32_t loc)
{
        Seq_addhi(data->unmappedSegs, (void*)(uintptr_t)loc);
}
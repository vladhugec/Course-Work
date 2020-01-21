


#include <stdlib.h>
#include <stdio.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <assert.h>
#include <stdbool.h>

#include <seq.h>
#include <inttypes.h>
#include <string.h>

#define SIZE uint32_t

/******************************/
/*                            */
/*       -------------        */
/*      | DECLARATIONS|       */
/*       -------------        */
/*                            */
/******************************/



/*******************************/
/*     UM DATA Container       */
/*******************************/

typedef struct UM_DATA_STRUCT {
        SIZE* zeroSeg;
        SIZE sizeZeroSeg;
        Seq_T segments;
        Seq_T unmappedSegs;
        SIZE pCounter;
        SIZE* registers;
} *UM_data;

/*******************************/
/*    Initialize and Free      */
/*******************************/

/* make_UM_data
Inputs: none
Returns: UM_data struct
Does: allocates memory and inititalizes variables for the UM_data struct
*/
static inline UM_data make_UM_data();

/* initialize_UM_data
Inputs: pointer to a file and UM_data struct
Returns: none
Does: intitializes the values in the struct by inputting a file into the seqs
*/
static inline void initialize_UM_data(FILE *fp, UM_data data);


/* UM_data_free
Inputs: Um_data struct pointer
Returns: none
Does: frees the struct
*/
static inline void UM_data_free(UM_data *data);

/*******************************/
/*          Getters            */
/*******************************/

/* UM_data_get_pCounter
Inputs: Um_data struct
Returns: uint32_t
Does: returns int program counter
*/
static inline  SIZE UM_data_get_pCounter(UM_data data);

/* UM_data_get_next_unmapped_loc
Inputs: Um_data struct
Returns: int
Does: returns the next unmapped location in the segments array or -1 to addhi
*/
static inline SIZE UM_data_get_next_unmapped_loc(UM_data data);

/* UM_data_get_segments_length
Inputs: Um_data struct
Returns: int
Does: returns the lengths of the seq that holds all of the segments
*/
static inline SIZE UM_data_get_segments_length(UM_data data);

/* UM_data_get_instruction
Inputs: Um_data struct, uint32, uint32
Returns: uint32
Does: returns the instruction located at m[outer_i][inner_t]
*/
static inline SIZE UM_data_get_instruction(UM_data data, 
                                SIZE outer_i, SIZE inner_i);
                                
static inline SIZE UM_data_get_zeroSeg_instruction(UM_data data, SIZE inner_i);

static inline SIZE* UM_data_makeTOarray(UM_data data);

/* UM_data_get_seg_instruction_count
Inputs: Um_data struct, int outer_u
Returns: int
Does: returns the number of instructions in a segment
*/
static inline SIZE UM_data_get_seg_instruction_count(UM_data data,
                                                     SIZE outer_i);

/* UM_data_get_regs
Inputs: Um_data struct
Returns: uint32_t pointer to the registers
Does: returns the array of registers from the struct
*/
static inline SIZE* UM_data_get_regs(UM_data data);


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
static inline void UM_data_seg_put(UM_data data, Seq_T new_seg, SIZE loc);

/*UM_data_seg_addhi
Inputs: Um_data struct, new_seg
Returns: none
Does: addhis a new segment on the seq of segments
*/
static inline void UM_data_seg_addhi(UM_data data, Seq_T new_seg);


/*UM_data_set_instruction
Inputs: Um_data struct, uint32, uint32, uint32
Returns: none
Does: sets m[outer_i][inner_i] = val
*/
static inline void UM_data_set_instruction(UM_data data, SIZE outer_i, 
                                SIZE inner_i, SIZE val);

/*UM_data_add_unmapped_loc
Inputs: Um_data struct, uint32
Returns: none
Does: adds a location to the unmapped segs seq
*/
static inline void UM_data_add_unmapped_loc(UM_data data, SIZE loc);

/*UM_data_duplicate_and_set_seg
Inputs: Um_data struct, uint32, uint32,
Returns: none
Does: copies a segment, sets it as the 0 segment and updates pCounter
*/
static inline void UM_data_duplicate_and_set_seg(UM_data data, 
                                SIZE outer_i, SIZE inner_i);
                                

/*******************************/
/*     Static Declarations     */
/*        UM_ALU LOGIC         */
/*******************************/

typedef uint32_t Um_instruction;

typedef enum Um_opcode {
        CMOV = 0, SLOAD, SSTORE, ADD, MUL, DIV,
        NAND, HALT, ACTIVATE, INACTIVATE, OUT, IN, LOADP, LV
} Um_opcode;

typedef enum Um_register { r0 = 0, r1, r2, r3, r4, r5, r6, r7 } Um_register;



static inline void UM_ALU_cMov(UM_data data, 
                                Um_register a, Um_register b, Um_register c);
static inline void UM_ALU_SLoad(UM_data data, 
                                Um_register a, Um_register b, Um_register c);
static inline void UM_ALU_SStore(UM_data data, 
                                Um_register a, Um_register b, Um_register c);
static inline void UM_ALU_add(UM_data data, 
                                Um_register a, Um_register b, Um_register c);
static inline void UM_ALU_multiply(UM_data data, 
                                Um_register a, Um_register b, Um_register c);
static inline void UM_ALU_divide(UM_data data, 
                                Um_register a, Um_register b, Um_register c);
static inline void UM_ALU_nand(UM_data data, 
                                Um_register a, Um_register b, Um_register c);
static inline void UM_ALU_mapSeg(UM_data data, 
                                Um_register b, Um_register c);
static inline void UM_ALU_unMapSeg(UM_data data, 
                                Um_register c);
static inline void UM_ALU_output(UM_data data, Um_register c);
static inline void UM_ALU_input(UM_data data, Um_register c);
static inline void UM_ALU_loadP(UM_data data,  
                                Um_register b, Um_register c);
static inline void UM_ALU_loadV(UM_data data, 
                                        Um_register a, SIZE val);
                                        
static inline int UM_ALU_Execute(Um_instruction word, UM_data data);

/******************************/
/*                            */
/*       -------------        */
/*      |     MAIN    |       */
/*       -------------        */
/*                            */
/******************************/




int main(int argc, char const *argv[])
{
        /* VALIDATE INPUT */
        if (argc != 2) {
                perror("Error - invalid input");
                return EXIT_FAILURE;
        }
        /* check elements of file if file passes input check */
        struct stat s;
        if (stat(argv[1], &s) == -1) {
                perror("Error - invalid file");
                return EXIT_FAILURE;
        }
        else if ((s.st_size % 4) != 0) {
                perror("Error - incomplete file");
                return EXIT_FAILURE;
        }

        FILE *program = fopen(argv[1], "rb");
        UM_data data = make_UM_data();
        initialize_UM_data(program, data);

        int go = 1;
        /*int go:
                == 0 --> error occured, exit EXIT_FAILURE
                == 1 --> all good, keep going
                == 2 --> halt instruction
        */
        
        do{
                Um_instruction instruction = UM_data_get_zeroSeg_instruction(
                                              data, UM_data_get_pCounter(data));
                go = UM_ALU_Execute(instruction, data);
        }
        while (go == 1);
        
        if (go == 0) {
                UM_data_free(&data);
                fclose(program);
                return EXIT_FAILURE;
        }

        UM_data_free(&data);
        fclose(program);
    
        return EXIT_SUCCESS;
}




/******************************/
/*                            */
/*       ----------------     */
/*      | IMPLEMENTATIONS|    */
/*       ----------------     */
/*                            */
/******************************/


static inline int UM_data_increment_pCounter(UM_data data)
{
        SIZE full = ~0;

        if (data->pCounter == full) {
                return 0;
        }
        else if (data->pCounter == data->sizeZeroSeg) {
                return 0;
        }
        else {
                data->pCounter += 1;
        }

        return 1;
}

/* IMPLEMENTATION OF BITPACK COPIED FROM SUPPLIED BITPACK.c */

static inline uint64_t shl(uint64_t word, unsigned bits)
{
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
        if (bits == 64)
                bits = 63; /* will get all copies of sign bit, 
                            * which is correct for 64
                            */
        /* Warning: following uses non-portable >> on
           signed value...see K&R 2nd edition page 206. */
        return ((int64_t) word) >> bits; 
}

static inline uint64_t Bitpack_getu(uint64_t word, unsigned width, unsigned lsb)
{
        unsigned hi = lsb + width; /* one beyond the most significant bit */
        /* different type of right shift */
        return shr(shl(word, 64 - hi),
                   64 - width); 
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
        return shl(shr(word, hi), hi)                 /* high part */
                | shr(shl(word, 64 - lsb), 64 - lsb)  /* low part  */
                | (value << lsb);                     /* new part  */
}

/*******************************/
/* MAIN ALU UNIT IMPLEMENTATION*/
/*******************************/

static inline int UM_ALU_Execute(Um_instruction word, UM_data data)
{
        Um_register regC, regB, regA;
        int val;
        int opcode = Bitpack_getu(word, 4, 28);
        int go = 0;

        if (opcode != LV){
                regC = Bitpack_getu(word, 3, 0);
                regB = Bitpack_getu(word, 3, 3);
                regA = Bitpack_getu(word, 3, 6);
        } else {
                val = Bitpack_getu(word, 25, 0);
                regA = Bitpack_getu(word, 3, 25);
                UM_ALU_loadV(data, regA, val);
                return UM_data_increment_pCounter(data);
        }

        switch (opcode)
        {
                case CMOV /* Conditional Move */:
                        UM_ALU_cMov(data, regA, regB, regC);
                        go = UM_data_increment_pCounter(data);
                        break;
                case SLOAD /* Segmented Load */:
                        UM_ALU_SLoad(data, regA, regB, regC);
                        go = UM_data_increment_pCounter(data);
                        break;
                case SSTORE /* Segmented Store */:
                        UM_ALU_SStore(data, regA, regB, regC);
                        go = UM_data_increment_pCounter(data);
                        break;
                case ADD /* ADD */:
                        UM_ALU_add(data, regA, regB, regC);
                        go = UM_data_increment_pCounter(data);
                        break;
                case MUL /* MULTIPLY */:
                        UM_ALU_multiply(data, regA, regB, regC);
                        go =  UM_data_increment_pCounter(data);
                        break;
                case DIV /* DIVIDE */:
                        UM_ALU_divide(data, regA, regB, regC);
                        go =  UM_data_increment_pCounter(data);
                        break;
                case NAND /* NAND */:
                        UM_ALU_nand(data, regA, regB, regC);
                        go =  UM_data_increment_pCounter(data);
                        break;
                case HALT /* HALT */:
                        return 2;
                case ACTIVATE /* MAP SEGMENT */:
                        UM_ALU_mapSeg(data, regB, regC);
                        go =  UM_data_increment_pCounter(data);
                        break;
                case INACTIVATE /* UNMAP SEGMENT */:
                        UM_ALU_unMapSeg(data, regC);
                        go =  UM_data_increment_pCounter(data);
                        break;
                case OUT /* OUTPUT */:
                        UM_ALU_output(data, regC);
                        go = UM_data_increment_pCounter(data);
                        break;
                case IN /* INPUT */:
                        UM_ALU_input(data, regC);
                        go = UM_data_increment_pCounter(data);
                        break;
                case LOADP /* Load Program */:
                        UM_ALU_loadP(data, regB, regC);
                        return 1;
                default /* FAILURE */:
                        return 0;
        }
        return go;
}

/*******************************/
/*  Static ALU Implementations */
/*******************************/

static inline void UM_ALU_cMov(UM_data data, 
                                Um_register a, Um_register b, Um_register c) 
{
        SIZE *r = UM_data_get_regs(data);
        if (r[c] != 0){
                r[a] = r[b];
        }
}

static inline void UM_ALU_SLoad(UM_data data, 
                                Um_register a, Um_register b, Um_register c) 
{
        SIZE *r = UM_data_get_regs(data);
        
        if (r[b] == 0) {
                r[a] = UM_data_get_zeroSeg_instruction(data, r[c]);
        }
        else {
                r[a] = UM_data_get_instruction(data, r[b], r[c]);
        }       
}

static inline void UM_ALU_SStore(UM_data data, 
                                Um_register a, Um_register b, Um_register c) 
{
        SIZE *r = UM_data_get_regs(data);
        UM_data_set_instruction(data, r[a], r[b], r[c]);
}

static inline void UM_ALU_add(UM_data data, 
                                Um_register a, Um_register b, Um_register c) 
{
        /* 2 << 31 == 2^32 ... mod addition by 2^32*/
        SIZE *r = UM_data_get_regs(data);
        r[a] = ((r[b] + r[c]) % ((uint64_t)2 << 31));
}

static inline void UM_ALU_multiply(UM_data data, 
                                Um_register a, Um_register b, Um_register c) 
{
        SIZE *r = UM_data_get_regs(data);
        r[a] = ((r[b] * r[c]) % ((uint64_t)2 << 31));
}

static inline void UM_ALU_divide(UM_data data, 
                                Um_register a, Um_register b, Um_register c)
{
        SIZE *r = UM_data_get_regs(data);
        r[a] = (r[b] / r[c]);
}

static inline void UM_ALU_nand(UM_data data, 
                                Um_register a, Um_register b, Um_register c)
{
        SIZE *r = UM_data_get_regs(data);
        r[a] = ~(r[b] & r[c]);
}

static inline void UM_ALU_mapSeg(UM_data data, 
                                Um_register b, Um_register c)
{
        SIZE *r = UM_data_get_regs(data);
        Seq_T new_seg = Seq_new(0);
        /*initialize r[c] number of spaces*/
        for (SIZE i = 0; i < r[c]; i++){
                Seq_addlo(new_seg, 0);
        }

        SIZE loc = UM_data_get_next_unmapped_loc(data);
        SIZE full = ~0;

        /*above func returns all ones if seq is empty
                if seq isnt empty add at loc, else addhi */
        if (loc != full && loc != 0) {
                UM_data_seg_put(data, new_seg, loc);
                r[b] = loc;
        } else {
                UM_data_seg_addhi(data, new_seg);
                r[b] = UM_data_get_segments_length(data) - 1;
        }
}

static inline void UM_ALU_unMapSeg(UM_data data, 
                                Um_register c)
{
        SIZE *r = UM_data_get_regs(data);
        UM_data_add_unmapped_loc(data, r[c]);
}

static inline void UM_ALU_output(UM_data data, Um_register c)
{
        SIZE *r = UM_data_get_regs(data);
        assert(r[c] < 256);

        fprintf(stdout, "%c", r[c]);
}

static inline void UM_ALU_input(UM_data data, Um_register c)
{
        int in = fgetc(stdin);

        SIZE *r = UM_data_get_regs(data);
        if (in == EOF) {
                r[c] = ~0;
        } else{
                assert(in < 256);
                r[c] = in;
        }
}

static inline void UM_ALU_loadP(UM_data data,  
                                Um_register b, Um_register c)
{
        SIZE *r = UM_data_get_regs(data);
        UM_data_duplicate_and_set_seg(data, r[b], r[c]);
}

static inline void UM_ALU_loadV(UM_data data, 
                                        Um_register a, SIZE val)
{
        SIZE *r = UM_data_get_regs(data);
        r[a] = val;
}

/*******************************/
/*    Initialize and Free      */
/*******************************/

/* make_registerArray
Inputs: none
Returns: SIZE pointer
Does: makes a register array that is used for the data struct
*/
static inline SIZE* make_registerArray()
{
    SIZE* array = malloc(32);

    for (int i = 0; i < 7; i++) {
        array[i] = 0;
    }

    return array;
}

static inline UM_data make_UM_data()
{
        UM_data UM = malloc(sizeof(struct UM_DATA_STRUCT));
        UM->zeroSeg = NULL;
        UM->segments = Seq_new(0);
        UM->unmappedSegs = Seq_new(0);
        UM->registers = make_registerArray();
        UM->pCounter = 0;

        return UM;
}

static inline void initialize_UM_data(FILE *fp, UM_data data)
{
        int count = 0; int byte; int i = 0;
        SIZE instruction = 0; 
        Seq_T zero_seg = Seq_new(0);

        while (EOF != (byte = fgetc(fp))) {
                instruction = Bitpack_newu(instruction, 8, 8*(3-count), byte);
                                
                if (count == 3) {
                        Seq_addhi(zero_seg, (void*)(uintptr_t)instruction);
                        instruction = 0;
                        count = 0;
                        i++;
                } else {
                        count++;
                }
        }
        data->sizeZeroSeg = i-1;
        Seq_addlo(data->segments, (void*)zero_seg);
        data->zeroSeg = UM_data_makeTOarray(data);
        data->pCounter = 0;
}

static inline void UM_data_free(UM_data *data)
{
        for (int i = 0; i < Seq_length((*data)->segments); i++) {
                Seq_T innerSeg = Seq_get((*data)->segments, i);
                Seq_free(&innerSeg);
        }
        
        free((*data)->zeroSeg);
        Seq_free(&((*data)->segments));
        Seq_free(&((*data)->unmappedSegs));
        free(((*data)->registers));
        free(*data);
}

static inline SIZE* UM_data_makeTOarray(UM_data data)
{
        int numVals = UM_data_get_seg_instruction_count(data, 0);
        Seq_T targetSeg = Seq_get(data->segments, 0);
        
        if (data->zeroSeg != NULL) {
                free(data->zeroSeg);
        }
        //fprintf(stderr, "%s", "NULL");
        SIZE* zero_seg = malloc(sizeof(SIZE) * numVals);
        
        for (int i = 0; i < numVals; i++) {
                zero_seg[i] = ((SIZE)(uintptr_t)Seq_get(targetSeg, i));
        }
        
        return zero_seg;
}

/*******************************/
/*          Getters            */
/*******************************/

static inline SIZE UM_data_get_pCounter(UM_data data) {
        return data->pCounter;
}

static inline SIZE* UM_data_get_regs(UM_data data)
{
        return data->registers;
}

static inline SIZE UM_data_get_segments_length(UM_data data)
{
        return Seq_length(data->segments);
}

static inline SIZE UM_data_get_instruction(UM_data data, 
                                SIZE outer_i, SIZE inner_i)
{
        Seq_T segment = Seq_get(data->segments, outer_i);
        void* instruction = Seq_get(segment, inner_i);

        return ((SIZE)(uintptr_t)instruction);
}

static inline SIZE UM_data_get_zeroSeg_instruction(UM_data data, SIZE inner_i)
{
        return (data->zeroSeg)[inner_i];
}

static inline SIZE UM_data_get_seg_instruction_count(UM_data data, SIZE outer_i)
{
        Seq_T inner_seg = Seq_get(data->segments, outer_i);
        
        return Seq_length(inner_seg);
}

static inline SIZE UM_data_get_next_unmapped_loc(UM_data data)
{
    if (Seq_length(data->unmappedSegs) != 0){
        void* val = Seq_remlo(data->unmappedSegs);
        return (SIZE)(uintptr_t)val;
    }
    return ~0;
}

/*******************************/
/*          Setters            */
/*******************************/

/*put a segment on segments*/
static inline void UM_data_seg_put(UM_data data, Seq_T new_seg, SIZE loc)
{
        if (loc < (unsigned)Seq_length(data->segments)) {
                Seq_put(data->segments, loc, (void*)new_seg);
        }
        else {
               UM_data_seg_addhi(data, (void*)new_seg);
        }
}

static inline void UM_data_seg_addhi(UM_data data, Seq_T new_seg)
{
        Seq_addhi(data->segments, (void*)new_seg);
}

static inline void UM_data_duplicate_and_set_seg(UM_data data, 
                                SIZE outer_i, SIZE inner_i)
{
        //printf("in dands");
        if (outer_i != 0) {
                data->sizeZeroSeg = UM_data_get_seg_instruction_count(data, 
                                                                outer_i);
                Seq_T new_seg = Seq_new(data->sizeZeroSeg);
 
                for (SIZE i = 0; i < data->sizeZeroSeg; i++) {
                        Seq_addhi(new_seg, (void*)(uintptr_t)
                                UM_data_get_instruction(data, outer_i, i));
                }
                
                Seq_put(data->segments, 0 , new_seg);
                free(data->zeroSeg);
                data->zeroSeg = NULL;
                data->zeroSeg = UM_data_makeTOarray(data);
        }
        
        data->pCounter = inner_i;
}

static inline void UM_data_set_instruction(UM_data data, 
                        SIZE outer_i, SIZE inner_i, SIZE val)
{
        if (outer_i == 0) {
                (data->zeroSeg)[inner_i] = val;
        }
        Seq_T segment = Seq_get(data->segments, outer_i);
        Seq_put(segment, inner_i, (void*)(uintptr_t)val);
}

static inline void UM_data_add_unmapped_loc(UM_data data, SIZE loc)
{
        Seq_addhi(data->unmappedSegs, (void*)(uintptr_t)loc);
}

#undef WORD

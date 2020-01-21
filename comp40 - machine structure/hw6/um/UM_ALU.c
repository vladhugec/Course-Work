/*
        UM_ALU.c

        Universal Machine Arithmatic Logic Unit

        Performs computations on registers based on 32 bit instructions

        By: Vladimir Hugec and Mason Pollack
        11/15/18
        Comp40-HW6
*/

#include "UM_ALU.h"

/* BITPACK */

#define SIZE uint32_t

#include "except.h"
#include "assert.h"

static inline int UM_data_increment_pCounter(UM_data data)
{
        uint32_t full = ~0;

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

static inline uint64_t Bitpack_getu(uint64_t word, unsigned width, unsigned lsb)
{
        unsigned hi = lsb + width; /* one beyond the most significant bit */
        assert(hi <= 64);
        /* different type of right shift */
        return shr(shl(word, 64 - hi),
                   64 - width); 
}

/*******************************/
/*     Static Declarations     */
/*******************************/

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
                                        Um_register a, int val);

/*******************************/
/*          MAIN UNIT         */
/*******************************/

int UM_ALU_Execute(Um_instruction word, UM_data data)
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
/*     Static Implementations  */
/*******************************/

static inline void UM_ALU_cMov(UM_data data, 
                                Um_register a, Um_register b, Um_register c) 
{
        uint32_t *r = UM_data_get_regs(data);
        if (r[c] != 0){
                r[a] = r[b];
        }
}

static inline void UM_ALU_SLoad(UM_data data, 
                                Um_register a, Um_register b, Um_register c) 
{
        uint32_t *r = UM_data_get_regs(data);
        r[a] = UM_data_get_instruction(data, r[b], r[c]);
}

static inline void UM_ALU_SStore(UM_data data, 
                                Um_register a, Um_register b, Um_register c) 
{
        uint32_t *r = UM_data_get_regs(data);
        /* note */ UM_data_set_instruction(data, r[a], r[b], r[c]);
}

static inline void UM_ALU_add(UM_data data, 
                                Um_register a, Um_register b, Um_register c) 
{
        /* 2 << 31 == 2^32 ... mod addition by 2^32*/
        uint32_t *r = UM_data_get_regs(data);
        r[a] = ((r[b] + r[c]) % ((uint64_t)2 << 31));
}

static inline void UM_ALU_multiply(UM_data data, 
                                Um_register a, Um_register b, Um_register c) 
{
        uint32_t *r = UM_data_get_regs(data);
        r[a] = ((r[b] * r[c]) % ((uint64_t)2 << 31));
}

static inline void UM_ALU_divide(UM_data data, 
                                Um_register a, Um_register b, Um_register c)
{
        uint32_t *r = UM_data_get_regs(data);
        r[a] = (r[b] / r[c]);
}

static inline void UM_ALU_nand(UM_data data, 
                                Um_register a, Um_register b, Um_register c)
{
        uint32_t *r = UM_data_get_regs(data);
        r[a] = ~(r[b] & r[c]);
}

static inline void UM_ALU_mapSeg(UM_data data, 
                                Um_register b, Um_register c)
{
        uint32_t *r = UM_data_get_regs(data);
        /*initialize r[c] number of spaces*/
        SIZE* new_seg = malloc(r[c]*4);
        for (uint32_t i = 0; i < r[c]; i++){
                new_seg[i] = 0;
        }

        uint32_t loc = UM_data_get_next_unmapped_loc(data);
        uint32_t full = ~0;

        /*above func returns all ones if seq is empty
                if seq isnt empty add at loc, else addhi */
        if (loc != full && loc != 0) {
                UM_data_seg_put(data, new_seg, loc);
                r[b] = loc;
        } else {
                fprintf(stderr, "LOC IS = %u \n", loc);
                assert(1 == 0);
                //UM_data_seg_addhi(data, new_seg);
                //r[b] = UM_data_get_segments_length(data) - 1;
        }
}

static inline void UM_ALU_unMapSeg(UM_data data, 
                                Um_register c)
{
        uint32_t *r = UM_data_get_regs(data);
        UM_data_add_unmapped_loc(data, r[c]);
}

static inline void UM_ALU_output(UM_data data, Um_register c)
{
        uint32_t *r = UM_data_get_regs(data);
        assert(r[c] < 256);

        fprintf(stdout, "%c", r[c]);
}

static inline void UM_ALU_input(UM_data data, Um_register c)
{
        int in = fgetc(stdin);

        uint32_t *r = UM_data_get_regs(data);
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
        uint32_t *r = UM_data_get_regs(data);
        UM_data_duplicate_and_set_seg(data, r[b], r[c]);
}

static inline void UM_ALU_loadV(UM_data data, 
                                        Um_register a, int val)
{
        uint32_t *r = UM_data_get_regs(data);
        r[a] = val;
}

#undef SIZE
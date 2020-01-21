/* bitmanip.c
Vladimir Hugec and Jamie Weiss
Comp40 - HW4

Implemenation of bitmanip functions
*/

#include "bitmanip.h"
#include <stdio.h>
#include "except.h"
#include "assert.h"


/* return new uint64_t with the bit @ loc set to val */
uint64_t setBitAt(uint64_t word, unsigned loc, unsigned val)
{
        assert(loc < 64);
        assert(val == 1 || val == 0);

        return (word & (~(uLShift((uint64_t)1,loc)) | (uLShift((uint64_t)val, loc))));
        /* does AND between the word and a uint64 with all ones
           but the value of val @ loc */ 
}

/* return the value of the bit @ loc in word */
uint64_t UgetBitAt(uint64_t word, unsigned loc)
{
        assert(loc < 64);

        return (uRShift(word, loc)) & (uint64_t)1;
        /* does AND between the word shifted such that loc is the least
           significant bit and a uint64 with a single one at lsb */
}

/* return the value of the bit @ loc in word */
int64_t getBitAt(int64_t word, unsigned loc)
{
        assert(loc < 64);

        return (RShift(word, loc)) & (int64_t)1;
        /* does AND between the word shifted such that loc is the least
           significant bit and a uint64 with a single one at lsb */
}

/* return new uint64_t with the bit @ loc toggled
        i.e. if bit == 1 -> set to 0 and vice versa */
uint64_t UtoggleBitAt(uint64_t word, unsigned loc)
{
        assert(loc < 64);

        return (word ^ (uLShift((uint64_t)1, loc)));
        /* does XOR between the word and a uint64 shifted by loc such that 
        there is single one loc, 0^1 = 1 & 1^1 = 0 [bit@loc^1]  */

}

/* return new uint64_t with the bit @ loc toggled
        i.e. if bit == 1 -> set to 0 and vice versa */
int64_t toggleBitAt(int64_t word, unsigned loc)
{
        assert(loc < 64);

        return (word ^ (uLShift((uint64_t)1, loc)));
        /* does XOR between the word and a uint64 shifted by loc such that 
        there is single one loc, 0^1 = 1 & 1^1 = 0 [bit@loc^1]  */

}

/* return new uint64_t with the bits over the range (start, end] toggled */
uint64_t UtoggleBitsOverInterval(uint64_t word, unsigned start, unsigned end)
{
        assert(start < end);
        assert((start == 0 || start > 0) && start < 62);
        assert(end <= 64 && end > 0);

        for (unsigned i = start; i < end; i++) {
                word = toggleBitAt(word, i);
        }

        return word;
}

/* return new uint64_t with the bits over the range (start, end] toggled */
int64_t toggleBitsOverInterval(int64_t word, unsigned start, unsigned end)
{
        assert(start < end);
        assert((start == 0 || start > 0) && start < 62);
        assert(end <= 64 && end > 0);

        for (unsigned i = start; i < end; i++) {
                word = toggleBitAt(word, i);
        }

        return word;
}

/* sets bit @ loc = 0 */
uint64_t clearBitAt(uint64_t word, unsigned loc)
{       
        assert(loc < 64);

        return setBitAt(word, loc, 0);
}

/* sets bits over interval (start, end] to 0 */
uint64_t clearBitsOverInterval(uint64_t word, unsigned start, unsigned end)
{
        assert(start < end);
        assert((start == 0 || start > 0) && start < 62);
        assert(end <= 64 && end > 0);

        for (unsigned i = start; i < end; i++) {
                word = clearBitAt(word, i);
        }

        return word;
}

/* shifts unsigned word left by val, if val is max size of 64 return 0 */
uint64_t uLShift(uint64_t word, unsigned val)
{
        assert(val <= 64);

        if (val == 64) {
                return 0;
        }
        
        return (word << val);
}

/* shifts unsigned word right by val, if val is max size of 64 return 0 */
uint64_t uRShift(uint64_t word, unsigned val) 
{
        assert(val <= 64);
        
        if (val == 64) {
                return 0;
        }
        
        return (word >> val);
}

/* shifts unsigned word right by val, but if val is max size of 64 
 set val to 63 since for a signed word with a signed bit 
 this shift will be true */
int64_t RShift(uint64_t word, unsigned val)
{
        assert(val <= 64);

        if (val == 64) {
                val = 63;
        }
                        /* cast into int64_t */
        int64_t result = ((int64_t)word) >> val;

        return result;
}

/* map to each bit in uint64_t word */
void UmapAllBitsIn(uint64_t word, 
                void apply(char bit, unsigned loc, void *cl), 
                void *cl)
{
        for (int i = 63; i >= 0; i--) {
                apply(getBitAt(word, i), i, cl);
        }
}

/* map to each bit in uint64_t word */
void mapAllBitsIn(int64_t word, 
                void apply(char bit, unsigned loc, void *cl), 
                void *cl)
{
        for (int i = 63; i >= 0; i--) {
                apply(getBitAt(word, i), i, cl);
        }
}

/**************************************************/
/*              PRINTING FUNCTIONS                */
/**************************************************/

/* apply(..) function used in mapping to print the bit mapped */
void printBit(char bit, unsigned loc, void *cl)
{
        (void)loc; (void)cl;
        printf("%u", bit);
}
/* prints all the bits in a uint64_t from msb->lsb */
void UprintAllBitsIn(uint64_t word)
{
        mapAllBitsIn(word, printBit, NULL);
        printf("\n");
}

void printAllBitsIn(int64_t word)
{
        mapAllBitsIn(word, printBit, NULL);
        printf("\n");
}
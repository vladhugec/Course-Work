/*
bitpack.c
Vladimir Hugec and Jamie Weiss
Comp40 - HW4 - Arith

Implementaion of the provided bitpack.h
*/

#include <math.h>
#include "assert.h"
#include "bitpack.h"

Except_T Bitpack_Overflow = { "Overflow packing bits" };

/**************************************************/
/*           64-Bit SHIFTING FUNCTIONS            */
/**************************************************/

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
                val = 63; /* with thanks to Maddie Payne for guidence */
        }
                        /* cast into int64_t */
        int64_t result = ((int64_t)word) >> val;

        return result;
}


/**************************************************/
/*       Signed Bit Manipulation Functions        */
/**************************************************/

/* return the value of the bit @ loc in word */
int64_t getBitAt(int64_t word, unsigned loc)
{
        assert(loc < 64);

        return (RShift(word, loc)) & (int64_t)1;
        /* does AND between the word shifted such that loc is the least
           significant bit and a uint64 with a single one at lsb */
}

/* return new int64_t with the bit @ loc toggled
        i.e. if bit == 1 -> set to 0 and vice versa */
int64_t toggleBitAt(int64_t word, unsigned loc)
{
        assert(loc < 64);

        return (word ^ (uLShift((uint64_t)1, loc)));
        /* does XOR between the word and a uint64 shifted by loc such that 
        there is single one loc, 0^1 = 1 & 1^1 = 0 [bit@loc^1]  */

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

/**************************************************/
/*       Unsigned Bit Manipulation Functions      */
/**************************************************/

/* return new uint64_t with the bit @ loc set to val */
uint64_t setBitAt(uint64_t word, unsigned loc, unsigned val)
{
        assert(loc < 64);
        assert(val == 1 || val == 0);

        return (word & (~(uLShift((uint64_t)1,loc)) 
                | (uLShift((uint64_t)val, loc))));
        /* does AND between the word and a uint64 with all ones
           but the value of val @ loc */ 
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

/* sets bit @ loc = 0 (only unsigned versions of clear.. are made 
since for clearing sign is irrelevant) */
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

/**************************************************/
/*              Bitpack.h Functions               */
/**************************************************/

bool Bitpack_fitsu(uint64_t n, unsigned width)
{
        /* 64 bits is max width */
        assert(width <= 64);

        uint64_t testBound = uRShift(n, width);

        if (testBound == 0) {
                return true;
        }

        return false;
}

bool Bitpack_fitss(int64_t n, unsigned width)
{
        assert(width <= 64);

        uint64_t newN = uLShift(n, 64 - width);
        int64_t testBound = RShift(newN, 64 - width);

        if (n == testBound) {
                return true;
        }

        return false;
}

uint64_t Bitpack_getu(uint64_t word, unsigned width, unsigned lsb)
{
        /* width + lsb is size of segment, segment cant exceed max size */
        assert(width + lsb <= 64);

        uint64_t one = 1;

        /* trims up to 'lsb' 
           e.g. lsb = 2,   10101 -> 101 */
        uint64_t s_word = uRShift(word, lsb);

        /* makes width bits that are all 1's
           e.g. width = 5 -> 11111 */
        uint64_t mask = uLShift(one, width) - one;

        return (mask & s_word);
}

int64_t Bitpack_gets(uint64_t word, unsigned width, unsigned lsb)
{
        /* width + lsb is size of segment, segment cant exceed max size */
        assert(width + lsb <= 64);

        if (width == 0) {
                return 0;
        }
        /* get unsigned bits over specifed range */
        uint64_t uBits = Bitpack_getu(word, width, lsb);
        int64_t test;

        if (getBitAt(uBits, width-1) == 1) {
                test =  ~(toggleBitsOverInterval(uBits, 0, width));
        }
        else {
                uBits = UtoggleBitsOverInterval(uBits, 0, 64);
                test = ~uBits;
        }

        return test;
}

uint64_t Bitpack_newu(uint64_t word, unsigned width, unsigned lsb, 
                                                        uint64_t value)
{
        assert(width <= 64);
        assert(width + lsb <= 64);

        if(!(Bitpack_fitsu(value, width) == true)) {
                RAISE(Bitpack_Overflow);
        }

        word = clearBitsOverInterval(word, lsb, width+lsb);
        value = uLShift(value, lsb);
        
        return (word | value);
}

uint64_t Bitpack_news(uint64_t word, unsigned width, unsigned lsb, 
                                                        int64_t value)
{
        assert(width <= 64);
        assert(width + lsb <= 64);

        if(!(Bitpack_fitss(value, width) == true)) {
                RAISE(Bitpack_Overflow);
        }

        uint64_t newVal = ~value;
        newVal = UtoggleBitsOverInterval(newVal, 0, width);
        newVal = clearBitsOverInterval(newVal, width, 64);

        return Bitpack_newu(word, width, lsb, newVal);
}

extern Except_T Bitpack_Overflow;


/*
bitpack.c
Vladimir Hugec and Jamie Weiss
Comp40 - HW4 - Arith
10/14/19
*/

#include <math.h>
#include <assert.h>
#include "bitmanip.h"
#include <stdio.h>

Except_T Bitpack_Overflow = { "Overflow packing bits" };

bool Bitpack_fitsu(uint64_t n, unsigned width)
{
        /* 64 bits is max width */
        assert(width <= 64);

        uint64_t testBound = uRShift(n, width);

        printf("\n\nTHIS IS IN FITSU TEST\n");
        printf("Printing Original \n");
        printAllBitsIn(n);
        printf("Printing AFTER right shift by width\n");
        printAllBitsIn(testBound);

        if (testBound == 0) {
                return true;
        }

        return false;
}

bool Bitpack_fitss(int64_t n, unsigned width)
{
        /* 64 bits is max width */
        assert(width <= 64);

        uint64_t newN = uLShift(n, 64 - width);
        printf("\n\nTHIS IS IN FITSS TEST\n");
        printf("Printing Original \n");
        printAllBitsIn(n);
        printf("Printing AFTER left shift by 64-width\n");
        printAllBitsIn(newN);
        int64_t testBound = RShift(newN, 64 - width);
        printf("Printing AFTER RIGHT SHIFT by 64-width\n");
        printAllBitsIn(testBound);

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

        uint64_t bits = uLShift(word, 64-(width+lsb));
        int64_t newBits = RShift(bits, 64 - width);

        /* toggle the bits desired bits and return the inverted result */
        //int64_t result1 = ~(toggleBitsOverInterval(uBits, 0, width));

        printf("\n \n \n TESTING GETSSS \n");

        printf("PRINTING EXPECTED \n");
        printAllBitsIn(newBits);
        printf("PRINTING ACTUAL RESULT\n");
        printAllBitsIn(test);
        printf("\n TESTING COMPLETE \n");

        return test;
}

uint64_t Bitpack_newu(uint64_t word, unsigned width, unsigned lsb, uint64_t value)
{
        if(!(Bitpack_fitsu(value, width) == true)) {
                RAISE(Bitpack_Overflow);
        }

        /* clear the bits over the interval (lsb, width+lsb]
                this is */
        word = clearBitsOverInterval(word, lsb, width+lsb);
        value = uLShift(value, lsb);
        
        return (word | value);
}

uint64_t Bitpack_news(uint64_t word, unsigned width, unsigned lsb, int64_t value)
{
        if(!(Bitpack_fitss(value, width) == true)) {
                RAISE(Bitpack_Overflow);
        }

        uint64_t newVal = ~value;
        newVal = UtoggleBitsOverInterval(newVal, 0, width);
        newVal = clearBitsOverInterval(newVal, width, 64);

        return Bitpack_newu(word, width, lsb, newVal);
}

extern Except_T Bitpack_Overflow;


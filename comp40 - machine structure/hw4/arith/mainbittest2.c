#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "assert.h"
#include "bitmanip.h"

bool Bitpack_fitsu(uint64_t n, unsigned width)
{
        /* 64 bits is max width */
        assert(width <= 64);

        /* total number of values that 'width' bits can represent */
        uint64_t tv = pow(2,width);

        if (n < tv) {
                return true;
        }

        return false;
}

bool Bitpack_fitss(int64_t n, unsigned width)
{
        /* 64 bits is max width */
        assert(width <= 64);

        /* upper and lower bound are the range of 
        values that 'width' bits can represent */
        int64_t lowerBound = (-1) * (pow(2,width)/2);
        int64_t upperBound = (pow(2,width)/2) - 1;

        if (n >= lowerBound && n <= upperBound) {
                return true;
        }

        return false;
}

uint64_t Bitpack_getu(uint64_t word, unsigned width, unsigned lsb)
{
        /* 64 bits is max width */
        assert(width <= 64);
        /* width + lsb is size of segment, segment cant exceed max size */
        assert(width + lsb <= 64);

        /* trims up to 'lsb' 
           e.g. lsb = 2,   10101 -> 101 */
        uint64_t s_word = word >> lsb;

        /* makes width bits that are all 1's
           e.g. width = 5 -> 11111 */
        uint64_t mask = (1 << width) - 1;

        return (mask & s_word);
}

int64_t Bitpack_gets(uint64_t word, unsigned width, unsigned lsb)
{
        /* get unsigned bits over specifed range */
        int64_t uBits = Bitpack_getu(word, width, lsb);

        for (unsigned int i = 0; i < width; i++) {
                uBits ^= 1UL << i;
        }
        //uBits = toggleBitsOverInterval(uBits, 0, width);

        /* return the inverted result */
        return ~uBits;
}

int main(int argc, char const *argv[])
{
        (void)argc; (void)argv;
    //uint64_t uN1 = 10;
    uint64_t uN2 = 5;
    int64_t N1 = 1000;
    int64_t N2 = 5;
    unsigned width1 = 3;
    unsigned width2 = 55;
    printf("TEST: \n");
    bool resultU = Bitpack_fitsu(uN2, width1);
    bool result2 = Bitpack_fitss(N2, width1);
    bool result = Bitpack_fitss(N1, width2);

    printf("CORRECT = (%lu,%u) -> %i \n", uN2, width1, 1);
    printf("OUTPUT =  (%lu,%u) -> %i \n", uN2, width1, resultU);

    printf("CORRECT = (%lu,%u) -> %i \n", N2, width1, 0);
    printf("OUTPUT =  (%lu,%u) -> %i \n", N2, width1, result2);

    printf("CORRECT = (%lu,%u) -> %i \n", N1, width2, 1);
    printf("OUTPUT =  (%lu,%u) -> %i \n", N1, width2, result);

    uint64_t resultyU = Bitpack_getu(0x3f4, 6, 2);
    int64_t resulty = Bitpack_gets(0x3f4, 6, 2);

    printf("---------\n");

    printf("CORRECT = (%u,%u) -> %i \n", 6, 2, 61);
    printf("OUTPUT =  (%u,%u) -> %lu \n", 6, 2, resultyU);

    printf("CORRECT = (%u,%u) -> %i \n", 6, 2, -3);
    printf("OUTPUT =  (%u,%u) -> %lu \n", 6, 2, resulty);


    printf("END TEST\n");

    return EXIT_SUCCESS;
}



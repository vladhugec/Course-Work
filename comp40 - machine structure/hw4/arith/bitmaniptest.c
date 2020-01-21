#include "bitmanip.h"
#include "bitpack.h"
#include <stdlib.h>
#include <stdio.h>

int main(int argc, char const *argv[])
{
        (void)argc;
        (void)argv;
        uint64_t allOnes = 0x7FFFFFFFFFFFFFFF;
        unsigned loc = 4;
        unsigned val = 0;

        printf("\n TESTING SET BIT \n");
        printf("PRINTING ORIGINAL\n");
        //char bit = getBitAt(allOnes, loc);
        printAllBitsIn(allOnes);
        uint64_t result = setBitAt(allOnes, loc, val);
        char bit2 = getBitAt(result, loc);
        //printf("BIT @ loc:%u = %u\n", loc, bit);
        //printf("--- SETTING BIT @ LOC:%u = %u ---\n", loc, val);
        //printf("BIT @ loc:%u = %u\n", loc, bit2);
        printf("PRINTING RESULT\n");
        printAllBitsIn(result);
        printf("\n TESTING COMPLETE \n");


        
        uint64_t resultT = toggleBitAt(result, loc);
        char bit3 = getBitAt(resultT, loc);
        printf("BIT BEFORE TOGGLE = ");
        printf("loc:%u = %u\n", loc, bit2);
        printf("BIT AFTER TOGGLE = ");
        printf("loc:%u = %u\n", loc, bit3);

        uint64_t resultTA = toggleBitsOverInterval(allOnes, 5, 11);
        printf("BIT BEFORE TOGGLE = ALL ONES? : ");
        printf("loc:%u = %lx\n", loc, resultT);
        printf("BIT AFTER TOGGLE = ");
        printf("loc:%u = %lx\n", loc, resultTA);

        printf("\n TESTING PRINTING ALL BITS \n");
        printf("PRINTING UNTOGGLED\n");
        printAllBitsIn(resultT);
        printf("PRINTING TOGGLED\n");
        printAllBitsIn(resultTA);
        printf("\n TESTING COMPLETE \n");




        return EXIT_SUCCESS;
}
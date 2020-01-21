#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>
#include "bitmanip.h"
#include "bitpack.h"

int main(int argc, char const *argv[])
{
    (void)argc; (void)argv;
    //uint64_t uN1 = 10;
    uint64_t uN2 = 5;
    int64_t N1 = 1000;
    int64_t N2 = 5;
    unsigned width1 = 3;
    unsigned width3 = 4;
    unsigned width2 = 55;
    printf("TEST: \n");
    bool resultU = Bitpack_fitsu(uN2, width1);
    bool result2 = Bitpack_fitss(N2, width3);
    bool result = Bitpack_fitss(N1, width2);

    if (resultU == true && result2 == true && result == true){
        printf("ALLLLL AREEE TRUEEE\n\n");
    }

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
    printf("OUTPUT =  (%u,%u) -> %li \n", 6, 2, resulty);

    	uint64_t originalThing = 0x8D02D;
        uint64_t newThing      = 0x26;
        uint64_t expectedResult= 0x8D9AD;

        uint64_t resultB2B = Bitpack_newu(originalThing, 6, 6, newThing);

        printf("\n TESTING NEWU \n");
        printf("PRINTING EXPECTED\n");
        printAllBitsIn(expectedResult);
        printf("PRINTING ACTUAL RESULT\n");
        printAllBitsIn(resultB2B);
        printf("\n TESTING COMPLETE \n");

        //uint64_t originalThing2 = 0;
        //int64_t newThing2      = -3;
        uint64_t expectedResult2 = 0xF40;
        //0000000000000000000000000000000000000000000000000000000000000000
        //0000000000000000000000000000000000000000000000000000111101000000

        printf("\n TESTING NEWs \n");
        int64_t minus3 = -3;
        int64_t fifteen = -15;
        int64_t minus10 = -10;
        printf("PRINTING -3 \n");
        printAllBitsIn(minus3);
        printf("\nPRINTING -15 \n");
        printAllBitsIn(fifteen);

/*
        uint64_t resultB2B2 = Bitpack_news(originalThing2, 6, 6, newThing2);
        printf("PRINTING EXPECTED \n");
        printAllBitsIn(expectedResult2);
        printf("PRINTING ACTUAL RESULT\n");
        printAllBitsIn(resultB2B2);
        printf("\n TESTING COMPLETE \n");

        //1111111111111111111111111111111111111111111111111111111111111101 = -3

*/


	printf("\n TESTING LAST \n");

	int64_t resultyU2 = Bitpack_gets(expectedResult2, 6, 6);
	printAllBitsIn(resultyU2);
	printf("\n");
	printf("THIS IS = %li \n", resultyU2);

    uint64_t minus3at66 = Bitpack_news(0, 5, 0, fifteen);

    int64_t resultyU2g = Bitpack_gets(minus3at66, 5, 0);


    printf("\n \n \n \n TESTING NEWSSSSSSs \n");
        printf("PRINTING EXPECTED\n");
        printAllBitsIn(fifteen);
        printf("PRINTING ACTUAL RESULT\n");
        printAllBitsIn(resultyU2g);
        printf("\n TESTING COMPLETE \n");

        minus3at66 = Bitpack_news(minus3at66, 5, 6, minus10);

        resultyU2g = Bitpack_gets(minus3at66, 5, 6);


        printf("\n \n \n \n TESTING -10 \n");
        printf("PRINTING EXPECTED\n");
        printAllBitsIn(minus10);
        printf("PRINTING ACTUAL RESULT\n");
        printAllBitsIn(resultyU2g);
        printf("\n TESTING COMPLETE \n");


        if (Bitpack_getu(Bitpack_newu(originalThing, 6, 6, newThing), 6, 6) == newThing) {
            printf("\n \n \n \n SUCCESSSSSSSSSSSS unsigned \n \n ");
        }
        if (Bitpack_gets(Bitpack_news(minus3at66, 5, 0, minus10), 5, 0) == minus10) {
            printf("\n \n \n \n SUCCESSSSSSSSSSSS signed \n \n ");
        }

    printf("END TEST\n");

    return EXIT_SUCCESS;
}



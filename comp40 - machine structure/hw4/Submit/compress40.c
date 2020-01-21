/*  compress40.c

    Vladimir Hugec and Jamie Weiss
    HW4 - Arith
    
    implementation of provided compress40.h
*/

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "compress40.h"
#include "a2methods.h"
#include "arith40.h"
#include "bitpack.h"
#include "pnm.h"
#include "manipulate.h"
#include "assert.h"
#include "a2plain.h"
#include "a2blocked.h"
#include "uarray2.h"


/* reads PPM, writes compressed image */
void compress40(FILE *input)
{
        assert(input);
        A2Methods_T methods = uarray2_methods_plain;
        Pnm_ppm image = Pnm_ppmread(input, methods);
        assert(image);
        compress(image);
        Pnm_ppmfree(&image);
} 

/* reads compressed image, writes PPM */
void decompress40(FILE *input)
{
        assert(input);
        Pnm_ppm decompressed = decompress(input);
        Pnm_ppmwrite(stdout, decompressed);
        Pnm_ppmfree(&decompressed);
}
#ifndef MANIPULATE_H
#define MANIPULATE_H
/*  
manipulate.h

    Vladimir Hugec and Jamie Weiss
    COMP 40 HW4 - Arith
    
    Module for manipulating a Pnm_ppm type
*/

#include "arith40.h"
#include "pnm.h"

#define A2 A2Methods_UArray2

/***********************************/
/* Compress and Decompress Drivers */
/***********************************/

extern void compress(Pnm_ppm pixmap);

extern Pnm_ppm decompress(FILE *file);

/***********************************/
/*        Memory Manipulation      */
/***********************************/

extern void checkDimensions(Pnm_ppm pixmap);
extern void trimDimensions(Pnm_ppm pixmap, A2 pm2);
extern Pnm_ppm allocateRGBPixmap(int width, int height);
extern Pnm_ppm initDecompressedPM(FILE *file);
extern void initializeFILE(FILE *file, unsigned width, unsigned height);

#endif
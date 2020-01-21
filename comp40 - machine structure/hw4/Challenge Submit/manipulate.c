/*  manipulate.c

    Vladimir Hugec and Jamie Weiss
    COMP 40 HW4 - Arith
    
    Implementation of manipulate.h
*/

#include <stdlib.h>
#include <math.h>
#include <mem.h>

#include "assert.h"
#include "a2methods.h"
#include "a2plain.h"
#include "a2blocked.h"
#include "uarray2.h"
#include "bitpack.h"
#include "pnm.h"

#include "manipulate.h"
#include "ConvertRGBandCOMP.h"
#include "pack_and_unpack.h"

/***********************************/
/* Compress and Decompress Drivers */
/***********************************/

void compress(Pnm_ppm pixmap){
        assert(pixmap);
        checkDimensions(pixmap);
        RGBtoCOMP(pixmap);
        initializeFILE(stdout, pixmap->width, pixmap->height);
        for (unsigned bRow = 0; bRow < pixmap->height / 2; bRow++){
                for (unsigned bCol = 0; bCol < pixmap->width / 2; bCol++) {
                uint64_t word = 0;
                int startRowIndex = bRow * 2;
                int startColIndex = bCol * 2;
                word = getPackedData(pixmap,startRowIndex,startColIndex, word);
                putWordInFile(word);
                }
        }
}

Pnm_ppm decompress(FILE *compressedFP) {
        A2Methods_T methods = uarray2_methods_plain;
        Pnm_ppm decompPM = initDecompressedPM(compressedFP);

        for (unsigned bRow = 0; bRow < (decompPM->height/2); bRow++) {
                for (unsigned bCol = 0; bCol < (decompPM->width/2); bCol++) {
                        uint64_t word = getWordFromFile(compressedFP);
                        assert(word);

                        double a = unpackA(word);
                        double b = unpackB(word);
                        double c = unpackC(word);
                        double d = unpackD(word);
                        double Pb = unpackPb(word);
                        double Pr = unpackPr(word);

                        double y1 = getY1(a, b, c, d);
                        double y2 = getY2(a, b, c, d);
                        double y3 = getY3(a, b, c, d);
                        double y4 = getY4(a, b, c, d);

                        Pnm_rgb topLeftLoc = methods->at(decompPM->pixels,
                                                        (bCol*2),(bRow*2));
                        Pnm_rgb topRightLoc = methods->at(decompPM->pixels,
                                                        (bCol*2)+1, (bRow*2));
                        Pnm_rgb botLeftLoc = methods->at(decompPM->pixels,
                                                        (bCol*2), (bRow*2)+1);
                        Pnm_rgb botRightLoc = methods->at(decompPM->pixels,
                                                        (bCol*2)+1,(bRow*2)+1);

                        Pnm_rgb topLeft = COMPtoRGB(y1, Pb, Pr, 
                                                decompPM->denominator);
                        Pnm_rgb topRight = COMPtoRGB(y2, Pb, Pr, 
                                                decompPM->denominator);
                        Pnm_rgb botLeft = COMPtoRGB(y3, Pb, Pr, 
                                                decompPM->denominator);
                        Pnm_rgb botRight = COMPtoRGB(y4, Pb, Pr, 
                                                decompPM->denominator);

                        *topLeftLoc = *topLeft;
                        *topRightLoc = *topRight;
                        *botLeftLoc = *botLeft;
                        *botRightLoc = *botRight;

                        free(topLeft);
                        free(topRight);
                        free(botLeft);
                        free(botRight);
                }
        }
    return decompPM;
}

/***********************************/
/*        Memory Manipulation      */
/***********************************/

void checkDimensions(Pnm_ppm pixmap)
{
        A2Methods_T methods = uarray2_methods_plain;
        A2Methods_UArray2 a2;
        if (pixmap->width % 2 != 0) {
                a2 = methods->new(pixmap->width-1, pixmap->height, sizeof(struct Pnm_rgb));
                trimDimensions(pixmap, a2);
        }
        if (pixmap->height % 2 != 0) {
                a2 = methods->new(pixmap->width, pixmap->height-1, sizeof(struct Pnm_rgb));
                trimDimensions(pixmap, a2);
        }
}

Pnm_ppm allocateRGBPixmap(int width, int height)
{
        A2Methods_T methods = uarray2_methods_plain;
        A2Methods_UArray2 a2 = methods->new(width, height, sizeof(struct Pnm_rgb));
        Pnm_ppm pixmap = malloc(sizeof(struct Pnm_ppm));
        assert(pixmap);
        pixmap->pixels = a2;   
        pixmap->width = width;
        pixmap->height = height;
        pixmap->methods = methods;
        pixmap->denominator = 255;

        return pixmap;
}

void trimDimensions(Pnm_ppm pixmap, A2Methods_UArray2 a2)
{
        A2Methods_T methods = uarray2_methods_plain;
        for (int h = 0; h < methods->height(a2); h++) {
                for (int w = 0; w < methods->width(a2); w++) {
                        Pnm_rgb pix = methods->at(pixmap->pixels, w, h);
                        Pnm_rgb pix2 = methods->at(a2, w, h);
                        *pix2 = *pix;
                }
        }
        methods->free(&(pixmap->pixels));
        pixmap->pixels = a2;
        pixmap->width = methods->width(a2);
        pixmap->height = methods->height(a2);
}

void initializeFILE(FILE *file, unsigned width, unsigned height){
        fprintf(file, "COMP40 Compressed image format 2\n%u %u\n", width, height);
}

Pnm_ppm initDecompressedPM(FILE *file){
        unsigned height, width;
        int read = fscanf(file, "COMP40 Compressed image format 2\n%u %u", &width, &height);
        assert(read == 2);
        int c = getc(file);
        assert(c == '\n');

        return allocateRGBPixmap(width, height);
}



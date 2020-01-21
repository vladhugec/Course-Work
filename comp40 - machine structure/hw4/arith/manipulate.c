#include "manipulate.h"
#include <stdlib.h>
#include "assert.h"
#include "a2methods.h"
#include "a2plain.h"
#include "a2blocked.h"
#include "uarray2.h"
#include <math.h>

#define A2 A2Methods_UArray2

void checkDimensions(Pnm_ppm pixmap)
{
        A2Methods_T methods = uarray2_methods_plain;
        if (pixmap->width % 2 != 0) {
                A2 a2 = allocateRGBPixmap(pixmap->width-1, pixmap->height, methods);
                trimDimensions(pixmap, a2);
        }
        if (pixmap->height % 2 != 0) {
                A2 a2 = allocateRGBPixmap(pixmap->width, pixmap->height-1, methods);
                trimDimensions(pixmap, a2);
        }
}

void trimDimensions(Pnm_ppm pixmap, A2 pm2)
{ 
        UArray2_T a2 = pm2;
        unsigned width = a2->width;
        unsigned height = a2->height;
        for (unsigned w = 0; w < width; w++) {
                for (unsigned h = 0; h < height; h++) {
                        Pnm_rgb pix = pixmap->methods->at(pixmap->pixels, w, h);
                        Pnm_rgb pix2 = pixmap->methods->at(pm2, w, h);
                        *pix2 = *pix;
                }
        }
        freePixmap(pixmap);
        pixmap->pixels = pm2;
        pixmap->width = width;
        pixmap->height = height;
}

void freePixmap(Pnm_ppm pixmap)
{
        pixmap->methods->free(&(pixmap->pixels));
}

void setComponent(COMPONENT yPbPr, int red, int green, int blue, int denominator)
{
        /// first divide r g b each by denominator 
        // to get (0,1) value then do ->
        float fRed = (float) red / denominator;
        float fGreen = (float) green / denominator;
        float fBlue = (float) blue / denominator;
        yPbPr->y = 0.299 * fRed+0.587 * fGreen+0.114 * fBlue; 
        yPbPr->Pb = -0.168736 * fRed-0.331264 * fGreen+0.5 * fBlue;
        yPbPr->Pr = 0.5 * fRed-0.418688 * fGreen-0.081312 * fBlue;
}

COMPONENT convertToComponent(Pnm_rgb pixel, int denominator)
{
        COMPONENT yPbPr = malloc(sizeof(COMPONENT));
        setComponent(yPbPr, pixel->red, pixel->green, pixel->blue, denominator);
        return yPbPr;
}

void MAPrbgTOcomp(int col, int row, A2Methods_UArray2 A2, void *elem, void *cl)
{
        (void) A2;
        Pnm_ppm pixmap = cl;
        Pnm_rgb pixel = pixmap->methods->at(pixmap->pixels, col, row);
        COMPONENT yPbPr = convertToComponent(pixel, pixmap->denominator);
        COMPONENT insertLoc = elem;
        *insertLoc = *yPbPr;
}

void RGBtoCOMP(Pnm_ppm pixmap)
{
        A2Methods_T methods = uarray2_methods_plain;
        A2Methods_mapfun *map = methods->map_row_major;
        A2 compPixmap = allocateCOMPPixmap(pixmap->width, 
                                        pixmap->height, methods);
        map(compPixmap, (A2Methods_applyfun*) MAPrbgTOcomp, pixmap);
        freePixmap(pixmap);
        pixmap->pixels = compPixmap;
}
/*
void COMPtoRGB(Pnm_ppm pixmap)
{

}

void packBlockToWord(Pnm_ppm pixmap)
{
        A2 packedPM = allocateCOMPPixmap(pixmap->width/2, pixmap->height/2, 
                                                        pixmap->methods);
        float totPB, totPR;
        totPB = 0; totPR = 0;

        for (int bW = 0; bW < packedPM->width; bW++){
                for (int bH = 0; bH < packedPM->height; bH++){
                        COMPONENT block = malloc(sizeof(COMPONENT));
                        COMPONENT blockLoc = pixmap->methods->at(packedPM, bW, bH);
                        for (int w = bW*2; w < (bW*2)+1; w++) {
                                for (int h = bH*2; w < (bH*2)+1; w++) {
                                        COMPONENT pixel = pixmap->methods->at(pixmap->pixels, w, h);
                                        totPB += pixel->Pb;
                                        totPR += pixel->Pr;
                                }
                        }
                        block->Pb = totPB/4;
                        block->Pr = totPR/4;

                
                }

        }
}*/

uint64_t mapPregion(float value){
        unsigned uValue = Arith40_index_of_chroma(value);
        uint64_t bValue = uValue;
        return bValue;
}

int64_t mapDCTregion(float value){
        int64_t region;
        region = round(value / 0.02);
        return region;
}

uint64_t mapAregion(float value){
        uint64_t region;
        region = round(value * 511);
        return region;
}

float getA(float y1, float y2, float y3, float y4){
        return (y1 + y2 + y3 + y4) / 4.0;
}

float getB(float y1, float y2, float y3, float y4){
        float b = (y4 + y3 - y2 - y1) / 4.0;
        if (abs(b) > 0.3)
                return 0.3;
        else
                return b;
}

float getC(float y1, float y2, float y3, float y4){
        float c = (y4 - y3 + y2 - y1) / 4.0;
        if (abs(c) > 0.3)
                return 0.3;
        else
                return c;
}

float getD(float y1, float y2, float y3, float y4){
        float d = (y4 - y3 - y2 + y1) / 4.0;
        if (abs(d) > 0.3)
                return 0.3;
        else
                return d;
}

float getP(float x, float y, float z, float q){
        return (x + y + z + q) / 4.0;
}

Pnm_ppm allocateDataMap(int width, int height, A2Methods_T methods){
        A2 blocks = methods->new(width, height, sizeof(uData));
        Pnm_ppm uMap = malloc(sizeof(Pnm_ppm));
        uMap->pixels = blocks;
        uMap->width = width;
        uMap->height = height;
        return uMap;
}

uData calculateData(COMPONENT topLeft,    COMPONENT topRight,
                    COMPONENT bottomLeft, COMPONENT bottomRight){
        uData currUdata = malloc(sizeof(uData));
        // now from each cell, calculate each quantization using formula from spec
        float a = getA(topLeft->y, topRight->y, bottomLeft->y,bottomRight->y);
        // and set that new data as an attribute of the current struct
        uint64_t uA = mapAregion(a);
        currUdata->a = uA;
        //printf("current a = %d\n", a);
        // NOTE: we are going to store everything as a float right now and will
        //       cast if necessary when actually packing

        float b = getB(topLeft->y, topRight->y, bottomLeft->y,bottomRight->y);
        int64_t uB = mapDCTregion(b);
        currUdata->b = uB;
        //printf("current b = %d\n", b);
        float c = getC(topLeft->y, topRight->y, bottomLeft->y,bottomRight->y);
        int64_t uC = mapDCTregion(c);
        currUdata->c = uC;
        //printf("current c = %d\n", c);
        float d = getD(topLeft->y, topRight->y, bottomLeft->y,bottomRight->y);
        int64_t uD = mapDCTregion(d);
        currUdata->d = uD;
        //printf("current d = %d\n", d);

        float Pr = getP(topLeft->Pr, topRight->Pr, bottomLeft->Pr,bottomRight->Pr);
        uint64_t uPr = mapPregion(Pr);
        currUdata->Pr = uPr;
        //printf("current Pr = %d\n", Pr);

        float Pb = getP(topLeft->Pb, topRight->Pb, bottomLeft->Pb,bottomRight->Pb);
        uint64_t uPb = mapPregion(Pb);
        currUdata->Pb = uPb;
        //printf("current Pb = %d\n", Pb);

        return currUdata;
}

Pnm_ppm getPackingData(Pnm_ppm cPixmap, A2Methods_T methods){
        Pnm_ppm uMap = allocateDataMap(cPixmap->width /2 , cPixmap->height /2, methods);
        
        // these two will simply be for indexing through blocks 
        // we need it becuase our for loop doesnt index correctly
        int bR = 0;
        int bC = 0;
        // these loops should iterate straight to the top left corner of each 2x2 box
        for(unsigned r = 0; r < cPixmap->height; r += 2){
                for(unsigned c = 0; c < cPixmap->width; c += 2){
                        printf("current block index = [%d, %d]\n", bC, bR);
                        COMPONENT topLeft = methods->at(cPixmap->pixels, c, r);
                        COMPONENT topRight = methods->at(cPixmap->pixels, c + 1, r);
                        COMPONENT bottomLeft = methods->at(cPixmap->pixels, c, r + 1);
                        COMPONENT bottomRight = methods->at(cPixmap->pixels, c + 1, r + 1);
                        printf("current block cells\n [%d, %d]\t[%d, %d]\n [%d, %d]\t[%d, %d]\n\n\n", c, r, c+1, r, c, r+1, c+1, r+1);

                        uData currUdata = calculateData(topLeft, topRight, bottomLeft, bottomRight);

                        uData loc = methods->at(uMap->pixels, bC, bR);
                        *loc = *currUdata;

                        bC++;
                }
                bR ++;
        }        
        return uMap;
}

/*void unpackWordFromBlock(Pnm_ppm pixmap)
{

}

void printOut(Pnm_ppm pixmap)
{

}

void readIn(FILE *fp)
{

}*/

A2 allocateRGBPixmap(int width, int height, A2Methods_T methods)
{
        A2Methods_UArray2 a2 = methods->new(width, height, sizeof(Pnm_rgb));
        return a2;
}

A2 allocateCOMPPixmap(int width, int height, A2Methods_T methods)
{
        A2Methods_UArray2 a2 = methods->new(width, height, sizeof(COMPONENT));
        return a2;
}

#undef A2
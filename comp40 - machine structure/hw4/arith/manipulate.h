#ifndef MANIPULATE_H
#define MANIPULATE_H

#include "arith40.h"
#include "pnm.h"

#define A2 A2Methods_UArray2

typedef struct Pnm_yPbPr *COMPONENT;
typedef struct uData *uData;

struct Pnm_yPbPr {
        float y;
        float Pb;
        float Pr;
};

struct uData {
        uint64_t a, Pr, Pb;
        int64_t b, c, d;
};

extern void checkDimensions(Pnm_ppm pixmap);
extern void trimDimensions(Pnm_ppm pixmap, A2 pm2);
extern void RGBtoCOMP(Pnm_ppm pixmap);
extern void COMPtoRGB(Pnm_ppm pixmap);
extern void packBlockToWord(Pnm_ppm pixmap);
extern void unpackWordFromBlock(Pnm_ppm pixmap);
extern void printOut(Pnm_ppm pixmap);
extern void readIn(FILE *fp);
extern void freePixmap(Pnm_ppm pixmap);

uint64_t mapPregion(float value);
int64_t mapDCTregion(float value);
uint64_t mapAregion(float value);
float getA(float y1, float y2, float y3, float y4);
float getB(float y1, float y2, float y3, float y4);
float getC(float y1, float y2, float y3, float y4);
float getD(float y1, float y2, float y3, float y4);
float getP(float x, float y, float z, float q);
Pnm_ppm allocateDataMap(int width, int height, A2Methods_T methods);
uData calculateData(COMPONENT topLeft,    COMPONENT topRight,
                    COMPONENT bottomLeft, COMPONENT bottomRight);
Pnm_ppm getPackingData(Pnm_ppm cPixmap, A2Methods_T methods);
A2 allocateRGBPixmap(int width, int height, A2Methods_T methods);
A2 allocateCOMPPixmap(int width, int height, A2Methods_T methods);


#endif
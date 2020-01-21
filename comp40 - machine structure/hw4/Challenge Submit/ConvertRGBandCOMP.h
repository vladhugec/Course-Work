#ifndef CONVERTRGBANDCOMP_H
#define CONVERTRGBANDCOMP_H
/*
ConvertRGBandCOMP.h

Vladimir Hugec and Jamie Weiss
Comp40 HW4 Arith

Header file for conversion between RGB color and YPbPr color
*/

#include "pnm.h"

typedef struct Pnm_yPbPr {
        double y;
        double Pb;
        double Pr;
} *COMPONENT;

extern void RGBtoCOMP(Pnm_ppm pixmap);
extern COMPONENT convertToComponent(Pnm_rgb pixel, int denominator);
extern void setComponent(COMPONENT yPbPr, int red, int green, int blue, 
                                            int denominator);
extern Pnm_rgb COMPtoRGB(double y, double Pb, double Pr, int denominator);
extern void fixDenominator(Pnm_rgb rgb);
extern void checkNegativeRGBVal(Pnm_rgb rgb);

#endif
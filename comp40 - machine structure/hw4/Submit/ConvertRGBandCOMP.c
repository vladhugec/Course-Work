/*
ConvertRGBandCOMP.c

Vladimir Hugec and Jamie Weiss
Comp40 HW4 Arith

Implementation of for ConvertRGBandCOMP.h
*/
#include <math.h>
#include <stdlib.h>
#include "ConvertRGBandCOMP.h"
#include "assert.h"
#include "a2plain.h"
#include "a2blocked.h"
#include "uarray2.h"
#include "manipulate.h"


void RGBtoCOMP(Pnm_ppm pixmap){
        A2Methods_T methods = uarray2_methods_plain;
        A2Methods_UArray2 comp = methods->new(pixmap->width, pixmap->height, 
                                sizeof(struct Pnm_yPbPr));
        for (unsigned i = 0; i < pixmap->height; i++){
        for (unsigned j = 0; j < pixmap->width; j++){
                Pnm_rgb currentRGB = methods->at(pixmap->pixels, j, i);

                COMPONENT currentCOMP = convertToComponent(currentRGB, 
                                                pixmap->denominator);

                COMPONENT loc = methods->at(comp, j, i);
                *loc = *currentCOMP;
                free(currentCOMP);
                }
        }
        methods->free(&(pixmap->pixels));
        pixmap->pixels = comp;
}

COMPONENT convertToComponent(Pnm_rgb pixel, int denominator)
{
        COMPONENT yPbPr = malloc(sizeof(struct Pnm_yPbPr));
        assert(yPbPr);
        setComponent(yPbPr, pixel->red, pixel->green, pixel->blue, denominator);
        return yPbPr;
}

void setComponent(COMPONENT yPbPr, int red, int green, int blue, int denominator)
{
        double fRed = (double)red / (double)denominator;
        double fGreen = (double)green / (double)denominator; 
        double fBlue = (double)blue / (double)denominator;

        yPbPr->y = 0.299 * fRed + 0.587 * fGreen + 0.114 * fBlue; 
        yPbPr->Pb = -0.168736 * fRed - 0.331264 * fGreen + 0.5 * fBlue;
        yPbPr->Pr = 0.5 * fRed - 0.418688 * fGreen - 0.081312 * fBlue;

}

Pnm_rgb COMPtoRGB(double y, double Pb, double Pr, int denominator){
        double red = (1.0 * y + 0.0 * Pb + 1.402 * Pr) * (double) denominator;
        double green = (1.0 * y - 0.344136 * Pb - 0.714136 * Pr) * (double) denominator;
        double blue = (1.0 * y + 1.772 * Pb + 0.0 * Pr) * (double) denominator;
        Pnm_rgb rgb = malloc(sizeof(struct Pnm_rgb));
        rgb->red = round(red);
        rgb->blue = round(blue);
        rgb->green = round(green);

        checkNegativeRGBVal(rgb);
        fixDenominator(rgb);

        return rgb;
}

void fixDenominator(Pnm_rgb rgb) {
        if (rgb->red > 255) {
                rgb->red = 255 - (rgb->red-255);
        }
        if (rgb->green > 255) {
                rgb->green = 255 - (rgb->green - 255);
        }
        if (rgb->blue > 255) {
                rgb->blue = 255 - (rgb->blue - 255);
        }
}

void checkNegativeRGBVal(Pnm_rgb rgb) {
        int red = rgb->red;
        int green = rgb->green;
        int blue = rgb->blue;

        if (red < 0) {
                rgb->red = red*-1;
        }
        if (green < 0) {
                rgb->green = green*-1;
        }
        if (blue < 0) {
                rgb->blue = blue*-1;
        }
}
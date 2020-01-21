#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <pnm.h>
#include "uarray2.h"
#include <a2methods.h>
#include "a2plain.h"
#include "a2blocked.h"
#include <math.h>
#include <string.h>

typedef struct FilesComparison {
        int f1H;
        int f1W;
        int f2H;
        int f2W;
        int smallerWidth;
        int smallerHeight;
} fComp;

fComp* testPixelDimensions(int f1W, int f1H, int f2W, int f2H) {
        fComp *fc = malloc(sizeof(fComp));
        fc->f1H = f1H;
        fc->f1W = f1W;
        fc->f2H = f2H;
        fc->f2W = f2W;

        if (f1W <= f2W)
                fc->smallerWidth = f1W;
        else
                fc->smallerWidth = f2W;

        if (f1H <= f2H)
                fc->smallerHeight = f1H;
        else
                fc->smallerHeight = f2H;

        return fc;
}

void testFileDataParam(fComp *fD) {
        TRY
                assert(fD->f1H == fD->f2H || fD->f1H == fD->f2H + 1 
                                        || fD->f1H == fD->f2H - 1);

                assert(fD->f1H == fD->f2H || fD->f1H == fD->f2H + 1 
                                        || fD->f1H == fD->f2H - 1);

                assert(fD->f1H == fD->f2H || fD->f1H == fD->f2H + 1 
                                        || fD->f1H == fD->f2H - 1);

                assert(fD->f1H == fD->f2H || fD->f1H == fD->f2H + 1 
                                        || fD->f1H == fD->f2H - 1);
        EXCEPT(Assert_Failed)
                fprintf(stderr, "ERROR: heights and widths of files " 
                                "provided differ my more than 1 \n");
                exit(EXIT_FAILURE);
        END_TRY;
}

float computeMeanSqDiff(Pnm_ppm pmF1, Pnm_ppm pmF2, fComp *fData) {
        float sumPixDiff = 0;
        float denominator = 3 * fData->smallerWidth * fData->smallerHeight;

        for (int w = 0; w < fData->smallerWidth; w++) {
                for (int h = 0; h < fData->smallerHeight; h++) {
                        Pnm_rgb pixF1 = pmF1->methods->at(pmF1->pixels, w, h);
                        Pnm_rgb pixF2 = pmF2->methods->at(pmF2->pixels, w, h);

                        int redDiffSq = pow(pixF1->red - pixF2->red, 2);
                        int greenDiffSq = pow(pixF1->green - pixF2->green, 2);
                        int blueDiffSq = pow(pixF1->blue - pixF2->blue, 2);
                        sumPixDiff += (redDiffSq + greenDiffSq + blueDiffSq);

                }
        }

        float totSqDiff = sumPixDiff/denominator;

        return sqrt(totSqDiff);
}

float diff(FILE *fp1, FILE *fp2) 
{
        A2Methods_T methods = uarray2_methods_plain;
        
        Pnm_ppm pixmapF1 = Pnm_ppmread(fp1, methods);
        Pnm_ppm pixmapF2 = Pnm_ppmread(fp2, methods);
        //create new reader for file and save file data as mData

        fComp *filesData = testPixelDimensions(pixmapF1->width, 
                pixmapF1->height, pixmapF2->width, pixmapF2->height);

        testFileDataParam(filesData);
        
        return computeMeanSqDiff(pixmapF1, pixmapF2, filesData);        
}


int main(int argc, char const *argv[])
{
        assert(argc == 3);

        FILE *fp1 = fopen(argv[1], "rb"); 
        FILE *fp2 = fopen(argv[2], "rb"); 

        if ((fp1 == NULL) & (strcmp(argv[1], "-") == 0)) {
                fp1 = stdin;
        }
        else if ((fp2 == NULL) & (strcmp(argv[2], "-") == 0)) {
                fp2 = stdin;
        }

        assert(fp1 != NULL && fp2 != NULL);

        float differnece = diff(fp1, fp2);

        printf("%.4f\n", differnece);

        return EXIT_SUCCESS;
}

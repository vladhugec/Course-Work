#include <pnmrdr.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>

void getAvgBrightness(FILE *f) {
        TRY
                //create new reader for file and save file data as mData
                Pnmrdr_T reader = Pnmrdr_new(f);
                Pnmrdr_mapdata mData = Pnmrdr_data(reader);
                
                //ERROR: check that the file-type is the correct PNM filetype
                assert(mData.type == 2);

                unsigned numPixels = mData.width * mData.height;
                unsigned brightnessSum = 0;

                //loop through each pixel and add pixel's brightness to total
                for (unsigned i = 0; i < numPixels; i++){
                        brightnessSum += Pnmrdr_get(reader);
                }

                //calculate average, value between 0-1, and print to terminal
                double avgBrightness = (double)brightnessSum/numPixels
                                        /mData.denominator;
                printf("%.3f\n", avgBrightness);
                Pnmrdr_free(&reader);

        EXCEPT(Pnmrdr_Badformat);
                fprintf(stderr,"ERROR: No readable file provided\n");
                exit(EXIT_FAILURE);

        EXCEPT(Pnmrdr_Count);
                fprintf(stderr,"ERROR: Client read beyond the last integer\
                                in the file\n");
                exit(EXIT_FAILURE);

        END_TRY;
}

int main(int argc, char const *argv[])
{
        // FILE input using stdin buffer
        if (argc == 1) {
                //check if stdin buffer is empty
                if (isatty(STDIN_FILENO)) {
                        fprintf(stderr, "ERROR: no arguments supplied\n");
                        exit(EXIT_FAILURE);
                }

                getAvgBrightness(stdin);
                return EXIT_SUCCESS;
        }
        
        if (argc != 2) {
                fprintf(stderr,"ERROR: invalid number of arguments\n");
                exit(EXIT_FAILURE);
        }

        FILE *fp = fopen(argv[1], "rb");

        assert(fp != NULL);

        getAvgBrightness(fp);

        fclose(fp);

        return EXIT_SUCCESS;
}
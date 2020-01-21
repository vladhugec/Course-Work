#include <pnmrdr.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>
#include "uarray2.h"


/* extern void UArray2_map_col_major(UA2 uArray2, MAPFUNC_PTR apply, void *cl);
extern void UArray2_map_row_major(UA2 uArray2, MAPFUNC_PTR apply, void *cl);

typedef void (*MAPFUNC_PTR)(int, int, UA2, void*, void*);
*/

UArray2_T pnmGet(FILE *f){
        TRY
                //create new reader for file and save file data as mData
                Pnmrdr_T reader = Pnmrdr_new(f);
                Pnmrdr_mapdata mData = Pnmrdr_data(reader);

                unsigned numPixels = mData.width * mData.height;

                //loop through each pixel and add pixel's brightness to total
                for (unsigned i = 0; i < 30; i++){
                        printf("%u ",Pnmrdr_get(reader));
                }

                UArray2_T uA2 = UArray2_new(mData.height, mData.width, 
                                numPixels*sizeof(char));

                Pnmrdr_free(&reader);

                return uA2;

        EXCEPT(Pnmrdr_Badformat);
                fprintf(stderr,"ERROR: No readable file provided\n");
                exit(EXIT_FAILURE);

        EXCEPT(Pnmrdr_Count);
                fprintf(stderr,"ERROR: Client read beyond the last integer\
                                in the file\n");
                exit(EXIT_FAILURE);

        END_TRY;

        return NULL;
}


void uAGet(int height, int width, UArray2_T uArray2, void*pos, void*cl){
        TRY
                (void)height;
                (void)width;
                (void)uArray2;
                (void)cl;
                //create new reader for file and save file data as mData
                //Pnmrdr_T reader = Pnmrdr_new((FILE*)cl);
                //Pnmrdr_mapdata mData = Pnmrdr_data(reader);
                

                //unsigned numPixels = mData.width * mData.height;

                //loop through each pixel and add pixel's brightness to total
                for (unsigned i = 0; i < 30; i++){
                        printf("%d ", *(int*)pos);
                }
                printf("\n ");
                //Pnmrdr_free(&reader);

        EXCEPT(Pnmrdr_Badformat);
                fprintf(stderr,"ERROR: No readable file provided\n");
                exit(EXIT_FAILURE);

        EXCEPT(Pnmrdr_Count);
                fprintf(stderr,"ERROR: Client read beyond the last integer\
                                in the file\n");
                exit(EXIT_FAILURE);

        END_TRY;
}

void doThis(FILE *fp) {
        UArray2_T uA2 = pnmGet(fp);
        UArray2_map_row_major(uA2, uAGet, fp);
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

                doThis(stdin);
                return EXIT_SUCCESS;
        }
        
        if (argc != 2) {
                fprintf(stderr,"ERROR: invalid number of arguments\n");
                exit(EXIT_FAILURE);
        }

        FILE *fp = fopen(argv[1], "rb");

        assert(fp != NULL);

        doThis(fp);

        fclose(fp);

        return EXIT_SUCCESS;
}
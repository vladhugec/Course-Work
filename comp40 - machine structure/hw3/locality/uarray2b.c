#include "uarray2b.h"
#include <math.h>
#include <mem.h>
#include <assert.h>
#include <stdio.h>
#include <uarray.h>
#include "uarray2.h"
#define UA2B UArray2b_T
#define MAX_BLOCK_SIZE 64*1024

struct UA2B {
        int width;
        int height;
        int blocksize;
        int size;
        UArray2_T uArray2; /* each element is one "block" of UArray_T type */
};

void UArray2b_make(UA2B uArray2b, int width, int height, 
                int size, int blocksize) {
        /* ERROR CHECKS */
        assert(uArray2b); /* check NEW() error */

        /*width and height may not be divisible by blocksize so
                the array size should take into account the remainder*/
        int ua2_height = height/blocksize + height % blocksize;
        int ua2_width = width/blocksize + width % blocksize;

        UArray2_T uArray2 = UArray2_new(ua2_width, ua2_height, 
                                        sizeof(UArray_T));

        for (int i = 0; i < (ua2_height)*(ua2_width); i++) {
                void *box = UArray_at(uArray2->uArray, i);
                UArray_T newBox = UArray_new(blocksize*blocksize, size);
                assert(UArray_length(newBox) == blocksize*blocksize);
                *(UArray_T*)box = newBox;
        }

        /* check Hanson array make success */
        assert(uArray2);
        /* checks UArray2 requirements error */ 
        assert(size > 0);

        uArray2b->width = width;
        uArray2b->height = height;
        uArray2b->blocksize = blocksize;
        uArray2b->size = size;
        uArray2b->uArray2 = uArray2;
}

UArray2b_T UArray2b_new(int width, int height, int size, int blocksize) {
        /* Declare new array and allocate instance */
        UA2B uArray2b;
        NEW(uArray2b);

        if ((height*width) > 0) {
                UArray2b_make(uArray2b, width, height, size, blocksize);
        }
        else {
                UArray2b_make(uArray2b, width, height, size, blocksize);
        }
        
        return uArray2b;
}
        
UArray2b_T UArray2b_new_64K_block(int width, int height, int size) {
        int blocksize;

        if (size < MAX_BLOCK_SIZE) {
                blocksize = floor(sqrt(MAX_BLOCK_SIZE/size));
        }
        else {
                blocksize = 1;
        }

        return UArray2b_new(width, height, size, blocksize);
}

void UArray2b_free(UA2B *uArray2b) {
        /* ERROR CHECK */
        assert(uArray2b && *uArray2b); /*check existance uArray being freed */
        /* free instance in question */
        for (int i = 0; i < UArray2_length((*uArray2b)->uArray2); i++) {
                UArray_T *box = UArray_at((*uArray2b)->uArray2->uArray, i);
                UArray_free(box);
        }
        UArray2_free(&((*uArray2b)->uArray2));
        FREE(*uArray2b);
}
        
int UArray2b_width(UA2B uArray2b) {
        return uArray2b->width;
}
        
int UArray2b_height(UA2B uArray2b) {
        return uArray2b->height;
}
        
int UArray2b_size(UA2B uArray2b) {
        return UArray_size(uArray2b->uArray2->uArray);
}
        
int UArray2b_blocksize(UA2B uArray2b) {
        return uArray2b->blocksize;
}
        
void *UArray2b_at(UA2B uArray2b, int column, int row) {
        /* get the col and row #'s of the appropriate box */
        int colLoc = column/uArray2b->blocksize;
        int rowLoc = row/uArray2b->blocksize;
        /* get the col and row #'s of the appropriate element within box */
        int boxColLoc = column%uArray2b->blocksize;
        int boxRowLoc = row%uArray2b->blocksize;
        
        UArray_T *box = UArray2_at(uArray2b->uArray2, colLoc, rowLoc);
        
        int elemLoc = (uArray2b->blocksize*boxColLoc) + boxRowLoc;
        /* return appropriate element */
        return UArray_at(*box, elemLoc);
}
        
void UArray2b_map(UA2B uArray2b,
        void apply(int col, int row, UA2B uArray2b, void *elem, void *cl),
        void *cl) 
{
        int blockSize = uArray2b->blocksize;
        int height = UArray2_height(uArray2b->uArray2);
        int width = UArray2_width(uArray2b->uArray2);

        for (int c = 0; c < height; c++) {
                for (int r = 0; r < width; r++) {
                        for(int i = 0; i < blockSize*blockSize; i++) {
                                int cellRow = (i / blockSize) + c * blockSize;
                                int cellCol = i % blockSize + blockSize * r;

                                if (cellRow < uArray2b->height 
                                   && cellCol < uArray2b->width) {
                                        apply(cellCol, cellRow, uArray2b, 
                                        UArray2b_at(uArray2b, cellCol, cellRow)
                                        , cl);
                                }
                        }
                }
        }
}


#undef UA2B
#undef MAX_BLOCK_SIZE
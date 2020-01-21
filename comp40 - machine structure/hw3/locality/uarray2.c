/*   uarray2.c  
BY: Vladimir Hugec and Jamie Wiess
HW2 Comp-40     9-25-18 
*/

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <mem.h>
#include <assert.h>
#include <except.h>

#include "uarray2.h"

#define UA2 UArray2_T

void UArray2_make(UA2 uArray2, int width, int height, int size) {
        /* ERROR CHECKS */
        assert(uArray2); /* check NEW() error */
        
        UArray_T uArray = UArray_new(height*width, size);
        assert(uArray); /* check Hanson array make success */
        if ((height*width) > 0) {
                uArray2->uArray = uArray;
        }
        else {
                uArray2->uArray = NULL;
        }

        /* checks UArray2 requirements error */ 
        assert(size > 0);

        uArray2->width = width;
        uArray2->height = height;
}

UA2 UArray2_new(int width, int height, int size) {
        /* Declare new array and allocate instance */
        UA2 uArray2;
        NEW(uArray2);

        if ((height*width) > 0) {
                UArray2_make(uArray2, width, height, size);
        }
        else {
                UArray2_make(uArray2, width, height, size);
        }
        
        return uArray2;
}

void *UArray2_at(UA2 uArray2, int col, int row) {
        int loc = rowColToLoc(uArray2->width, col, row);
        /* ERROR CHECK */
        /* check existance of uArray being accessed */
        // fprintf(stderr, "\nIN 2 AT %s\n", "");
        // fprintf(stderr, "row: %u\n", row);
        // fprintf(stderr, "col:%u\n", col);
        // fprintf(stderr, "height %u\n", uArray2->height);
        // fprintf(stderr, "width %u\n", uArray2->width);
        // assert(row >= 0 && col >=0 && loc <(uArray2->height)*(uArray2->width));

        return UArray_at(uArray2->uArray, loc);
}

int rowColToLoc(int colWidth, int col, int row) {
        if (row == 0 && col >= 0) {
                return col;
        }
        else {
                return ((colWidth*row) + col);
        }
        return -1;
}

int UArray2_length(UA2 uArray2) {
        return UArray_length(uArray2->uArray);
}

int UArray2_width(UA2 uArray2) {
        assert(uArray2);
        return uArray2->width;
}

int UArray2_height(UA2 uArray2) {
        assert(uArray2);
        return uArray2->height;
}

int UArray2_size(UA2 uArray2) {
        return UArray_size(uArray2->uArray);
}

//UArray2 MAPPING FUNCTIONS
void UArray2_map_row_major(UA2 uArray2, MAPFUNC_PTR apply, void *cl) {
        /* iterate through rows */

        for (int i = 0; i < uArray2->height; i++) {
                for (int j = 0; j < uArray2->width; j++) {
                        int loc = rowColToLoc(uArray2->width, j, i);
                        apply(j, i, uArray2, UArray_at(uArray2->uArray, loc),
                                                                        cl); 
                }
        }
}

void UArray2_map_col_major(UA2 uArray2, MAPFUNC_PTR apply, void *cl) {
        /* iterate through columns */
        for (int j = 0; j < uArray2->width; j++) {
                for (int i = 0; i < uArray2->height; i++) {
                        int loc = rowColToLoc(uArray2->width, j, i);
                        apply(j, i, uArray2, UArray_at(uArray2->uArray, loc),
                                                                        cl);
                }
        }
}

//MEMORY Clearing FUNC
void UArray2_free(UA2 *uArray2) {
        /* ERROR CHECK */
        assert(uArray2 && *uArray2); /* check existance of uArray being freed*/
        /* free instance in question */
        UArray_free(&((*uArray2)->uArray));
        FREE(*uArray2);
}

#undef UA2

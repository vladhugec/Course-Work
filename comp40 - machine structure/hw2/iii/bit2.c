/*   bit2.c   
BY: Vladimir Hugec and Jamie Wiess
HW2 Comp-40     9-25-18 */
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <mem.h>
#include <assert.h>
#include <except.h>

#include "bit2.h"

#define B2 Bit2_T

void Bit2_make(struct B2* bArray2, int width, int height) {
        /* ERROR CHECKS */
        assert(bArray2); /* check NEW() error */

        Bit_T bArray = Bit_new(height*width);
        assert(bArray); /* check Hanson array make success */

        if ((height*width) > 0) {
                bArray2->bArray = bArray;
        }
        else {
                bArray2->bArray = NULL;
        }
        bArray2->width = width;
        bArray2->height = height;
}

Bit2_T Bit2_new(int width, int height) {
        /* Declare new array and allocate instance */
        B2 bArray2;
        NEW(bArray2);

        if ((height*width) > 0) {
                Bit2_make(bArray2, width, height);
        }
        else {
                Bit2_make(bArray2, width, height);
        }
        
        return bArray2;
}

int Bit2_width(B2 bArray2) {
        assert(bArray2);
        return bArray2->width;
}

int Bit2_height(B2 bArray2) {
        assert(bArray2);
        return bArray2->height;
}

int Bit2_put(B2 bArray2, int width, int height, int bit) {
        int loc = rowColToLoc(bArray2, width, height);
        return Bit_put(bArray2->bArray, loc, bit);
}

int Bit2_get(B2 bArray2, int width, int height) {
        int loc = rowColToLoc(bArray2, width, height);
        return Bit_get(bArray2->bArray, loc);
}

int Bit2_count(B2 bArray2) {
        return Bit_count(bArray2->bArray);
}

int Bit2_length(B2 bArray2) {
        return Bit_length(bArray2->bArray);
}

int rowColToLoc(B2 bArray2, int col, int row) {
        if (row == 0 && col >= 0)
                return col;
        else
                return ((bArray2->width * row) + col);

        return -1;
}

void Bit2_map_col_major(B2 bArray2, MAPFUNC2d apply, void *cl) {
        /* iterate through columns */
        for (int j = 0; j < bArray2->height; j++) {
                for (int i = 0; i < bArray2->width; i++) {
                        apply(i, j, bArray2, Bit2_get(bArray2, i, j), cl); 
                }
        }
}

void Bit2_map_row_major(B2 bArray2, MAPFUNC2d apply, void *cl) {
        /* iterate through rows */
        for (int i = 0; i < bArray2->width; i++) {
                for (int j = 0; j < bArray2->height; j++) {
                        apply(i, j, bArray2, Bit2_get(bArray2, i, j), cl); 
                }
        }
}

void Bit2_free(B2 *bArray2) {
        /* ERROR CHECK */
        assert(bArray2 && *bArray2); /* check existance of uArray being freed*/
        /* free instance in question */
        Bit_free(&((*bArray2)->bArray));
        FREE(*bArray2);
}

#undef B2
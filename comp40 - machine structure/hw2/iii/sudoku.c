/*   sudoku.c  
BY: Vladimir Hugec and Jamie Wiess
HW2 Comp-40     9-25-18 
*/

#include "sudoku.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "pnmrdr.h"
#include <assert.h>


int main(int argc, char *argv[])
{
        if (argc == 1) { /* if stdin input */
                checkTable(readFromPBM(stdin));
        }
        else {
                /* pbm file command line arg */
                FILE *file = getFile(argc, argv);
                checkTable(readFromPBM(file));
        }
}


/* --------------------------------- */
/*     FUNCTIONS IMPLEMENTATIONS     */
/* --------------------------------- */

void insertPixel(int col, int row, UArray2_T tableMap, void *bit, void *cl) {
        (void)col; (void)row; (void)tableMap;
        *(int*)bit = Pnmrdr_get(*(Pnmrdr_T*)cl);
}

UArray2_T readFromPBM(FILE *inputFP) {
        Pnmrdr_T reader = Pnmrdr_new(inputFP);
        Pnmrdr_mapdata mData = Pnmrdr_data(reader);
        assert(mData.type == 2);

        UArray2_T tableMap = UArray2_new(mData.width, 
                                         mData.height, sizeof(int));
        UArray2_map_col_major(tableMap, insertPixel, &reader);

        return tableMap;
}

FILE * getFile(int argc, char ** argv) {
        FILE *fp = NULL;
        if (argc != 2) {
                fprintf(stderr,"ERROR: invalid number of arguments\n");
                exit(EXIT_FAILURE);
        }

        if (argc == 1){
                /* check if stdin buffer is empty */
                if (isatty(STDIN_FILENO)){
                        fprintf(stderr,"ERROR: no arguments supplied\n");
                        exit(EXIT_FAILURE);
                }
                fp = stdin;
        }
        else {
                fp = fopen(argv[1], "rb");
                assert(fp != NULL);
        }
        return fp;
}

void checkTable(UArray2_T table){
        Set_T elems = Set_new(0, NULL, NULL);
        checkRows(table, &elems);
        checkColumns(table, &elems);
        checkBoxes(table, &elems);
        Set_free(&elems);
}

void checkRows(UArray2_T table, Set_T *elems) {
        UArray2_map_row_major(table, checkCurrentLoc, elems);
        deleteAllSetMembers(elems);
}

void checkColumns(UArray2_T table, Set_T *elems){
        UArray2_map_col_major(table, checkCurrentLoc, elems);
        deleteAllSetMembers(elems);
}


void checkBoxes(UArray2_T table, Set_T *elems){
        UArray2_map_boxes(table, checkCurrentLoc, elems);
        deleteAllSetMembers(elems);
}

/* If a rule is broken exit the program with 1 and free data */
void repeatFound(Set_T *elems, UArray2_T *uA2) {
        deleteAllSetMembers(elems);
        Set_free(elems);
        UArray2_free(uA2);
        exit(EXIT_FAILURE);
}

/* Algorithm to convert coordinates to 3x3 box index */
int evaluateBoxNum(int width, int height) {
        if (width == 0) {
                return (3 * (height/3));
        }
        return (3 * (height/3) + (9/width));
}

/* --------------------------------- */
/* MAP FUNCTIONS FOR BOXES IN SUDOKU */
/* --------------------------------- */


void checkCurrentLoc(int width, int height, UArray2_T uA2, 
                        void *pos, void* set) {
        (void)width;
        (void)height;
        (void)uA2;
        
        checkMaxIntensity(*(int*)pos);
        const void *val = Atom_int(*(int*)pos);

        int found = Set_member(*(Set_T*)set, val);

        if (found == 1) {
                repeatFound(set, &uA2); /* exit program (1) */
        }

        Set_put(*(Set_T*)set, val);
        checkSet(set, MAXDIMEN);
}

void checkMaxIntensity(int val) {
        if (!(val > 0 && val <= MAXDIMEN))
                exit(EXIT_FAILURE);
}

void UArray2_map_boxes(UArray2_T table, MAPFUNC apply, void *set) {
        int boxNum = 0, i, j;
        int rowStartLoc = 3*(floor(boxNum/3));
        int colStartLoc = 3*boxNum%3;

        do {
                for (i = rowStartLoc; i < rowStartLoc+3; i++) {
                        for (j = colStartLoc; j < colStartLoc+3; j++){
                                apply(j, i, table,UArray2_at(table, j, i),set);
                        }
                }
                boxNum = evaluateBoxNum(j, i);
                rowStartLoc = 3*(floor(boxNum/3));
                colStartLoc = 3*boxNum%3;
        }
        while (boxNum < 8);
}

/* --------------------------------- */
/* Hanson Set Manipulation Functions */
/* --------------------------------- */

void checkSet(Set_T *sMembers, int endSize) {
        if (Set_length(*sMembers) >= endSize) {
                deleteAllSetMembers(sMembers);
        }
}

void deleteAllSetMembers(Set_T *sMembers) {
        for (int i = 1; i < 10; i++) {
                const void *val = Atom_int(i);
                Set_remove(*sMembers, val);
        }
}
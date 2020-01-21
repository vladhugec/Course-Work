/* 
    UM_segData.c
    Implementation of UM_segData.h functions

    Data Loader and unloader for UM

    Vladimir Hugec and Mason Pollack
*/

#include "UM_segData.h"
#include <inttypes.h>
#include <stdlib.h>

int* make_registerArray() {
    int* array = malloc(sizeof(uint32_t)*8);

    for (int i = 0; i < 7; i++) {
        array[i] = 0;
    }

    return array;
}

Seq_T make_mainSeq() {
    Seq_T outerSeq = Seq_new(0);
    return outerSeq;
}

Seq_T make_newSegment(int x) {
    Seq_T segment = Seq_new(x);
    
    for (int i = 0; i < x; i++) {
        Seq_put(segment, i, (void*)(uintptr_t)0);
    }

    return segment;
}

int get_unmapped_loc(Seq_T last_unmapped){
    if (Seq_length(last_unmapped) == 0){
        return -1;
    }
    else{
        int * val_ptr = Seq_remlo(last_unmapped);
        return *val_ptr;
    }
}

void unmapAndSaveLoc(Seq_T mainSeq, Seq_T unmappedLocs, int loc) {
    Seq_put(mainSeq, loc, NULL);
    Seq_addhi(unmappedLocs, (void*)(uintptr_t)loc);
}
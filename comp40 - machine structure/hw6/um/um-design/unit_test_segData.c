/*

UNIT TEST FOR SEGMENT DATA STORAGE/LOADING/UNLOADING

Vladimir Hugec and Mason Pollack

*/

#include "UM_segData.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <inttypes.h>

int main() {
    Seq_T mainSeq = make_mainSeq();
    Seq_T segment = make_newSegment(10);
    Seq_T segment2 = make_newSegment(10);
    Seq_T segment3 = make_newSegment(10);
    Seq_T segment_all_0 = make_newSegment(10);
    Seq_T unmapped_locs  = Seq_new(0);

    Seq_put(mainSeq, 0, segment);
    Seq_put(mainSeq, 1, segment2);
    Seq_put(mainSeq, 2, segment3);
    
    for (int i = 0; i < 10; i++) {
        Seq_put(segment, i, (void*)(uintptr_t)i);
        Seq_put(segment2, i, (void*)(uintptr_t)i);
        Seq_put(segment3, i, (void*)(uintptr_t)i);
        Seq_put(segment_all_0, i, (void*)(uintptr_t)0);
    }
    
    Seq_T segment4 = Seq_get(mainSeq, 0);

    for (int i = 0; i < 10; i++) {
        int * val_ptr = Seq_get(segment4, i);
        int * val_ptr2 = Seq_get(segment, i);
        assert(*val_ptr == *val_ptr2);
    }

    /*put more segs onto seg array, remove first and make sure next is put in
    1st location*/
    unmapAndSaveLoc(mainSeq, unmapped_locs, 0);
    int loc = get_unmapped_loc(unmapped_locs);
    assert(loc == 0);
    Seq_put(mainSeq, loc, segment_all_0);

    Seq_T seg = Seq_get(mainSeq, loc);
    for (int i = 0; i < 10; i++) {
        int* val = Seq_get(seg, i);
        assert(*val == 0);
    }

    printf("END TEST\n");

}
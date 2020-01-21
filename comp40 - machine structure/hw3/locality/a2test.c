#include <stdbool.h>
#include <stdio.h>
#include "assert.h"
#include "a2methods.h"
#include "a2plain.h"
#include "a2blocked.h"
#include "uarray2b.h"


#define W 13
#define H 15
#define BS 4

static A2Methods_T methods;
typedef A2Methods_UArray2 A2;

static void check_and_increment(int i, int j, A2 a, void *elem, void *cl) 
{
        (void)i;
        (void)j;
        (void)a;
        int *p = elem;
        int *counter = cl;

        //fprintf(stderr, "val of p: %u\n" , *p);
        //fprintf(stderr, "val of cl: %u\n" , *counter);

        assert(*p == *counter);
        *counter += 1;   // NOT *counter++! 
}

static void small_check_and_increment(void *elem, void *cl)
{
        int *p = elem;
        int *counter = cl;

        //fprintf(stderr, "val of p: %u\n" , *p);
        //fprintf(stderr, "val of cl: %u\n" , *counter);
        assert(*p == *counter);
        *counter += 1;   // NOT *counter++!
}

static void double_row_major_plus()
{
        // store increasing integers in row-major order
        A2 array = methods->new_with_blocksize(W, H, sizeof(int), BS);
        int counter = 1;
        for (int j = 0; j < H; j++) { 
                for (int i = 0; i < W; i++) { // col index varies faster 
                        int *p = methods->at(array, i, j);
                        *p = counter++;
                }
        }
        counter = 1;
        for (int j = 0; j < H; j++) {
                for (int i = 0; i < W; i++) {
                        int *p = methods->at(array, i, j);
                        assert(*p == counter);
                        counter++;
                }
        }
        if (methods->map_row_major) {
                counter = 1;
                //fprintf(stderr, "%s\n", "map_row_major");
                methods->map_row_major(array, check_and_increment, &counter);
        }
        if (methods->small_map_row_major) {
                counter = 1;
                //fprintf(stderr, "%s\n", "small_map_row_major");
                methods->small_map_row_major(array,
                                             small_check_and_increment,
                                             &counter);
        }
        //fprintf(stderr, "%s\n", "finished both maps");
        methods->free(&array);
        //fprintf(stderr, "%s\n", "freed");
}

#if 0
static void show(int i, int j, A2 a, void *elem, void *cl) 
{
        (void)a; (void)cl;
        printf("A[%02d][%02d] == %05u -- should be %05d\n",
               i, j, *(unsigned *)elem, 1000 * i + j);
}
#endif

static inline void check(A2 a, int i, int j, unsigned n) 
{
        unsigned *p = methods->at(a, i, j);
        assert(*p == n);
}

bool has_minimum_methods(A2Methods_T m)
{
        return m->new != NULL && m->new_with_blocksize != NULL
                && m->free != NULL && m->width != NULL && m->height != NULL
                && m->size != NULL && m->blocksize != NULL && m->at != NULL;
}

bool has_small_plain_methods(A2Methods_T m) 
{
        return m->small_map_default != NULL
                && m->small_map_row_major != NULL
                && m->small_map_col_major != NULL;
}

bool has_plain_methods(A2Methods_T m) 
{
        return m->map_default != NULL
                && m->map_row_major != NULL
                && m->map_col_major != NULL;
}

bool has_small_blocked_methods(A2Methods_T m) 
{
        return m->small_map_default != NULL
                && m->small_map_block_major != NULL;
}

bool has_blocked_methods(A2Methods_T m) 
{
        return m->map_default != NULL && m->map_block_major != NULL;
}

static inline void copy_unsigned(A2Methods_T methods, A2 a,
                                 int i, int j, unsigned n) 
{
        unsigned *p = methods->at(a, i, j);
        *p = n;
}

static void test_methods(A2Methods_T methods_under_test) 
{
        //fprintf(stderr, "%s\n", "in test");
        methods = methods_under_test;
        assert(methods);
        assert(has_minimum_methods(methods));
        assert(has_small_plain_methods(methods)
               || has_small_blocked_methods(methods));
        assert(!(has_small_plain_methods(methods)
                 && has_small_blocked_methods(methods)));
        assert(!(has_plain_methods(methods)
                 && has_blocked_methods(methods)));

        if (!(has_plain_methods(methods) || has_blocked_methods(methods)))
                fprintf(stderr, "Some full mapping methods are missing\n");
        //fprintf(stderr, "%s\n", "1");
        A2 array = methods->new_with_blocksize(W, H, sizeof(unsigned), BS);
        copy_unsigned(methods, array,  2,  1, 99);
        copy_unsigned(methods, array,  3,  3, 88);
        copy_unsigned(methods, array, 10, 10, 77);
        check(array,  2,  1, 99);
        check(array,  3,  3, 88);
        check(array, 10, 10, 77);
        //fprintf(stderr, "%s\n", "2");
        for (int i = 0; i < W; i++) {
                for (int j = 0; j < H; j++) {
                        unsigned n = 1000 * i + j;
                        copy_unsigned(methods, array, i, j, n);
                        unsigned *p = methods->at(array, i, j);
                        assert(*p == n);
                }
        }
        //fprintf(stderr, "%s\n", "3");
        for (int i = 0; i < W; i++) {
                for (int j = 0; j < H; j++) {
                        unsigned n = 1000 * i + j;
                        unsigned *p = methods->at(array, i, j);
                        assert(*p == n);
                }
        }
        double_row_major_plus();
        //fprintf(stderr, "%s\n", "back in test");
        methods->free(&array);
        //fprintf(stderr, "%s\n", "freed again");
}

int main(int argc, char *argv[])
{
        
        
        assert(argc == 1);
        (void)argv;
        test_methods(uarray2_methods_plain);
        test_methods(uarray2_methods_blocked);
        printf("Passed.\n");
        
        //only if we reach this point without assertion failure
                               
        // (void)argc;
        // (void)argv;
        // UArray2b_T uA2b = UArray2b_new(4, 4, sizeof(int), 2);
        // int count = 0;
        // UArray2b_map(uA2b, setAll, &count);
        // printf("SETTING DONE\n");
        // UArray2b_map(uA2b, printBox, NULL);
        // printf("PRINTING DONE\n");
        // UArray2b_free(&uA2b);

        return 0;
        
}


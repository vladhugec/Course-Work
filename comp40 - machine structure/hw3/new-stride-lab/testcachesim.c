/******************************************************************/
/*                                                                */
/*                    testcachesim                                */
/*                                                                */
/*    Author: Noah Mendelsohn                                     */
/*                                                                */
/*    This is a simple example of a program that will let you     */
/*    experiment with the cachesim cache simulator.               */
/*                                                                */
/*    This program is also intended as a sample that you can      */
/*    modify to implement other tests and experiments. Mostly     */
/*    your modifications will be in the code marked with the      */
/*    ugly heavy comment boxes, but you may also need             */
/*    to change the main program or data declarations.            */
/*                                                                */
/*                                                                */
/******************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <limits.h>
#include <string.h>
#include <errno.h>

#include "cachesim.h"

/*
 * Arrays will be created with this element type
 */
#define ELEMENT_TYPE int

/*
 * Each element of the array will be visited this many times
 */
#define VISIT_COUNT 4


struct parsed_args {
        int line_count;
        int assoc;
        int block_size;
        int element_count;
        int stride;
        bool cache_sum;
};

struct parsed_args parse_args(int argc, char *argv[]);
void do_stuff(ELEMENT_TYPE *data, struct parsed_args *parsed_argsp);
static void print_report_header(unsigned int element_size, int element_count,
                                int stride, bool cache_sum);
int
strided_accesses(int element_count, int stride);

char *progname;    /* Global, but just for usage output */


/* ----------------------------------------------------------- 

                     Main program

   ----------------------------------------------------------- */


int
main(int argc, char *argv[])
{
        struct parsed_args parsed_args;
        ELEMENT_TYPE *data;

        /*
         * Parse command line and initialize data buffer
         */
        progname = argv[0];
        parsed_args = parse_args(argc, argv);
        data = malloc(parsed_args.element_count * sizeof(ELEMENT_TYPE));
        if (data == NULL) {
                fprintf(stderr, "ERROR: could not allocate data buffer\n");
                exit(EXIT_FAILURE);
        }

        /*
         * Initialize simulated cache
         */

        init_cachesim(parsed_args.line_count, parsed_args.assoc, 
                      parsed_args.block_size);
        cache_set_memory_origin(&data[0]); /* cache things mem starts here */


        /*
         * Read and write lots of data -- modify the
         * do_stuff function to cause different patterns
         * of access.
         */
        do_stuff(data, &parsed_args);

        /*
         * Print a report with cache summary statistics to stdout
         */
        print_report_header(sizeof(ELEMENT_TYPE), parsed_args.element_count, 
                            parsed_args.stride, parsed_args.cache_sum);
        print_cache_stats();

        print_cache_control();

        /*
         * clean up and leave
         */
        terminate_cachesim();             /* free simulator memory and
                                             close logging files if any */
        free(data);                       /* data buffer */

        

}

/**************************************************************/
/**************************************************************/
/**                                                          **/
/**                    do_stuff                              **/
/**                                                          **/
/**    Change this function any way you like to cause        **/
/**    different patterns of access to the simulated         **/
/**    memory!  Remember: data is already allocated          **/
/**    to hold an array of elements.                         **/
/**                                                          **/
/**    The type of each element in the                       **/
/**    array is ELEMENT_TYPE, which you                      **/
/**    can change by altering type #define near              **/
/**    the top of the program. So, each element is           **/
/**    sized to hold an instance of that type.               **/
/**                                                          **/
/**    Space has been allocated for the number of            **/
/**    elements in parsed_argsp -> element_count.            **/
/**                                                          **/
/**************************************************************/
/**************************************************************/


void 
do_stuff(ELEMENT_TYPE *data , struct parsed_args *parsed_argsp)
{
        int i;
        int j = 0;
        /* By putting the sum in a struct with a long, we
           force the whole structure to start on address multiple
           of 8. That increases the chance that s will be in the
           same cache line as the start of our data buffer, which
           from malloc will be a multiple of 16! */
        struct alignme {
                ELEMENT_TYPE s;                /* the actual sum */
                long force_align;
        } sum = {0,0};
        ELEMENT_TYPE temp_sum = 0;
        ELEMENT_TYPE retrieved_data = 0;   
        (void) retrieved_data; /* for when we're !NOISY */

        /* ------------------------------------------------------- */
        /*    THE FOLLOWING CODE IS JUST A SAMPLE OF WHAT YOU      */
        /*    CAN DO. MAKE NEW COPIES OF THIS SOURCE AND           */
        /*    CHANGE IT TO TRY DIFFERENT TESTS.                    */
        /* ------------------------------------------------------- */

        /*
         * Loop storing and retrieving data so that we can see
         * what the cache does. Use different 
         * loop lengths, data array sizes, data element sizes
         * etc. to see how well the cache handles them.
         */

        for (j = 0; j<VISIT_COUNT; j++) {
                for (i=0; i < parsed_argsp->element_count ; 
                     i+=parsed_argsp->stride) {
                        /*
                         * We always cache array accesses.
                         * Whether we also cache sum depends on command line switch
			 * -cachesum switch (default is NOT to cache the sum)
                         */
                        if (parsed_argsp->cache_sum) {
                                CACHED_SET(data[i], i*j);
                                /* Subtle detail: We need the temp below
                                   because, C compiler evals target of
                                   CACHED_SET before data value. So, the more
                                   obvious code which would be:
                                      CACHED_SET(sum.s,
                                               CACHED_GET(data[i]) +
                                               CACHED_GET(sum.s) ) 
                                   would mistakenly model write access to sum
                                   as coming before the read. */
                                temp_sum = CACHED_GET(data[i]) + CACHED_GET(sum.s);
                                CACHED_SET(sum.s, temp_sum);
                        } else {
                                CACHED_SET(data[i], i*j);
                                retrieved_data = CACHED_GET(data[i]);
                        }
                }
        }

}



/**************************************************************/
/**************************************************************/
/**                                                          **/
/**                Print Report Header                       **/
/**                                                          **/
/**                                                          **/
/**     Explain here in detail the test that your program    **/
/**     is running. By doing this, you will ensure           **/
/**     that every output report carries correct             **/
/**     information about the test.                          **/
/**                                                          **/
/**************************************************************/
/**************************************************************/

static void 
print_report_header(unsigned int element_size, int element_count,
                    int stride, bool cache_sum)
{
        int elements_accessed_per_iteration;

        printf("\n");
        printf("**************************************************************\n");
        printf("*            Stride Performance Test Results                 *\n");
        printf("**************************************************************\n\n");

        printf("Testing on %d array elements each of size %u bytes with stride %u.\n",
               element_count, element_size, stride );
        printf("Total array size is %d bytes.\n\n",
               element_count * element_size);


        if (element_count > 1) {
                printf("Elements in the the array are accessed with "
                       "write/read pairs.\n");
                if (stride >= element_count) {
                        printf("\nWARNING: stride %u is >= number of array "
                               "elements (which is %d).\n"
                               "Only arr[0] has been accessed!\n\n", 
                               stride, element_count);
                } else {
                        printf("This is done first on element[0], "
                               "then element [%d] and so on.\n", 
                               stride);
                }
                printf("The entire process is repeated %d times.\n\n",
                       VISIT_COUNT);
        } else {
                printf("The array has only one element and it is read/written "
                       "%d times.\n\n", VISIT_COUNT);
                if (stride > element_count) {
                        printf("WARNING: stride %u makes no sense with "
                               "1 element array!\n"
                               "Only arr[0] has been accessed!\n\n", 
                               stride);
                }
        }

        if (cache_sum) {
                printf("A running sum of array elements was maintained and simulated as cacheable.\n\n");
        } else {
                printf("No sum of array elements was maintained. All simulated cache traffic was from array accesses.\n\n");
        }

        


        elements_accessed_per_iteration = strided_accesses(element_count, stride);

        printf("Total element accesses: %d  (%d read and %d write)\n",
               elements_accessed_per_iteration * VISIT_COUNT * 2, 
               elements_accessed_per_iteration * VISIT_COUNT,
               elements_accessed_per_iteration * VISIT_COUNT); 
        printf("Total bytes accessed: %d\n\n",
               elements_accessed_per_iteration * VISIT_COUNT * element_size * 2);
}


/* number of accesses we'll make in one pass */
int
strided_accesses(int element_count, int stride)
{
        int full_strides = element_count/stride;
        int extras = element_count % stride;
        int retval = full_strides + ((extras>0)?1:0);

        return retval; 
}

/* ----------------------------------------------------------- 

                  Program usage and parameter parsing

   ----------------------------------------------------------- */


void
usage()
{
        fprintf(stderr, "Usage: %s  [ -assoc nnn ]  [ -stride nnn ]  [-cachesum] -lines nnn -blksize nnn   -elements nnnn \n", progname);
        exit(EXIT_FAILURE);
}

/* adapted from Linux man page for strtol */
/* NOTE: this routine rejects negative numbers, as it is intended 
   specifically for passing the command line args to this program */
int 
parse_int(const char *str)
{
        long val;
        char *endptr;

        errno = 0;    /* To distinguish success/failure after call */
        val = strtol(str, &endptr, 10);  /* base is 10 */

        /* Check for various possible errors */

        if ((errno == ERANGE && (val == LONG_MAX || val == LONG_MIN))
            || (errno != 0 && val == 0)) {
                perror("strtol");
                usage();
        }

        if ((endptr == str) || (endptr != strchr(str, '\0'))) {
                fprintf(stderr, "Non-numeric data found in numeric argument\n");
                usage();
        }

        if (val > INT_MAX || val < 0) {
                fprintf(stderr, "Number out of range\n");
                usage();
        }

        return (unsigned int)val;
}

 
struct parsed_args
parse_args(int argc, char *argv[]) 
{
        struct parsed_args ret;
        int arg; 

        ret.line_count = -1;
        ret.assoc = 1;
        ret.stride = 1;
        ret.block_size = -1;
        ret.element_count = -1;
        ret.cache_sum = false;             /* default to false */

        for(arg=1; arg < argc ; arg++)
        {
                if(!strcmp(argv[arg],"-lines"))
                {
                        arg++; /* need to test if arg can be incremented */
                        if (arg >= argc)
                                usage();
                        ret.line_count = parse_int(argv[arg]);
                        
                } else if(!strcmp(argv[arg],"-assoc"))
                {
                        arg++; /* need to test if arg can be incremented */
                        if (arg >= argc)
                                usage();
                        ret.assoc = parse_int(argv[arg]);
                        
                } else if(!strcmp(argv[arg],"-blksize"))
                {
                        arg++; /* need to test if arg can be incremented */
                        if (arg >= argc)
                                usage();
                        ret.block_size = parse_int(argv[arg]);
                        
                } else if(!strcmp(argv[arg],"-elements"))
                {
                        arg++; /* need to test if arg can be incremented */
                        if (arg >= argc)
                                usage();
                        ret.element_count= parse_int(argv[arg]);
                        
                } else if(!strcmp(argv[arg],"-stride"))
                {
                        arg++; /* need to test if arg can be incremented */
                        if (arg >= argc)
                                usage();
                        ret.stride= parse_int(argv[arg]);
                        
                } else if(!strcmp(argv[arg],"-cachesum"))
                {
                        ret.cache_sum = true;
                        
                } else {
                        fprintf(stderr, "Invalid argument %s\n", argv[arg]);
                        usage();
                }
        }       

        /* Make sure each one was explicitly set */
        if ( ret.line_count == -1 || ret.assoc == -1 || 
             ret.block_size == -1 || ret.element_count == -1) {
                usage();
        }

        
        return ret;
}

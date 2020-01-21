#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <math.h>
#include <errno.h>

#include "cachesim.h"

/*
 * Enough iterations will be done to do approximately this many
 * byte acccesses, unless that would lead to less than 5 iterations
 */

#define MBYTES_TO_ACCESS 20.0

struct parsed_args {
	int line_count;
	int assoc;
	int block_size;
	int stride;
	double megabytes;
};

static void print_report_header(double megabytes, int stride, int iter);
struct parsed_args parse_args(int argc, char *argv[]);
static void run_stride_tests(size_t bytes, int iter,
			     int stride, char *mem);
static char xor(const char *p, const char *limit, int stride);

char *progname;    /* Global, but just for usage output */

/* NEEDSWORK: the sink construction is carried over
   from the earlier stride.c exercise, but is probably
   not needed here. Nothing would get optimized away
   anyway (the old source said that making sure
   the looks didn't get optimized away was the
   reason for storing in sink */
static volatile int sink;	/* keeps result from being optimized away */

/**************************************************************/
/**************************************************************/
/**                                                          **/
/**                     main program                         **/
/**                                                          **/
/**    See the comment below on the do_stuff() function      **/
/**    for instructions on changing the memory access        **/
/**    patterns for your cache tests.                        **/
/**                                                          **/
/**************************************************************/
/**************************************************************/

int
main(int argc, char *argv[])
{
	struct parsed_args parsed_args;
	size_t bytes;
	double megabytes;
	char *p_mem;
	unsigned int stride;
	int iter; 
	
	/*
	 * Parse command line and initialize data buffer
	 */
	progname = argv[0];
	parsed_args = parse_args(argc, argv);
	stride = parsed_args.stride;
	megabytes = parsed_args.megabytes;


	/*
	 * Allocate the buffer to use in the test and make sure parms ok
	 */
	bytes = megabytes * 1024 * 1024;
	p_mem = malloc(bytes);

	if (p_mem == NULL) {
		if ((double)(size_t) megabytes != megabytes)
			fprintf(stderr, "%s: Cannot allocate %.2fMBytes\n",
				argv[0], megabytes);
		else
			fprintf(stderr, "%s: Cannot allocate %dMBytes\n",
				argv[0], (int)megabytes);
		exit(ENOMEM);
	}

	if (stride > (bytes-1)) {
		fprintf(stderr, "Error: stride %d cannot be greater "
			"than memory size %lu\n", stride, bytes-1);
		exit(EXIT_FAILURE);
	}

	/*
	 * Initialize simulated cache
	 */

	init_cachesim(parsed_args.line_count, parsed_args.assoc, 
		      parsed_args.block_size);
	cache_set_memory_origin(p_mem); /* cache things mem starts here */


	
	/*
	 * Read and write lots of data -- modify the
	 * do_stuff function to cause different patterns
	 * of access.
	 */
	iter = MBYTES_TO_ACCESS / megabytes;
	if (iter < 5)
		iter = 5;
	
	print_report_header(megabytes, stride, iter);

	run_stride_tests(bytes, iter, stride, p_mem);

	/*
	 * Print a report with cache summary statistics to stdout
	 */
	print_cache_stats();

	/*
	 * clean up and leave
	 */
	terminate_cachesim();             /* free simulator memory and
					     close logging files if any */
	free(p_mem);                       /* data buffer */

	

}


/**************************************************************/
/**************************************************************/
/**                                                          **/
/**               Do the striding through memory             **/
/**                                                          **/
/**************************************************************/
/**************************************************************/

static void run_stride_tests(size_t bytes, int iter,
			     int stride, char *mem)
{
	int i;

	/* NEEDSWORK: See comment above on definition of sink */
	for (i = 0; i < iter; i++)
		sink = xor(mem, mem + bytes, stride);



}

static char xor(const char *p, const char *limit, int stride)
{
	char sum = 0;
	int offset;
	const char *s;


	for (offset = 0; offset < stride; offset++)
		for (s = p + offset; s < limit; s += stride)
			sum ^= CACHED_GET(*s);;
	return sum;
}


/**************************************************************/
/**************************************************************/
/**                                                          **/
/**               Miscellaneous Support Routines             **/
/**                                                          **/
/**************************************************************/
/**************************************************************/

void
usage()
{
	fprintf(stderr, "Usage: %s   [ -assoc nnn ] [ -stride nn ] -lines nnn  -blksize nnn   -megabytes nn \n", progname);
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

 
double 
parse_double(const char *str)
{
	double val;
	char *endptr;

	errno = 0;    /* To distinguish success/failure after call */
	val = strtod(str, &endptr);  /* base is 10 */

	/* Check for various possible errors */
	/* NEEDSWORK: not sure the following check is quite right */

	if (errno == ERANGE || val == HUGE_VALF || val == HUGE_VALL
	    || (errno != 0 && val == 0)) {
		perror("strtold");
		usage();
	}

	if ((endptr == str) || (endptr != strchr(str, '\0'))) {
		fprintf(stderr, "Non-numeric data found in numeric argument\n");
		usage();
	}

	if (val < 0.0) {
		fprintf(stderr, "Number out of range\n");
		usage();
	}

	return val;
}

 
struct parsed_args
parse_args(int argc, char *argv[]) 
{
	struct parsed_args ret;
	int arg; 

	ret.line_count = -1;
	ret.assoc = 1;
	ret.block_size = -1;
	ret.stride = 1;
	ret.megabytes = -1.0;

	for(arg=1; arg < argc ; arg++)
	{
		if(!strcmp(argv[arg],"-lines"))
		{
			arg++; 
			if (arg >= argc)
				usage();
			ret.line_count = parse_int(argv[arg]);
			
		} else if(!strcmp(argv[arg],"-assoc"))
		{
			arg++; 
			if (arg >= argc)
				usage();
			ret.assoc = parse_int(argv[arg]);
			
		} else if(!strcmp(argv[arg],"-blksize"))
		{
			arg++; 
			if (arg >= argc)
				usage();
			ret.block_size = parse_int(argv[arg]);
			
		} else if(!strcmp(argv[arg],"-stride"))
		{
			arg++; 
			if (arg >= argc)
				usage();
			ret.stride= parse_int(argv[arg]);
			
			if (!(ret.stride > 0)) {
				fprintf(stderr, "Stride must be > 0\n");
				usage();
			}

		} else if(!strcmp(argv[arg],"-megabytes"))
		{
			arg++; 
			if (arg >= argc)
				usage();
			ret.megabytes= parse_double(argv[arg]);
			if (!(ret.megabytes > 0)) {
				fprintf(stderr, "Megabytes must be > 0\n");
				usage();
			}
		} else {
			fprintf(stderr, "Invalid argument %s\n", argv[arg]);
			usage();
		}
	}	

	/* Make sure each one was explicitly set */
	if ( ret.line_count == -1 || ret.assoc == -1 || 
	     ret.block_size == -1 || ret.megabytes == (-1.0)
	     || ret.stride == -1) {
		usage();
	}

	
	return ret;
}

static void
print_report_header(double megabytes, int stride, int iter)
{

	printf("\n");
	printf("**************************************************************\n");
	printf("*            Stride Performance Test Results                 *\n");
	printf("**************************************************************\n\n");

	printf("Testing on ");
	if ((double)(size_t) megabytes != megabytes) {
		if (megabytes > 0.1) {
			printf("%.2f MBytes", megabytes);
		} else {
			printf("%f MBytes", megabytes);
		} 
	} else {
		printf("%d MBytes",(int)megabytes);
	}
	
	printf(" of data with stride of %d bytes.\n\n", stride);

	printf("Each byte is read the same number of times, but\n");
	printf("addresses are incremented by %d before returning\n", stride);
	printf("to the first (previously) unaccessed byte in the array.\n");
	printf("No bytes are written to memory during these tests.\n\n");

	printf("The overall process is repeated such that a total of %.1f Mb\n",
	       megabytes * iter);
	printf("are accessed.\n\n");
}

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <assert.h>
#include <time.h>

static void run_stride_tests(size_t bytes, double megabytes,
			     int stride, char *mem);
static char xor(const char *p, const char *limit, int stride);
static void set_data(char *p, const char *limit);

static volatile int sink;	/* keeps result from being optimized away */

int main(int argc, char *argv[])
{
	double megabytes;
	int stride;
	size_t bytes;
	char *p_mem;

	if (argc != 3) {
		fprintf(stderr, "Usage: %s megabytes stride\n", argv[0]);
		exit(EINVAL);
	}
	megabytes = atof(argv[1]);	/* never use atoi() */
	stride = atoi(argv[2]);	/* never use atoi() */

	assert(megabytes > 0 && stride > 0);

	bytes = megabytes * 1024 * 1024;
	p_mem = malloc(bytes);

	if (p_mem == NULL) {
		if ((double)(size_t) megabytes == megabytes)
			fprintf(stderr, "%s: Cannot allocate %.2fMiB\n",
				argv[0], megabytes);
		else
			fprintf(stderr, "%s: Cannot allocate %dMiB\n",
				argv[0], (int)megabytes);
		exit(ENOMEM);
	}
	fprintf(stderr, "Setting initial data values\n");
	set_data(p_mem, p_mem + bytes);
	fprintf(stderr, "Running cache tests\n");
	run_stride_tests(bytes, megabytes, stride, p_mem);

	return 0;
}

static void set_data(char *p, const char *limit)
{
	long i;
	for (i=0; i < (limit-p); i++) {
		*(p+i) = i;
	}
}

static void run_stride_tests(size_t bytes, double megabytes,
			     int stride, char *mem)
{
	clock_t start = clock(), stop;
	int i, iter = 500 / megabytes;
	double loads, seconds;

	if (iter < 5)
		iter = 5;

	for (i = 0; i < iter; i++)
		sink = xor(mem, mem + bytes, stride);

	stop = clock();
	loads = (double)bytes *iter;
	seconds = (double)(stop - start) / (double)CLOCKS_PER_SEC;

	if ((double)(size_t) megabytes == megabytes)
		printf("%dMiB", (int)megabytes);
	else
		printf("%.2fMiB", megabytes);

	printf(" stride %d results in %5.2fns CPU time per load"
	       " (total %.3fs)\n", stride, seconds / 1e-9 / loads, seconds);
}

static char xor(const char *p, const char *limit, int stride)
{
	char sum = 0;
	int offset;
	const char *s;

	assert(stride > 0);

	for (offset = 0; offset < stride; offset++)
		for (s = p + offset; s < limit; s += stride)
			sum ^= *s;
	return sum;
}

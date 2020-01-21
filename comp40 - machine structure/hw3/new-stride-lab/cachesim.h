#ifndef CACHESIM_H_INCLUDED
#define CACHESIM_H_INCLUDED

/***************************************************************/
/*                                                             */
/*                    cachesim.h                               */
/*                                                             */
/*    Interface for an experimental cache simulator            */
/*                                                             */
/*    Author: Noah Mendelsohn                                  */
/*                                                             */
/*                                                             */
/*                                                             */
/***************************************************************/

#include <stdint.h>

typedef uint64_t cached_addr_t;

/*
 * Returned from get_cache_info
 */

struct cache {
	/* supplied when created */
	unsigned int line_count;
	unsigned int assoc;
	unsigned int block_size;

	/* computed from parms above */
	/*  (ptr & mask) >> shift gives integer value */
	unsigned int sets;
	cached_addr_t tag_mask;
	unsigned int tag_shift;
	cached_addr_t set_mask;
	unsigned int set_shift;
	cached_addr_t block_mask;

	/* simulated memory starts at this process address */
	void *origin;

	/* aggregate stats */
	unsigned int reads;
	unsigned int writes;
	unsigned int reads_for_writes; /* reads for partial line writes */
	unsigned int hits;
	unsigned int read_misses;
	unsigned int write_misses;
	unsigned int evictions;
	unsigned int writes_for_evictions;
	unsigned int dirty;

};

/* ----------------------------------------------------
            User-callable interfaces
   ---------------------------------------------------- */

/*
 *   Use these to simulate a read or a write of memory respectively
 */
#define CACHED_SET(var, value) (cache_set_field(&(var), sizeof(var)), var=(value))
#define CACHED_GET(var) (cache_get_field(&(var), sizeof(var)), (var))


/* 
 * Initalization, configuration and termination of the simulator
 */

void
init_cachesim(unsigned int line_count, unsigned int assoc, unsigned int block_size);

void
cache_set_memory_origin(const void *origin);

void
cachesim_reset_counters();

void
terminate_cachesim(void);

/* 
 * Write reports to stdout
 */

void
print_cache_stats();

void
print_cache_control();

/* 
 * Get or reset stats in program-readable structure
 */

extern struct cache cachesim_get_cache_info();
extern void cachesim_reset_counters();



/* ----------------------------------------------------
         Internal routines needed by macros above
   ---------------------------------------------------- */

extern void cache_set_field(const void *ptr, unsigned int len);
extern void cache_get_field(const void *ptr, unsigned int len);

#endif

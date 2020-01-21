/***************************************************************/
/*                                                             */
/*                    cachesim.c                               */
/*                                                             */
/*    A simple, experimental cache simulator.                  */
/*                                                             */
/*    Author: Noah Mendelsohn                                  */
/*                                                             */
/*                                                             */
/*    NOTE: as you would expect, block sizes, assoc & line     */
/*          count must be power of two. Line count             */
/*          must fit integral number of sets.                  */
/*          Hanson exceptions will be raised if these          */
/*          rules or other input constraints are violated.     */
/*                                                             */
/*    Set associativity was added Sept, 2014. The              */
/*    invariants associated with this are:                     */
/*                                                             */
/*    * Lines within set are filled highest index first        */
/*                                                             */
/*    * Lines never used are marked invalid                    */
/*                                                             */
/*    * Most recently accessed lines are in lowest index       */
/*      and are moved to that position when a hit              */
/*      occurs.                                                */
/*                                                             */
/*    As an optimization, lines are searched low to hi         */
/*    anyway because once things get going we hope             */
/*    for hits on recent stuff                                 */
/*                                                             */
/*    NEEDSWORK: Always simulates a store-in cache             */
/*          Framework is there to extend to your hoice.        */
/*          For now, lines are kept DIRTY until evicted        */
/*          If a dirty line is evicted, the "writes_for_reads" */
/*          counters are incremented. (And by the way,         */
/*          the reads_for_writes counter tracks cases          */
/*          in which a short write requires the                */
/*          rest of the line to be filled from memory first.   */
/*                                                             */
/*    NEEDSWORK: Most routines should be static, or            */
/*          if moved to other source files should              */
/*          be named to avoid conflict w/user code.            */
/*                                                             */
/*                                                             */
/*    NEEDSWORK: Does not currently simulate accesses that     */
/*          involve more than one cache line                   */
/*                                                             */
/***************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <inttypes.h>
#include <except.h>
#include <assert.h>
#include "cachesim.h"

/* Both of the following are to enable testing output */
/* #define DEBUG */
/* #define PRINT_LINES */     /* print state of each cache set as affected */

/* ------------------------------------------------------------------*/
/*                     Constants                                     */
/* ------------------------------------------------------------------*/

/* default simulated memory read and write times in nanoseconds */
/* These are used to estimate running times in the final report */

#define HIT_TIME  1   
#define READ_ACCESS_TIME  100   
#define WRITE_ACCESS_TIME  130   


/* ------------------------------------------------------------------*/
/*       Macros for isolating fields in an address                   */
/* ------------------------------------------------------------------*/


/* Extract a bit field given mask that selects it and corresponding shift */
#define ISOLATE(ptr, mask, shift) ((((cached_addr_t)(ptr)) & (mask)) >> shift)

/* note: tags are never shifted...sets are */
#define TAG(ptr) ((ptr) & cache.tag_mask)
#define SET(ptr) (ISOLATE((ptr), cache.set_mask, cache.set_shift)) 
#define BLOCK(ptr) ((ptr) | cache.block_mask) 

#define MISS (-1)

/* ------------------------------------------------------------------*/
/*                     Debugging Output                              */
/* ------------------------------------------------------------------*/

/* WARNING: following uses GNU extension */
#ifdef DEBUG
#define DEBUGOUT(...) fprintf(stderr, __VA_ARGS__)
#else
#define DEBUGOUT(...) 
#endif

#ifdef PRINT_LINES
#define LINEPRINT(...) fprintf(stderr, __VA_ARGS__)
#else
#define LINEPRINT(...) 
#endif


/* ------------------------------------------------------------------*/
/*                     Assertions and Exceptions                     */
/* ------------------------------------------------------------------*/

/*
 * Bad user input
 *
 *   Each call to INPUT_ASSERT provides:
 *
 *   boolean test: should be true if input is good
 *   name of routine in which error occurs for debug output
 *   msg1 (required)  char * or string literal
 *   msg2 (optional - NULL if not provided)
 *
 *   The two messages are written one after the other.
 *   msg2 is typically used to name a field causing errors.
 */
 
Except_T Bad_Input = {"Invalid input parameter"};

#define INPUT_ASSERT(test, routine, msg, msg2)                    \
        if (!(test)) {                                            \
                fprintf(stderr, "%s %s %s\n",                     \
                        routine, msg, ((msg2==NULL) ? "" : msg2));\
                RAISE(Bad_Input);                                 \
        }                                                         


/*
 * internal invariant violation
 *
 *   Each call to INTERNAL_ASSERT provides:
 *
 *   boolean test: should be true if input is good
 *   name of routine in which error occurs for debug output
 *   msg1 (required)  char * or string literal
 *   msg2 (optional - NULL if not provided)
 *
 *   The two messages are written one after the other.
 *   msg2 is typically used to name a field causing errors.
 *
 *   For performance, these checks will be suppressed if
 *   -DNO_INTERNAL_CHECKS is specified on the compilation
 */
 
Except_T Internal_Error = {"Internal consistency check fails"};

#ifdef NO_INTERNAL_CHECKS
#define INTERNAL_ASSERT(test, routine, msg, msg2)       
#else
#define INTERNAL_ASSERT(test, routine, msg, msg2)                         \
        if (!(test)) {                                                    \
                fprintf(stderr, "INTERNAL CONSISTENCEY ERROR%s %s %s\n",  \
                        routine, msg, ((msg2==NULL) ? "" : msg2));        \
                RAISE(Internal_Error);                                    \
        }                                                         
#endif



/* ------------------------------------------------------------------*/
/*              Data Structures Representing the Cache               */
/* ------------------------------------------------------------------*/


/* Overall state of the cache */
struct cache cache;      /* NEEDSWORK: should avoid global vars */


/* dynamically allocated array of line descriptors */
struct cacheline *cache_lines;

struct cacheline {
        cached_addr_t tag;                   /* not shifted but masked */
        unsigned int reads;
        unsigned int writes;
        unsigned int reads_for_writes;   
        unsigned int hits;
        unsigned int evictions;
        unsigned int writes_for_evictions;

        /* Note: initial value of all flags will be zero -- 
           define any new flags accordingly */
        unsigned char flags; 
#define CACHE_LINE_VALID 0x01
#define CACHE_LINE_DIRTY 0x02
};

/* ------------------------------------------------------------------*/
/*          Prototypes - forward references to local functions       */
/* ------------------------------------------------------------------*/

void init_control_fields(unsigned int line_count, 
                         unsigned int assoc, unsigned int block_size); 

void init_cache_lines(unsigned int line_count);
void free_cache_lines(void);
static unsigned int single_bit_position(cached_addr_t input, char *debug_field_name);

#ifdef PRINT_LINES
void
print_set(int set, struct cacheline *clp);
#endif



/* ------------------------------------------------------------------*/
/*              Initialization & Termination Functions               */
/* ------------------------------------------------------------------*/


/*
 *                       init_cachesim
 *
 *      Initializes a cache simulation. Supplied block size is in bytes
 *      and must be a power of two.
 */
void
init_cachesim(unsigned int line_count, unsigned int assoc, unsigned int block_size)
{
        init_control_fields(line_count, assoc, block_size);
        init_cache_lines(line_count);

}


/*
 *                       terminate_cachesim
 *
 *      Frees all resources, but does no reporting.
 */
void
terminate_cachesim(void)
{
        free_cache_lines();

}


void
init_control_fields(unsigned int line_count, unsigned int assoc, 
                    unsigned int block_size)
{
        unsigned int sets; /* same as cache.sets but we use this often */


        /*
         * record input parameters
         */
        cache.line_count = line_count;
        INPUT_ASSERT(line_count > 0, "cachesim:init_control_fields",
                     "Number of cache lines must be > 0",
                     NULL);

        cache.assoc      = assoc;
        INPUT_ASSERT(assoc <= line_count, "cachesim:init_control_fields",
                     "Associativity must be less than line count",
                     NULL);

        cache.block_size = block_size;
        INPUT_ASSERT(block_size > 0, "cachesim:init_control_fields",
                     "block_size must be > 0", NULL);

        /*
         * Origin - logical start of memory
         */
        cache.origin = NULL;     /* NEEDSWORK: Assumes NULL == 0 as we
                                    do subtraction on this later */


        /*
         * compute useful constants, masks and shift values from input
         */

        sets = line_count / assoc;
        INPUT_ASSERT(line_count == (sets * assoc),  "cachesim:init_control_fields",
                     "Linecount is must be an even multiple of associativity",NULL);

        cache.sets = sets;

        cache.block_mask = block_size-1; 
       
        cache.set_shift = single_bit_position(block_size, "block_size");
        cache.set_mask = (cache.sets-1) << cache.set_shift;
        cache.tag_mask = ~((cached_addr_t)((block_size * sets) - 1));
        cache.tag_shift = single_bit_position(block_size * sets, 
                                           "block_size * sets (Note: sets is "
                                           "linecount / assoc)");

        DEBUGOUT("DEBUG> init_cachesim line_count=%u assoc=%u "
                 "block_size=%u sets=%u\n", 
                 line_count, assoc, block_size, cache.sets);
        DEBUGOUT("DEBUG> init_cachesim block_mask=%lx\n",
                   cache.block_mask);
        DEBUGOUT("DEBUG> init_cachesim set_mask=%lx cache.set_shift=%u\n",
                   cache.set_mask, cache.set_shift);
        DEBUGOUT("DEBUG> init_cachesim tag_mask=%lx cache.tag_shift=%u\n",
                   cache.tag_mask, cache.tag_shift);

        /*
         * Invariant check: mask bits must be disjoint and
         * must when "or'd" together fill the 64 bit word.
         */

        INTERNAL_ASSERT((cache.set_mask & cache.block_mask) == 0,
                        "cachesim:init_cachesim", 
                        "set mask and block mask overlap",
                        NULL);
        INTERNAL_ASSERT((cache.set_mask & cache.tag_mask) == 0,
                        "cachesim:init_cachesim", 
                        "set mask and tag mask overlap", 
                        NULL);
        INTERNAL_ASSERT((cache.block_mask & cache.tag_mask) == 0,
                        "cachesim:init_cachesim", 
                        "tag_mask and block mask overlap", 
                        NULL);

	/* Note: some of the seemingly extra casts in the expression
           below are needed because of the promotions from 16 to 32
	   bits that C does before most arithmetic and bit operators are
	   applied. The casts are not needed if cached_addr_t is >= 32,
	   but for smaller addresses the promotions can cause compilation
	   errors without the casts. */

        INTERNAL_ASSERT((((cached_addr_t)(cache.block_mask | cache.set_mask | 
					  cache.tag_mask)) == 
                         (cached_addr_t)(~((cached_addr_t)0))),
                        "cachesim:init_cachesim", 
                        "block set and tag masks don't fill addr bits", 
                        NULL);

        /*
         * Tracking structs for individual lines will be allocated later
         */
        cache_lines = NULL;

        /*
         * Set performance countes to zero
         */
        cachesim_reset_counters();


}

/*
 *                  cachesim_reset_counters
 *
 *       Called internally but also available to users, e.g.
 *       for testing scenarios where you don't want to count
 *       misses while "warming" the cache.
 */
void
cachesim_reset_counters()
{
        cache.hits = 0;
        cache.read_misses = 0;
        cache.write_misses = 0;
        cache.reads = 0;
        cache.writes = 0;
        cache.evictions = 0;
        cache.dirty = 0;
        cache.reads_for_writes = 0;
        cache.writes_for_evictions = 0;
}

/*
 *                     init_cache_lines
 *
 *   Allocate array of cache lines and initialize fields in them
 */

void
init_cache_lines(unsigned int line_count)
{
        unsigned int i;
        struct cacheline *clp;

        cache_lines = malloc(sizeof(struct cacheline) * line_count);
        INTERNAL_ASSERT((cache_lines != NULL),
                        "cachesim:init_init_cache_lines",
                        "Failed to allocate memory for simulated cache lines",
                        NULL);
        /* INTERNAL_ASSERT gives better messages, but can be compiled
           out...make sure we fail either way */
        assert(cache_lines != NULL);

        for (i = 0; i < line_count; i++ ) {
                clp = &(cache_lines[i]);
                clp -> tag = 0;
                clp -> reads = 0;
                clp -> writes = 0;
                clp -> hits = 0;
                clp -> evictions = 0;
                clp -> writes_for_evictions = 0;
                clp -> reads_for_writes = 0;
                clp -> flags = 0;
        }
}

/*
 *                     free_cache_lines
 *
 *   Release data associated with cache lines but not 
 *   control information common to the whole cache.
 */

void
free_cache_lines(void)
{
        if (cache_lines != NULL)
                free(cache_lines);
        cache_lines = NULL;
}

/*
 *      The address supplied here will be taken as logical
 *      address zero. The cache will treat addresses as 
 *      being relative to this. Default is zero.
 *
 *      This is useful if you're experimenting with, say
 *      an array, and want the start of the array to
 *      be cached in the first line(s) of the cache.
 */
void
cache_set_memory_origin(const void *origin)
{
        DEBUGOUT("DEBUG> set_memory_origin = %p\n",
                   origin);

        cache.origin = (void *)origin;
}

/* ------------------------------------------------------------------*/
/*        Routines to find or allocate cache line for an address     */
/* ------------------------------------------------------------------*/

/*
 *                          first_line_of_set
 *
 * For a given address, return the index of the first
 * cache line for the set in which the line can be cached
 */
int
first_line_of_set(cached_addr_t addr)
{
        unsigned int set = SET(addr);
        unsigned int set_start = set * cache.assoc;

        DEBUGOUT( "DEBUG> first_line_of_set addr=%lx set_start=%u\n",
                   addr, set_start);

        return set_start;
}

/*
 *                    hit_line
 *
 * Return index of line containing suppiled address,
 * or MISS if the line is not in the cache.
 *
 * As a side effect, this ensures that the hit line
 * is in the lowest index in the set of any used line.
 */
int
hit_line(cached_addr_t addr)
{
        cached_addr_t addr_tag;          /* tag bits from supplied address */
        struct cacheline *clp;
        struct cacheline temp_cacheline; 
        unsigned int i;
        unsigned int first_line_in_set;
        int first_valid_line = 0;      
        int this_line;
        bool found_valid_line = false;

        addr_tag = addr & cache.tag_mask;

        DEBUGOUT("DEBUG> hit_line addr=%lx addr_tag=%lx ",
                   addr, addr_tag);

	/*
	 * Search each line in the set for a hit
         */
        first_line_in_set = first_line_of_set(addr);
        for (i = 0; i < cache.assoc; i++ ) {
                this_line = first_line_in_set + i;
                clp = &(cache_lines[this_line]);
                /* 
		 * note first valid line in set if any 
		 */
                if ((!found_valid_line) && 
                    (clp -> flags & CACHE_LINE_VALID)) {
                        first_valid_line = first_line_in_set + i;
                        found_valid_line = true;
                }
                if (clp -> tag == addr_tag) {
                        if (clp -> flags & CACHE_LINE_VALID) {
				/*
				 * Cache hit
				 */
                                DEBUGOUT( "DEBUG> hit_line addr=%lx HIT=%u\n",
                                   addr, this_line);
                                /* if hit on less recently used */
                                /* promote the line to first_valid line
                                   and demote others */
                                if (first_valid_line != this_line) {
                                DEBUGOUT( "DEBUG> hit_line promoting slot %u to %u\n",
                                          this_line, first_valid_line);
                                        temp_cacheline = 
                                                cache_lines[this_line];
                                        memmove(&cache_lines[first_valid_line + 1],
                                               &cache_lines[first_valid_line],
                                               sizeof(struct cacheline) * 
                                               (this_line - first_valid_line));
                                        cache_lines[first_valid_line] = temp_cacheline;
                                        
                                }
                                return first_valid_line;
                        } else {
                                /* Invalid lines never result in hits 
                                   could be accidental tag match, probably
                                   to zeros */
                                DEBUGOUT( "DEBUG> hit_line addr=%lx MATCHED "
                                  "BUT INVALID=%u\n",
                                   addr, this_line);
                        }

                }
        }

	/*
	 * Cache Miss
         */
        DEBUGOUT("DEBUG> hit_line addr=%lx MISS!\n",
                addr);
        return MISS;
}

/*
 *                            victim
 *
 *      Given an address known not to be in the cache,
 *      find a victim line. 
 *
 *      As side effect, victim slot will always be the lowest index unused
 *      slot. Two cases:
 *          1) There are free slots: in this case the highest indexed
 *             free slot is used.
 *          2) There are no free slots. We'll give out slot 0, but we
 *             must push down the other n-1 slots, dropping the LRU
 *
 */

int
victim(cached_addr_t addr)
{
        bool found_valid = false;
        unsigned int valid_line;
        int victim = 0;
        struct cacheline temp_cacheline; 
        struct cacheline *clp;
        int this_line;
        int first_line_in_set = first_line_of_set(addr);

        DEBUGOUT("DEBUG> victim addr=%lx\n",
                   addr);

        /*
         * Find valid slot with lowest index, if any
         */
        for (valid_line = 0; valid_line < cache.assoc; valid_line++ ) {
                this_line = first_line_in_set + valid_line;
                clp = &(cache_lines[this_line]);
                if (clp -> flags & CACHE_LINE_VALID) {
                        found_valid = true;
                        DEBUGOUT("DEBUG>     victim: FOUND_VALID - "
				 "lowest valid is=%d\n",
                                 valid_line);

                        break;
                }               
                
        }

        /*
         * Either !found_valid or valid_line is lowest valid line.
         * Choose victim as either highest invalid line
         * or line 0
         */
        if (found_valid) {
                if (valid_line > 0) {
                        /* 
			 * set partially full- assign an unused line 
			 */
                        victim = first_line_in_set + valid_line -1;
                        DEBUGOUT("DEBUG>     victim: partially full victim=%d\n",
                                 victim);
                } else {
                        /* 
			 * Whole set full of valid entries;
                         * last entry will become the first and turn
                         * into the victim. Once it's victimized
                         * it will be the Most Recently Used 
			 */
                        if (cache.assoc > 1) {
                                temp_cacheline = cache_lines[first_line_in_set + 
                                                             cache.assoc - 1]; 
                                                             /* last */
                                /* move everything down one slot */
                                memmove(&cache_lines[first_line_in_set + 1],
                                        &cache_lines[first_line_in_set],
                                        sizeof(struct cacheline) * (cache.assoc - 1));
                                cache_lines[first_line_in_set] = temp_cacheline; 
                                                             /* victim */
                        }
                        victim = first_line_in_set;
                        DEBUGOUT("DEBUG>     victim: whole set valid victim=%d ",
                                 first_line_in_set + cache.assoc - 1);
                                 
#ifdef DEBUG
                        if (cache.assoc > 1) {
                                DEBUGOUT("moved to slot %d\n", first_line_in_set);
                        }
#endif 
                        DEBUGOUT("\n");

                }
        } else {
                /* 
		 * Entire set was invalid 
		 */
                victim = first_line_in_set + cache.assoc - 1;
                DEBUGOUT("DEBUG>     victim: whole set invalid victim=%d\n",
                         victim);
        }

        return victim;
}

/* ------------------------------------------------------------------*/
/*                                                                   */
/*              Simulated User Data Access Routines                  */
/*                                                                   */
/*                                                                   */
/*     These record cache state changes for supplied access          */
/*     requests (set and get).                                       */
/*                                                                   */
/*     NEEDSWORK: common code in _get_ and _set_ routines            */
/*     should be factored and shared.                                */
/*                                                                   */
/* ------------------------------------------------------------------*/

void 
cache_set_field(const void *ptr, unsigned int len) 
{
        int line_number;
        int victim_line;
        struct cacheline *clp;

        /*
         * Relocate supplied address relative to simulated start of memory
         */
        cached_addr_t simulated_ptr = (cached_addr_t)((char *)ptr - 
                                                      (char *)(cache.origin));
        cached_addr_t simulated_tag = TAG(simulated_ptr);

#ifdef PRINT_LINES
        LINEPRINT("SET(%u[0x%x], %u) ", (unsigned int)simulated_ptr, 
                                        (unsigned int)simulated_ptr, len);
#endif 
        DEBUGOUT("\n\nDEBUG> cache_set_field ptr=%p simulated_ptr=%lx len=%u\n",
                 ptr, simulated_ptr, len);

        /*
         * Make sure this reference fits into one cache line
         */
        INPUT_ASSERT(simulated_tag == TAG(simulated_ptr + len -1),  
                     "cachesim:cache_set_field",
                     "Accessing fields that cross cache line boundarids " 
                     "is not supported in this version of the simulator",
                     NULL);


        /*
         * Get index of cache line or else MISS
         *
         * NEEDSWORK: is cast below redundant? Can't see why not
         *            but reluctant to break something.
         */
        line_number = hit_line((cached_addr_t)simulated_ptr);

        /*
         * If it's a miss - if line has valid data,  record the eviction
         * If write doesn't fill the block, then mark a read_for_eviction first
         */
        if (line_number == MISS) {
                cache.write_misses++;
                victim_line = victim((cached_addr_t)simulated_ptr);
                DEBUGOUT("    (back in cache_set_field) MISS victim=%d ", 
                         victim_line);
                clp = &(cache_lines[victim_line]);
                if ((clp -> flags) & CACHE_LINE_VALID) {
                        clp -> evictions++;
                        cache.evictions++;
                        DEBUGOUT("evictions=%d ", clp -> evictions);
                        if ((clp -> flags) & CACHE_LINE_DIRTY) {
                                /* model writing out the old data to memory */
                                cache.dirty--;  /* but about to dirty it again */
                                clp -> writes_for_evictions++;
                                cache.writes_for_evictions++;
                                DEBUGOUT("writes_for_evictions=%d ", 
                                         clp -> writes_for_evictions);
                        }
                }
                clp->flags |= (CACHE_LINE_VALID | CACHE_LINE_DIRTY);
                cache.dirty++;
                /* if not full block write, then we need to read it first */
                if (len != cache.block_size) {
                        clp -> reads_for_writes++;
                        cache.reads_for_writes++;
                        DEBUGOUT("reads_for_writes=%d ", 
                                clp -> reads_for_writes);
                }
                clp -> tag = simulated_tag;
#ifdef PRINT_LINES
                LINEPRINT(" MISS\n");
#endif 
        /*
         * Cache hit
         */
        } else {                          /* cache hit in line_number */
                clp = &(cache_lines[line_number]);
                cache.hits++;
                DEBUGOUT("HIT line_number=%d ", line_number);
#ifdef PRINT_LINES
                LINEPRINT(" HIT\n");
#endif 
        }

        clp -> writes++;
        cache.writes++;
        DEBUGOUT("writes=%u tag=%lx\n", clp -> writes, clp -> tag);
#ifdef PRINT_LINES
        print_set(SET(simulated_ptr), &(cache_lines[first_line_of_set(simulated_ptr)]));
#endif
}

void 
cache_get_field(const void *ptr, unsigned int len) 
{
        int line_number;
        int victim_line;
        struct cacheline *clp;
        (void)len;                    /* unused when debug output turned off */


        /*
         * Relocate supplied address relative to simulated start of memory
         */
        cached_addr_t simulated_ptr = (cached_addr_t)((char *)ptr - (char *)(cache.origin));
        cached_addr_t simulated_tag = TAG(simulated_ptr);
#ifdef PRINT_LINES
        LINEPRINT("GET(%u [0x%x], %u) ", (unsigned int)simulated_ptr, 
                                         (unsigned int)simulated_ptr, len);
#endif 

        DEBUGOUT("\n\nDEBUG> cache_get_field ptr=%p simulated_ptr=%lx len=%u\n",
                 ptr, simulated_ptr, len);

        /*
         * Make sure this reference fits into one cache line
         */
        INPUT_ASSERT(simulated_tag == TAG(simulated_ptr + len -1),  
                     "cachesim:cache_get_field",
                     "Accessing fields that cross cache line boundarids " 
                     "is not supported in this version of the simulator",
                     NULL);

        /*
         * Get index of cache line or else MISS
         */
        line_number = hit_line((cached_addr_t)simulated_ptr);

        /*
         * If it's a miss - if line has valid data,  record the eviction
         * If write doesn't fill the block, then mark a read_for_eviction first
         */
        if (line_number == MISS) {
                cache.read_misses++;
                victim_line = victim((cached_addr_t)simulated_ptr);
                DEBUGOUT("    (back in cache_get_field) MISS victim=%d ", 
                         victim_line);
                clp = &(cache_lines[victim_line]);
                if ((clp -> flags) & CACHE_LINE_VALID) {
                        clp -> evictions++;
                        cache.evictions++;
                        DEBUGOUT("evictions=%d ", clp -> evictions);
                        if ((clp -> flags) & CACHE_LINE_DIRTY) {
                                /* model writing out the old data to memory */
                                cache.dirty--;  /* but about to dirty it again */
                                clp -> writes_for_evictions++;
                                cache.writes_for_evictions++;
                                DEBUGOUT("writes_for_evictions=%d ", 
                                         clp -> writes_for_evictions);
                        }
                }
                clp->flags |= (CACHE_LINE_VALID );
                /* not dirty, we're just loading it now */
                clp->flags &=  ~(CACHE_LINE_DIRTY );
                clp -> tag = simulated_tag;
#ifdef PRINT_LINES
                LINEPRINT(" MISS\n");
#endif 
        /*
         * Cache hit
         */
        } else {                          /* cache hit in line_number */
                clp = &(cache_lines[line_number]);
                cache.hits++;
                DEBUGOUT("HIT line_number=%d ", line_number);
#ifdef PRINT_LINES
                LINEPRINT(" HIT\n");
#endif 
        }

        clp -> reads++;
        cache.reads++;
        DEBUGOUT("reads=%u tag=%lx\n", clp -> reads, clp -> tag);
#ifdef PRINT_LINES
        print_set(SET(simulated_ptr), &(cache_lines[first_line_of_set(simulated_ptr)]));
#endif
}


/* ------------------------------------------------------------------*/
/*                    Bit Manipulation Routines                      */
/* ------------------------------------------------------------------*/

/*
 *   single_bit_position
 *
 *   Correct input: cached_addr_t with exactly one bit on (I.e. power of 2)
 *
 *   Output: offset from low order bit position (I.e. right shift
 *   value required to move that bit to the one's place)
 *
 *   Checked runtime error: if input is not a power of two
 */

static unsigned int
single_bit_position(cached_addr_t input, char *debug_field_name) 
{
        unsigned int output = 0;

        /* move bit to one's place and count */
        while ((input & 1) == 0) {
                input = input >> 1;
                output++;
        }

        /* make sure it was the only bit */
        INPUT_ASSERT(input == 1, "cachesim:single_bit_position", 
                     "Must be power of two but is not:", 
                     debug_field_name);

        return output;    
}

/* ------------------------------------------------------------------*/
/*      Provide copy of cache info including performance stats       */
/* ------------------------------------------------------------------*/

struct cache cachesim_get_cache_info()
{
        return cache;          /* Returns a copy of all cash stats */
}



/* ------------------------------------------------------------------*/
/*          Statistics output functions                              */
/* ------------------------------------------------------------------*/

void
print_ns_time(unsigned long t)
{
        bool show_decimals = false;
        char *units = "ns";
        double t_double;


        /* NEEDSWORK: Following tries to keep things as integers as long
           as possible, but that's probably overkill. */

        /*
         * Set units to a suitble units string
         * Set t to be in thousands of those units
         */

        if (t > 1000) {
                /* no division here: we'll do this one in floating below */
                units = "us";
                show_decimals = true;
                if (t > 1000000) {  /* we want three not six decimal places */
                        t /= 1000;   
                        units = "ms";
                        if (t > 1000000) {
                                t /= 1000;   
                                units = "sec";
                        }
                }

        }

        /*
         * If original input number didn't get scaled
         * at all, then we don't have decimal places to show.
         * If it did, then because t is in thousands of
         * units, we do a double division to scale it properly
         * as a decimal fraction.
         */
        if (show_decimals) {
                t_double = ((double)t) / 1000.0;
                printf("%.2f%s", t_double, units);
        } else {
                printf("%lu%s", t, units);
        }
}

void
print_cache_stats()
{
        unsigned long hit_time, read_time, write_time;
        unsigned long total_with_cache, total_no_cache;
        unsigned int accesses;

        double ratio;

        printf("\n");
        printf("==============================================================\n");
        printf("          Aggregate Cache and Performance Statistics\n");
        printf("==============================================================\n\n");


        printf("Simulated Cache Characteristics:\n");
        printf("--------------------------------\n\n");

        printf("Lines=%u  Associativity=%u %s  Block_size=%u\n",
               cache.line_count, cache.assoc, ((cache.assoc==1)?"(direct mapped)":
                                               ((cache.assoc == cache.line_count) ?
                                                "(fully associative)" : "")),
               cache.block_size);
        printf("Total cache size is: %u bytes\n\n", 
               cache.line_count * cache.block_size);
        printf("The simulated cache is a \"store in\" type: "
               "dirty blocks are not written\n");
        printf("to memory until evicted, at which time a \"write_for_eviction\"\n");
        printf("is recorded. When a line is written partially, then a\n");
        printf("\"read_for_write\" accounts the need to read the line from\n");
        printf("memory before updating it.\n\n");

        printf("Performance Results:\n");
        printf("--------------------\n\n");


        accesses = cache.hits + cache.read_misses + cache.write_misses;
        printf("Accesses=%u  Hits=%u (%.1f%%)\nRead Misses=%u (%.1f%%) "
               "Write Misses=%u (%.1f%%)  Evictions=%u\n",
               accesses,
               cache.hits, 
               (cache.hits == 0) ? 0.0 :(float)(cache.hits * 100)/ (float)accesses,
               cache.read_misses, 
               (cache.read_misses == 0) ? 0.0 : (float)(cache.read_misses * 100) / 
                                                (float)cache.reads,
               cache.write_misses, 
               (cache.write_misses == 0) ? 0.0 : (float)(cache.write_misses * 100)/
                                                 (float)cache.writes,
               cache.evictions);
        printf("Reads=%u  Writes=%u  "
               "Reads_for_writes=%u  Writes_for_evictions=%u\n",
               cache.reads, cache.writes, 
               cache.reads_for_writes, cache.writes_for_evictions);

        printf("Dirty blocks remaining=%u\n\n",
               cache.dirty);

        /*
         * Estimate memory service times in nanoseconds with cache
         *
         * Make sure all arithmetic done in longs
         */

        hit_time = cache.hits;
        hit_time *= HIT_TIME;
        printf("Time for hits=");
        print_ns_time(hit_time);

        read_time = cache.read_misses + cache.reads_for_writes;
        read_time *= READ_ACCESS_TIME;
        printf("  Time for memory reads=");
        print_ns_time(read_time);

        write_time = cache.writes_for_evictions + cache.dirty;
        write_time *= WRITE_ACCESS_TIME;
        printf("\nTime for memory writes (including flushing dirties)=");
        print_ns_time(write_time);


        /*
         * Output cached vs. non-cached comparisons
         */

        printf("\nEstimated total memory access time:   With caching=");
        total_with_cache = hit_time + read_time + write_time;
        print_ns_time(total_with_cache);

        printf("   No caching=");
        total_no_cache = ((unsigned long)cache.reads * 
                          (unsigned long)READ_ACCESS_TIME);
        total_no_cache += ((unsigned long)cache.writes * 
                          (unsigned long)WRITE_ACCESS_TIME);
        print_ns_time(total_no_cache);

        ratio = ((double) total_with_cache) / ((double) total_no_cache);

        if (ratio < 1.05 && ratio > .95)
                printf("\nNo significant difference in performance due to caching.");
        else if (ratio < 1.0)
                printf("\nSpeedup: %.1fx", 1.0/ratio);
        else
                printf("\nSlowdown: %.1fx", ratio);

        printf("\n\n");

}

/* ------------------------------------------------------------------*/
/*               Debug output functions                              */
/* ------------------------------------------------------------------*/

#ifdef PRINT_LINES
void
print_line(int n, struct cacheline *clp)
{
        unsigned char flags = clp->flags;
        LINEPRINT("%d: Tag=%d [%c] || ", n, (int)clp->tag,
                  (flags&CACHE_LINE_VALID ? 
                   ((flags & CACHE_LINE_DIRTY) ? 'D' : 'V')  :
                   'I'));
}


/* 
 * Print the internal cache control state for the 
 * the given set, the first line of which is supplied 
 */
void
print_set(int set, struct cacheline *first_line_p)
{
        unsigned int i;

        LINEPRINT("Set: %d\n", set);

        for(i=0; i < cache.assoc; i++) {
                print_line(i, first_line_p++);
        }

        LINEPRINT("\n");
}
#endif

/* ------------------------------------------------------------------*/
/*           Print individual cache line state bits                  */
/*                                                                   */
/*     Valid, dirty, tag bits                                        */
/*                                                                   */
/*     NEEDSWORK: this is intended for end-user use but somewhat     */
/*                overlaps with debug functions above.               */
/*                                                                   */
/* ------------------------------------------------------------------*/


/*
 * printbits
 *
 * Print the low order nbits bits from the supplied value n
 */

static
void printbits(uint64_t n, int nbits)
{
	while (--nbits >= 0) {
		putchar((n >> nbits) & 1 ? '1' : '0');
	}
}

/*
 *    Added for comp40 2014 spring final to confirm contents of cache
 * after simulation
 */
void
print_cache_control()
{
	unsigned int i;
	struct cacheline *clp;
	int tag_bits = (sizeof(cached_addr_t) * 8) - cache.tag_shift;

	printf("\n");
	printf("==============================================================\n");
	printf("          Contents of individual cache line controls\n");
	printf("==============================================================\n\n");


	INTERNAL_ASSERT((cache_lines != NULL),
		        "cachesim:init_init_cache_lines",
			"Print_cache_control: failed to find memory "
			"for simulated cache lines",
			NULL);
	/* INTERNAL_ASSERT gives better messages, but can be compiled
	   out...make sure we fail either way */
	assert(cache_lines != NULL);

	printf("Tag has %d bits\n\n", tag_bits);

	printf("Line  Valid   Dirty Tag\n");

	for (i = 0; i < cache.line_count; i++ ) {
		clp = &(cache_lines[i]);
		printf("%5d %7s %5s ", i,
		       (clp->flags & CACHE_LINE_VALID)?"VALID":"INVALID",
		       (clp->flags & CACHE_LINE_DIRTY)?"DIRTY":"CLEAN");
		printbits((clp->tag) >> cache.tag_shift, 
			  tag_bits);
		printf("\n");
			
		       
	}
	printf("\n");
}

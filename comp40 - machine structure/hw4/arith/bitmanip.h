/* bitmanip.h
Vladimir Hugec and Jamie Weiss
Comp40 - HW4

Module for manipulating individual bits contained in a 64 bit word
Each manipulation returns a new copy of the inputted word but with
the manipulations added
*/

#ifndef BITMANIP_H
#define BITMANIP_H

#include <stdbool.h>
#include <stdint.h>

/* set the Bit at loc in word equal to val
    and returns a new uint64_t word  */
extern uint64_t setBitAt(uint64_t word, unsigned loc, unsigned val);

extern uint64_t UgetBitAt(uint64_t word, unsigned loc);
extern int64_t getBitAt(int64_t word, unsigned loc);

extern uint64_t UtoggleBitAt(uint64_t word, unsigned loc);
extern int64_t toggleBitAt(int64_t word, unsigned loc);

extern uint64_t UtoggleBitsOverInterval(uint64_t word, 
                                        unsigned start, unsigned end);
extern int64_t toggleBitsOverInterval(int64_t word, 
                                        unsigned start, unsigned end);

extern uint64_t clearBitAt(uint64_t word, unsigned loc);
extern uint64_t clearBitsOverInterval(uint64_t word, 
                                unsigned start, unsigned end);

extern uint64_t uLShift(uint64_t word, unsigned val);
extern uint64_t uRShift(uint64_t word, unsigned val);
extern int64_t  RShift(uint64_t word, unsigned val);

extern void printBit(char bit, unsigned loc, void *cl);
extern void printAllBitsIn(int64_t word);
extern void UprintAllBitsIn(uint64_t word);

extern void UmapAllBitsIn(uint64_t word, 
                        void apply(char bit, unsigned loc, void *cl), 
                        void *cl);
extern void mapAllBitsIn(int64_t word, 
                        void apply(char bit, unsigned loc, void *cl), 
                        void *cl);


#endif
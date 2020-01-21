#ifndef PACK_AND_UNPACK_H
#define PACK_AND_UNPACK_H
/* 
pack_and_unpack.h

Vladimir Hugec and Jamie Weiss
Comp40 HW4 Arith

Header file for "packing and unpacking" a 2x2 pixel block in YPbPr format 
using Discrete Cosine Transform into four variables named a, b, c and d, 
as well as averageing the four Pb and Pr values into an average Pb and Pr
and putting those values into a file
*/

#include "arith40.h"
#include "pnm.h"

#define A2 A2Methods_UArray2

extern void putWordInFile(uint64_t word);
extern uint64_t getWordFromFile(FILE *file);

extern uint64_t getPackedData(Pnm_ppm compMap, int startRowIndex, 
                                int startColIndex, uint64_t word);

extern uint64_t packA(uint64_t word,double y1,double y2, double y3, double y4);
extern uint64_t packB(uint64_t word,double y1,double y2, double y3, double y4);
extern uint64_t packC(uint64_t word,double y1,double y2, double y3, double y4);
extern uint64_t packD(uint64_t word,double y1,double y2, double y3, double y4);
extern uint64_t packPb(uint64_t word,   double Pb1, double Pb2, 
                                        double Pb3, double Pb4);
extern uint64_t packPr(uint64_t word, double Pr1, double Pr2, 
                                        double Pr3, double Pr4);

extern double getY1(double a, double b, double c, double d);
extern double getY2(double a, double b, double c, double d);
extern double getY3(double a, double b, double c, double d);
extern double getY4(double a, double b, double c, double d);

extern double unpackA(uint64_t word);
extern double unpackB(uint64_t word);
extern double unpackC(uint64_t word);
extern double unpackD(uint64_t word);
extern double unpackPb(uint64_t word);
extern double unpackPr(uint64_t word);

#endif
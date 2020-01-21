/* 
pack_and_unpack.c

Vladimir Hugec and Jamie Weiss
Comp40 HW4 Arith

Implementations of pack_and_unpack.h
*/

#include <stdlib.h>
#include <math.h>
#include "pack_and_unpack.h"
#include "ConvertRGBandCOMP.h"
#include "bitpack.h"
#include "a2plain.h"
#include "a2blocked.h"
#include "uarray2.h"
#include "manipulate.h"

uint64_t getWordFromFile(FILE *file){
        int firstByte = getc(file);
        int secondByte = getc(file);
        int thirdByte = getc(file);
        int fourthByte = getc(file);

        uint64_t word = 0;
        word = Bitpack_newu(word, 8, 0, fourthByte);
        word = Bitpack_newu(word, 8, 8, thirdByte);
        word = Bitpack_newu(word, 8, 16, secondByte);
        word = Bitpack_newu(word, 8, 24, firstByte);
        
        return word;
}

void putWordInFile(uint64_t word){
        int firstByte = Bitpack_getu(word, 8, 24);
        int secondByte = Bitpack_getu(word, 8, 16);
        int thirdByte = Bitpack_getu(word, 8, 8);
        int fourthByte = Bitpack_getu(word, 8, 0);

        putchar(firstByte);
        putchar(secondByte);
        putchar(thirdByte);
        putchar(fourthByte);
}

uint64_t getPackedData(Pnm_ppm compMap, int startRowIndex, int startColIndex, 
                                                        uint64_t word){
        A2Methods_T methods = uarray2_methods_plain;
        COMPONENT topLeft = methods->at(compMap->pixels, startColIndex, 
                                                        startRowIndex);

        COMPONENT topRight = methods->at(compMap->pixels, startColIndex + 1,
                                                         startRowIndex);

        COMPONENT botLeft = methods->at(compMap->pixels, startColIndex, 
                                                        startRowIndex + 1);

        COMPONENT botRight = methods->at(compMap->pixels, startColIndex + 1, 
                                                        startRowIndex + 1);

        uint64_t A = packA(word,topLeft->y,topRight->y,botLeft->y,botRight->y);
        uint64_t AB = packB(A,topLeft->y,topRight->y,botLeft->y,botRight->y);
        uint64_t ABC = packC(AB,topLeft->y,topRight->y,botLeft->y,botRight->y);
        uint64_t ABCD = packD(ABC,topLeft->y,topRight->y,botLeft->y,botRight->y);
        uint64_t ABCDPb = packPb(ABCD, topLeft->Pb, topRight->Pb, botLeft->Pb, 
                                                                botRight->Pb);
        uint64_t ABCDPbPr = packPr(ABCDPb, topLeft->Pr, topRight->Pr, botLeft->Pr, 
                                                                botRight->Pr);

        return ABCDPbPr;
}
double getY1(double a, double b, double c, double d){
        return a - b - c + d;
}

double getY2(double a, double b, double c, double d){
        return a - b + c - d;
}

double getY3(double a, double b, double c, double d){
        return a + b - c - d;
}

double getY4(double a, double b, double c, double d){
        return a + b + c + d;
}


uint64_t packA(uint64_t word, double y1, double y2, double y3, double y4){
        double a = (y1 + y2 + y3 + y4) / 4.0;
        uint64_t value = round(a * 63);
        uint64_t newWord = Bitpack_newu(word, 6, 26, value);
        return newWord;
}

uint64_t packB(uint64_t word, double y1, double y2, double y3, double y4){
        double b = (y4 + y3 - y2 - y1) / 4.0;
        if (b > 0.3) {
                b = 0.3;
        }
        if (b < -0.3) {
                b = -0.3;
        }
        int64_t value = round(b / 0.0097);
        uint64_t newWord = Bitpack_news(word, 6, 20, value);
        return newWord;
}

uint64_t packC(uint64_t word, double y1, double y2, double y3, double y4){
        double c = (y4 - y3 + y2 - y1) / 4.0;
        if (c > 0.3) {
                c = 0.3;
        }
        if (c < -0.3) {
                c = -0.3;
        }
        int64_t value = round(c / 0.0097);
        uint64_t newWord = Bitpack_news(word, 6, 14, value);
        return newWord;
}

uint64_t packD(uint64_t word, double y1, double y2, double y3, double y4){
        double d = (y4 - y3 - y2 + y1) / 4.0;
        if (d > 0.3) {
                d = 0.3;
        }
        if (d < -0.3) {
                d = -0.3;
        }
        int64_t value = round(d / 0.0097);
        uint64_t newWord = Bitpack_news(word, 6, 8, value);
        return newWord;
}

uint64_t packPb(uint64_t word, double Pb1, double Pb2, double Pb3, double Pb4){
        float Pb = 0;
        Pb = (Pb1 + Pb2 + Pb3 + Pb4) / (double)4;
        unsigned uValue = Arith40_index_of_chroma(Pb);
        uint64_t value = Bitpack_newu(0, 4, 0, uValue);
        uint64_t newWord = Bitpack_newu(word, 4, 4, value);
        return newWord;
}

uint64_t packPr(uint64_t word, double Pr1, double Pr2, double Pr3, double Pr4){
        float Pr = 0;
        Pr = (Pr1 + Pr2 + Pr3 + Pr4) / 4.0;
        unsigned uValue = Arith40_index_of_chroma(Pr);
        uint64_t value = uValue;
        uint64_t newWord = Bitpack_newu(word, 4, 0, value);
        return newWord;
}

double unpackA(uint64_t word){
        uint64_t value = Bitpack_getu(word, 6, 26);
        double a = value / 63.0;
        return a;
}

double unpackB(uint64_t word){
        int64_t value = Bitpack_gets(word, 6, 20);
        double b = value * 0.0097;
        return b;
}

double unpackC(uint64_t word){
        int64_t value = Bitpack_gets(word, 6, 14);
        double c = value * 0.0097;
        return c;
}

double unpackD(uint64_t word){
        int64_t value = Bitpack_gets(word, 6, 8);
        double d = value * 0.0097;
        return d;
}

double unpackPb(uint64_t word){
        uint64_t value = Bitpack_getu(word, 4, 4);
        double chroma = Arith40_chroma_of_index(value);
        return chroma;
}

double unpackPr(uint64_t word){
        uint64_t value = Bitpack_getu(word, 4, 0);
        double chroma = Arith40_chroma_of_index(value);
        return chroma;
}
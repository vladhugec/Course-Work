/*  compress40.c

    Vladimir Hugec and Jamie Weiss
    HW4 - Arith
    10-12-18

*/

#include "manipulate.h"
#include "a2methods.h"
#include "a2plain.h"
#include "arith40.h"
#include "bitpack.h"
#include "pnm.h"



/* reads PPM, writes compressed image */
void compress40(FILE *input)
{
       A2Methods_T methods = uarray2_methods_plain;
       Pnm_ppm pixmap = Pnm_ppmread(input, methods);
       checkDimensions(pixmap);
       RGBtoCOMP(pixmap);
       Pnm_ppm bitDataMap = getPackingData(pixmap, methods);
       (void) bitDataMap;


} 

/* reads compressed image, writes PPM 
void decompress40(FILE *input)
{
       A2Methods_T methods = uarray2_methods_plain;

       Pnm_ppm pixmap = Pnm_ppmread(input, methods);

}
*/
int main(int argc, char const *argv[]){
    (void) argc;
    FILE* file = fopen(argv[1], "rb");
    compress40(file);
}
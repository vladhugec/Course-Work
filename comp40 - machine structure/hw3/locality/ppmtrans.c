#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "assert.h"
#include "a2methods.h"
#include "a2plain.h"
#include "a2blocked.h"
#include "pnm.h"
#include <stdbool.h>
#include "cputiming.h"
/****************************/
/*  FUNCTION DECLARATIONS  */
/****************************/

#define A2 A2Methods_UArray2

/* store user-set transformation parameters */
typedef struct transformTypes {
        int rotate;
        int flip;
        bool transpose;
} tTypes;

void initializeTranformType(tTypes* t);
A2 updatePixmapDimensions(A2 newPixmap, Pnm_ppm pixmap, int angle);
void transpose(int col, int row, A2 a2, void *elem, void *cl);
void flip_hor(int col, int row, A2 a2, void *elem, void *cl);
void flip_vert(int col, int row, A2 a2, void *elem, void *cl);
A2 rotate(A2 newPixmap, A2Methods_mapfun *map, Pnm_ppm pixmap, tTypes *tData);
A2 manipulateImage(Pnm_ppm pixmap, A2Methods_mapfun *map, tTypes *tData,
                        char* time_file_name, char* map_string);

void print_timing(Pnm_ppm pixmap, tTypes *tData, char* time_file_name, 
                        double time, char *map_string);

/****************************/
/*      GIVEN METHODS       */
/****************************/

#define SET_METHODS(METHODS, MAP, WHAT) do {                    \
        methods = (METHODS);                                    \
        assert(methods != NULL);                                \
        map = methods->MAP;                                     \
        if (map == NULL) {                                      \
                fprintf(stderr, "%s does not support "          \
                                WHAT "mapping\n",               \
                                argv[0]);                       \
                exit(1);                                        \
        }                                                       \
} while (0)

static void
usage(const char *progname)
{
        fprintf(stderr, "Usage: %s [-rotate <angle>] "
                        "[-{row,col,block}-major] [filename]\n",
                        progname);
        exit(1);
}

/****************************/
/*          MAIN            */
/****************************/

int main(int argc, char *argv[]) 
{
        char *time_file_name = NULL;
        int   rotation       = 0;
        int   i;
        (void)time_file_name;
        char* map_string = "row-major";
        /* default to UArray2 methods */
        A2Methods_T methods = uarray2_methods_plain; 
        assert(methods);

        /* default to best map */
        A2Methods_mapfun *map = methods->map_default; 
        assert(map);
        tTypes tData;
        initializeTranformType(&tData);

        for (i = 1; i < argc; i++) {
                if (strcmp(argv[i], "-row-major") == 0) {
                        SET_METHODS(uarray2_methods_plain, map_row_major, 
                                    "row-major");
                } else if (strcmp(argv[i], "-col-major") == 0) {
                        SET_METHODS(uarray2_methods_plain, map_col_major, 
                                    "column-major");
                                    map_string = "col-major";
                } else if (strcmp(argv[i], "-block-major") == 0) {
                        SET_METHODS(uarray2_methods_blocked, map_block_major,
                                    "block-major");
                                    map_string = "block-major";
                } else if (strcmp(argv[i], "-rotate") == 0) {
                        if (!(i + 1 < argc)) {      /* no rotate value */
                                usage(argv[0]);
                        }
                        char *endptr;
                        rotation = strtol(argv[++i], &endptr, 10);
                        if (!(rotation == 0 || rotation == 90 ||
                            rotation == 180 || rotation == 270)) {
                                fprintf(stderr, 
                                        "Rotation must be 0, 90 180 or 270\n");
                                usage(argv[0]);
                        }
                        if (!(*endptr == '\0')) {    /* Not a number */
                                usage(argv[0]);
                        }
                        tData.rotate = rotation;
                } else if (strcmp(argv[i], "-time") == 0) {
                        time_file_name = argv[++i];
                } else if (strcmp(argv[i], "-flip") == 0){
                        if(strcmp(argv[++i], "horizontal") == 0){
                                tData.flip = 1;
                        }
                } else if (strcmp(argv[i], "vertical") == 0){
                        tData.flip = 0;
                        /*invalid data if flip isnt followed by v or h*/
                        assert(tData.flip == 0 || tData.flip == 1);
                }else if(strcmp(argv[i], "-transpose") == 0){
                        tData.transpose = true;
                }else if (*argv[i] == '-') {
                        fprintf(stderr, "%s: unknown option '%s'\n", argv[0],
                                argv[i]);
                } else if (argc - i > 1) {
                        fprintf(stderr, "Too many arguments\n");
                        usage(argv[0]);

                }else {
                        break;
                }
        }
        FILE* file = fopen(argv[argc-1], "rb");
        if (file == NULL || strcmp(argv[argc-1], time_file_name) == 0) {
                file = stdin;
        }
        Pnm_ppm pixmap = Pnm_ppmread(file, methods); 
        
        A2 manArray = manipulateImage(pixmap, map, &tData, time_file_name, 
                                                                map_string);
        pixmap->methods->free(&(pixmap->pixels));
        pixmap->pixels = manArray;
        Pnm_ppmwrite(stdout, pixmap);

        Pnm_ppmfree(&pixmap);
        fclose(file);
        return EXIT_SUCCESS;
}

/****************************/
/* FUNCTION IMPLEMENTATIONS */
/****************************/

/* 
manipulateImage(..)
        based on transformation perameters in tData, manipulate the image
        accordingly
 Returns: A UArray2_T of the manipulated image
*/
A2 manipulateImage(Pnm_ppm pixmap, A2Methods_mapfun *map, tTypes *tData, 
                                char* time_file_name, char* map_string) {

        CPUTime_T timer;
        A2 newPixmap;
        double time;

        timer = CPUTime_New();
        if (tData->rotate >= 0){
                /* if rotation is 180 degrees,we don't swap height and width */
                if (tData->rotate == 180){
                        newPixmap = pixmap->methods->new(pixmap->width, 
                                        pixmap->height, sizeof(Pnm_rgb)*2);
                } else if (tData->rotate == 0){
                        /* if rotate is 0, return the current image */
                        return pixmap;
                }
                else{
                        newPixmap = pixmap->methods->new(pixmap->height, 
                                        pixmap->width, sizeof(Pnm_rgb)*2);
                }
                CPUTime_Start(timer);
                newPixmap = rotate(newPixmap, map, pixmap, tData);
                time = CPUTime_Stop(timer);
        }
        /* if flip == 0, then user specified vertical flip, vice versa for 1 */
        else if (tData->flip == 0){
                newPixmap = pixmap->methods->new(pixmap->width, pixmap->height,
                                                         sizeof(Pnm_rgb)*2);
                CPUTime_Start(timer);
                (*map)(newPixmap, flip_vert, pixmap);
                time = CPUTime_Stop(timer);
        }
        else if(tData->flip == 1){
                newPixmap = pixmap->methods->new(pixmap->width, pixmap->height,
                                                         sizeof(Pnm_rgb)*2);
                CPUTime_Start(timer);
                (*map)(newPixmap, flip_hor, pixmap);
                time = CPUTime_Stop(timer);
        }
        else if(tData->transpose == true) {
                newPixmap = pixmap->methods->new(pixmap->height, pixmap->width,
                                                         sizeof(Pnm_rgb)*2);
                CPUTime_Start(timer);
                (*map)(newPixmap, transpose, pixmap);
                time = CPUTime_Stop(timer);
        }

        print_timing(pixmap, tData, time_file_name, time, map_string);
        CPUTime_Free(&timer);
        return newPixmap;
}
/* 
rotate(..)
        rotate the image according to the number of degrees contained in
        tData->rotate
 Returns: A UArray2_T of the manipulated image
*/
A2 rotate(A2 newPixmap, A2Methods_mapfun *map, Pnm_ppm pixmap, tTypes *tData)
{        
        A2 nextPixmap;
        if (tData->rotate == 90){
                (*map)(newPixmap, transpose, pixmap);
                nextPixmap = updatePixmapDimensions(newPixmap, pixmap, 90);
                (*map)(nextPixmap, flip_hor, pixmap);
        } else if (tData->rotate == 180){
                (*map)(newPixmap, flip_vert, pixmap);
                nextPixmap = updatePixmapDimensions(newPixmap, pixmap, 180);
                (*map)(nextPixmap, flip_hor, pixmap);
        } else if (tData->rotate == 270){
                (*map)(newPixmap, transpose, pixmap);
                nextPixmap = updatePixmapDimensions(newPixmap, pixmap, 270);
                (*map)(nextPixmap, flip_vert, pixmap);
        } else if (tData->rotate == 0) {
                return newPixmap;
        }

        return nextPixmap;
}
/* 
transpose(..)
        for a cell at coordinates (x,y)
        flip x and y so same cell is found at (y, x)
*/
void transpose(int col, int row, A2 a2, void *elem, void *cl)
{
        (void)elem;
        Pnm_ppm pixmap = cl;
        Pnm_rgb origLoc = pixmap->methods->at(pixmap->pixels, row, col);      
        Pnm_rgb transLoc = pixmap->methods->at(a2, col, row);
        *transLoc = *origLoc;
}
/* 
flip_hor(..)
        mirror image about y-axis
*/
void flip_hor(int col, int row, A2 a2, void *elem, void *cl)
{
        (void)elem;
        Pnm_ppm pixmap = cl;
        Pnm_rgb origLoc = pixmap->methods->at(pixmap->pixels, 
                                                pixmap->width-col-1, row);
        Pnm_rgb flipLoc = pixmap->methods->at(a2, col, row);
        *flipLoc = *origLoc;
}
/* 
flip_vert(..)
        mirror image about x-axis
*/
void flip_vert(int col, int row, A2 a2, void *elem, void *cl)
{
        (void)elem;
        Pnm_ppm pixmap = cl;
        Pnm_rgb origLoc = pixmap->methods->at(pixmap->pixels, 
                                                col, pixmap->height-row-1);
        Pnm_rgb flipLoc = pixmap->methods->at(a2, col, row);
        *flipLoc = *origLoc;
}
/* 
updatePixmapDimensions(..)
        saves current manipulation of image to pixelmap
        and creates new UArray2_T of manipulated dimentions

        NOTE: this needs to be done since the width and height for a transposed
        image will be swapped, images are transposed on 90 and 270 degree
        rotations             _
        i.e. |_____| rot90 ->| |
                             | |

Returns: A UArray2_T of updated dimentions
*/
A2 updatePixmapDimensions(A2 newPixmap, Pnm_ppm pixmap, int angle) {
        pixmap->methods->free(&(pixmap->pixels));
        pixmap->pixels = newPixmap;
        if (angle == 90 || angle == 270) {
                pixmap->width = pixmap->methods->width(newPixmap);
                pixmap->height = pixmap->methods->height(newPixmap);
        }
        
        A2 nextPixmap = pixmap->methods->new(pixmap->width, pixmap->height, 
                                                        sizeof(Pnm_rgb)*2);
        return nextPixmap;
}
/* 
print_timing(..)
        print the ammount of time passed since start of manipulate call
*/
void print_timing(Pnm_ppm pixmap, tTypes *tData, char* time_file_name, 
                                double time, char *map_string)
{
        if (time_file_name != NULL){
                FILE* timing_file = fopen(time_file_name, "a");
                fprintf(timing_file, "%s\n", "-------------------------------");
                if (tData->rotate > 0){
                        fprintf(timing_file, "Rotation of %u\n", tData->rotate);
                }else if (tData->flip == 0){
                        fprintf(timing_file, "%s\n", "Vertical Flip");
                }else if (tData->flip == 1){
                        fprintf(timing_file, "%s\n", "Horizontal Flip");
                }else if (tData->transpose == true){
                        fprintf(timing_file, "%s\n", "Transpose");
                }
                fprintf(timing_file, "Map Type: %s\n", map_string);
                fprintf(timing_file, "Total time: %f\n", time);
                fprintf(timing_file, "Time per pixel: %f\n", 
                                        time / pixmap->height / pixmap->width);
                fprintf(timing_file, "Image height: %u\n", pixmap->height);
                fprintf(timing_file, "Image width: %u\n", pixmap->width);
                fprintf(timing_file, "%s\n", "-------------------------------");
        }
}
/* 
initializeTranformType(..)
        initialize transformation parameters data struct
*/
void initializeTranformType(tTypes* t) {
        t->rotate = -1;
        t->flip = -1;
        t->transpose = NULL;
}

#undef A2
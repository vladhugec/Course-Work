/*   unblackedges.c  
BY: Vladimir Hugec and Jamie Wiess
HW2 Comp-40     9-25-18 
*/
#include "unblackedges.h"

int main(int argc, char* argv[]) {
        FILE *fp = openFile(argc, argv);

        Seq_T edges = Seq_new(0);
        Bit2_T bitMap = pbmToBit2(fp);
        buildEdgeSet(bitMap, edges);
       
        processEdges(bitMap, edges);
        pbmWrite(stdout, bitMap);
        
        Seq_free(&edges);
        Bit2_free(&bitMap);
}

FILE *openFile(int argc, char** argv) {
        FILE *fp = NULL;
        if (argc != 2) {
                fprintf(stderr,"ERROR: invalid number of arguments\n");
                exit(EXIT_FAILURE);
        }

        if (argc == 1) {
                /* check if stdin buffer is empty */
                if (isatty(STDIN_FILENO)) {
                        fprintf(stderr, "ERROR: no arguments supplied\n");
                        exit(EXIT_FAILURE);
                }
                fp = stdin;
        }
        else {
                fp = fopen(argv[1], "rb");
                assert(fp != NULL);
        }
        return fp; 
}

void pbmWrite(FILE *outputFP, Bit2_T bitMap) {
        (void)outputFP;
        fprintf(outputFP, "%s\n", FILE_TYPE);
        fprintf(outputFP, "%s\n", OUTPUTtxt);
        fprintf(outputFP, "%d %d", bitMap->width, bitMap->height);
        fprintf(outputFP, "\n");

        int count = 0;
        Bit2_map_col_major(bitMap, printStdOut, &count);
}

void printStdOut(int col, int row, Bit2_T bitMap, int bit, void *cl) {
        (void)col; (void)row; (void)bitMap;
        if (*(int*)cl == MAX_COL_WIDTH)
                fprintf(stdout, "\n");

        fprintf(stdout, "%d ", bit);
        *(int*)cl = *(int*)cl + 1;
}

void insertPixel(int col, int row, Bit2_T bitMap, int bit, void *cl) {
        (void)bit;
        Bit2_put(bitMap, col, row, Pnmrdr_get(*(Pnmrdr_T*)cl));
}

Bit2_T pbmToBit2(FILE *inputFP) {
        Pnmrdr_T reader = Pnmrdr_new(inputFP);
        Pnmrdr_mapdata mData = Pnmrdr_data(reader);

        /* ERROR: check that the file-type is the correct PNM filetype */
        assert(mData.type == 1);

        Bit2_T bitMap = Bit2_new(mData.width, mData.height);
        Bit2_map_col_major(bitMap, insertPixel, &reader);

        return bitMap;
}

void buildEdgeSet(Bit2_T bitmap, Seq_T edges) {
        Bit2_map_col_major(bitmap, makeEdgeSet, &edges);
}

void makeEdgeSet(int col, int row, Bit2_T bitMap, int bit, void *cl) {
        if (isEdgePixel(col, row, bitMap->width, bitMap->height) == 1)
                findBlackEdgePixels(col, row, bit, *(Seq_T*)cl);
}

int isEdgePixel(int col, int row, int bMapWidth, int bMapHeight) {
        if ((row == 0) || (col == 0)) {
                return 1;
        }
        if ((col == bMapWidth-1) || (row == bMapHeight-1)) {
                return 1;
        }
        
        return 0;
}

void findBlackEdgePixels(int col, int row, int bit, Seq_T bPixelSet) {
        if (bit == BLACK) {
                bitLoc* bitCords = setPixel(col, row);
                Seq_addhi(bPixelSet, bitCords); 
        }
}

void processEdges(Bit2_T bitMap, Seq_T edges) {
        while (Seq_length(edges) > 0){
                bitLoc* pixel = Seq_remlo(edges);

                if (Bit2_get(bitMap, pixel->col, pixel->row) == 0){
                        free(pixel);
                        continue;
                }

                addEdgeNeighbors(bitMap, edges, pixel);
                Bit2_put(bitMap, pixel->col, pixel->row, 0);
                free(pixel);
        }
}

void addEdgeNeighbors(Bit2_T bitMap, Seq_T edges, bitLoc* pixel) {
        checkNeighbors(pixel->col, pixel->row-1, bitMap, edges, DOWN);
        checkNeighbors(pixel->col, pixel->row+1, bitMap, edges, UP);
        checkNeighbors(pixel->col-1, pixel->row, bitMap, edges, LEFT);
        checkNeighbors(pixel->col+1, pixel->row, bitMap, edges, RIGHT);    
}

void checkNeighbors(int col, int row, Bit2_T bitMap, Seq_T edges, int direc){
        if (validatePos(row, col, direc, bitMap) == 1) { 
                if (Bit2_get(bitMap, col, row) == 1) {
                        bitLoc* nextPixel = setPixel(col, row);
                        Seq_addlo(edges, nextPixel); 
                }               
        }
}

int validatePos(int row, int col, int direc, Bit2_T bitMap) {
        if (direc == DOWN && row > 0) {
                return 1;
        }
        if (direc == UP && row < Bit2_height(bitMap)-1) {
                return 1;
        }
        if (direc == LEFT && col > 0) {
                return 1;
        }
        if (direc == RIGHT && col < Bit2_width(bitMap)-1) {
                return 1;
        }
        
        return 0;
}

bitLoc* setPixel(int col, int row) {
        bitLoc* pixel = malloc(sizeof(bitLoc));
        pixel->col = col; 
        pixel->row = row;
        return pixel;
}
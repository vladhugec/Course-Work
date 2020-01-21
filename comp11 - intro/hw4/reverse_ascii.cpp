/* reverse_ascii.cpp
 * 
 * Flipping pictures horizontally
 * 
 * Read in an ASCII art picture on standard input, and print a
 * reversed picture on standard output.
 *
 * The image format is has the number of rows followed by the number
 * of columns on the first line, and then an image map of rows lines,
 * each with columns characters.  For example, here is a 3 x 4 stick
 * figure image (the '3' is in column 1):
 *
 * 3 4
 *    o
 *   /|\
 *   / \
 * 
 * Modified by: Vladimir Hugec
 * Date: 3/8/2018
 */

#include <iostream>
#include <fstream>
#include <sstream>
#include <math.h>
// algorithm includes min and max
#include <algorithm>

using namespace std;

const int MAX_IMG_HGT = 25;  // was 24, but the wolf picture has a by-line
const int MAX_IMG_WID = 80;

/*
 * This is a structure for holding a picture.
 * You don't have to understand this right away.
 * If we haven't discussed structs in class yet, ignore it.
 */
struct ASCII_picture {
        int  rows, cols;
        char picture[MAX_IMG_HGT][MAX_IMG_WID];
};

void init_image(ASCII_picture *img);
void read_image_from(istream &input, ASCII_picture *img);
void print_image(ASCII_picture *img);

void reverse_array(char array[], int len);
void LR_flip(char image_array[MAX_IMG_HGT][MAX_IMG_WID], 
             int nrows, int ncols);


int main()
{
        ASCII_picture img;
        init_image(&img);
        read_image_from(cin, &img);
        LR_flip(img.picture, img.rows, img.cols);
        print_image(&img);

        return 0;
}

/*
 * Reverse the len elements in array.
 * Changes the contents of array.
 * 
 * --
 * 
 * NOTE;
 *    - len/2 is midpoint of coloumn
 *    - and each value i has corresponding value (len-1)-i
 *      - flip each value with corresponding one up to midpoint
 */
void reverse_array(char array[], int len)
{
       for (int i=0; i<len/2; i++)
       {
                char the_reverser = array[(len-1)-i];
                array[(len-1)-i] = array[i];
                array[i] = the_reverser;
       }
}


/*
 * Initalize the 2D array pic to contain all spaces.
 */
void init_image_map(char pic[MAX_IMG_HGT][MAX_IMG_WID])
{
        for (int r=0; r<MAX_IMG_HGT; r++)
                {for (int c=0; c<MAX_IMG_WID; c++)
                        {pic[r][c] = '\0';}}
}

/*
 * Flip the nrows x ncols rectangle in the upper left corner of
 * image_array left to right, like a mirror image.
 * The other elements of the array are left unchanged.
 */
void LR_flip(char image_array[MAX_IMG_HGT][MAX_IMG_WID], int nrows, int ncols)
{
        for (int i = 0; i<nrows; i++)
                {reverse_array(image_array[i], ncols);}
}

/*************************************************************************/
/*                        Here there be dragons                          */
/*                                                                       */
/*  There is no need to read below this line.  If you are curious, go    */
/*  ahead, but there is nothing here you need to understand for the      */
/*  assignment.                                                          */
/*************************************************************************/

/*
 * Initialize an image to 0 x 0 size and fill with blanks.
 */
void init_image(ASCII_picture *img)
{
        img->rows = 0;
        img->cols = 0;
        init_image_map(img->picture);
}

/*
 * Print image in the same format we read it in.
 * rows cols on first line
 * ASCII image map following
 */
void print_image(ASCII_picture *img)
{
        cout << img->rows << ' ' << img->cols << endl;
        
        for (int r = 0; r < img->rows; ++r) {
                for (int c = 0; c < img->cols; ++c)
                        cout << img->picture[r][c];
                cout << endl;
        }
}

/*
 * Print given error message and then abort program.
 */
void abort_because(string message)
{
        cerr << "Aborting:  " << message << endl;
        exit(1);
}

/*
 * Read the ASCII picture rows and columns from the first
 * line of input.
 *
 * Aborts if line cannot be read or parsed into two integers.
 */
void get_rows_cols_from(istream &input, ASCII_picture *img)
{
        string line;
        int    rows, cols;

        if (getline(input, line).fail())
                abort_because("cannot read image dimensions");
        
        // Don't read to img->rows, img->cols in case one fails
        istringstream iss(line);
        if ((iss >> rows >> cols).fail())
                abort_because("cannot read image dimensions:  " + line);
        
        if (rows > MAX_IMG_HGT or cols > MAX_IMG_WID) {
                cerr << "truncating image from " << rows << " x " << cols;
                rows = min(rows, MAX_IMG_HGT);
                cols = min(cols, MAX_IMG_WID);
                cerr << "to " << rows << " x " << cols << endl;
        }
        img->rows = rows;
        img->cols = cols;
}

/*
 * Read the the ASCII image data from the input.
 * Does not check for read failure.
 * Assumes img has valid rows and cols already.
 *
 * TODO:  Detect and report read failure.
 */
void read_image_map_from(istream &input, ASCII_picture *img)
{
        string line;
        int    rows = img->rows,
               cols = img->cols;
        int    r, c;

        // If missing lines, just return with whatever we got
        for (r = 0; (r < rows) and not getline(input, line).fail(); ++r)
                for (c = 0; (c < cols) and ((unsigned) c < line.length()); ++c)
                        img->picture[r][c] = line[c];
}

/*
 * Read an image from the given input stream.
 */
void read_image_from(istream &input, ASCII_picture *img)
{
        get_rows_cols_from(input, img);
        // cerr << "reading image:  " << img->rows << " x " << img->cols << endl;
        read_image_map_from(cin, img);
  }

/*
 * Read an image from the given file.
 * This program isn't using this function, but it's here if you're 
 * interested.
 */
void read_image_from_file(ASCII_picture *img, string filename)
{
        ifstream input(filename.c_str());

        if (not input.is_open())
                abort_because("Cannot open file:  " + filename);

        read_image_from(input, img);
}

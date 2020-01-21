/*
 * rockdims.cpp -- analyze height and width of a digital image
 *
 *  input has "." background  and "x" for rock
 * 
 * AUTHOR: Vladimir Hugec
 * DATE: Mar 1, 2017
 */

#include <iostream>
using namespace std;

const char ROCK  = 'x';
const int WIDTH  = 200;
const int HEIGHT = 50;

char digitized[HEIGHT][WIDTH];

// declarations go here
// main should be very short, just an outline


// Purpose: Fit data from text file into array
// 
// Reads character by character, inputs into array
void digitize()
{ 
  for(int h=0; h<HEIGHT; h++)
    {
      for(int w=0; w<WIDTH;w++)
			{
	  		char c;
	  		cin >> c;
	 		 digitized[h][w]=c;
			}
    }
}

// Purpose: Return # of rows which have a "ROCK" present
//
// If there is a rock in the row, add 1 to tot_height
int tot_height()
{
	int tot_height = 0;
	bool rock = false;
	for(int h=0; h<HEIGHT; h++)
  { 
		if (rock == true)
		{
			tot_height++;
		}
    rock = false;

    for(int w=0; w<WIDTH;w++)
		{ 
			if (digitized[h][w] == ROCK)
			{
				rock = true;
			}
		}
	}
	return tot_height;
}

// Purpose: Return the largest of ammount of "ROCK"s present in
//          in total file
//
// Add up all rocks in a row, if the number of rocks in current row are smaller
// than the ones in the previous row, ignore the number of rocks in that row
// and move to next row. If number of rocks in new current row is larger
// than the last one, then the new number of rocks becomes the max number of
// rocks per row
int maxwidth()
{
  int max_width = 0;
  int prev_max_width = 0;
	int flip = 0;
  for(int h=0; h<HEIGHT; h++)
  { 
    int count = 0;
    for(int w=0; w<WIDTH;w++)
		{ 
	  	if (digitized[h][w]==ROCK)
	    {
	      count++;
	    }
	  	if(w<WIDTH)
	    {
	      prev_max_width = max_width;
	      max_width = count;
	      if(max_width<prev_max_width)
				{
		  		flip = max_width;
		  		max_width = prev_max_width;
		  		prev_max_width = flip;
				}
	    }
		}  
 }
  return max_width;
}

int main() 
{
  digitize();
	cout << "Height: " << tot_height() << endl;
  cout << "Width: " << maxwidth() << endl;

  return 0;
}
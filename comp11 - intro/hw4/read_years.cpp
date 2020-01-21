/*
 * read_years.cpp - demo of sentinel
 * 
 * Modified by: Vladimir Hugec
 * Date : March 3, 2018
 * 
 * Purpose : Given a list of years, make an array with those years
 *           Prints out the array along with the average of all those
 *           inputs.
 */

#include <iostream>
using namespace std;

const int SPACE    =  1000;
const int SENTINEL = -1;

/////////////////////////////////////////////////////////////////////////
///////////////////////////   Interfaces  ///////////////////////////////
/////////////////////////////////////////////////////////////////////////

//
// Print a sequence of numbers
// terminated by a sentinel
//
void   print_seq(int nums[]);

//
// Return the average (mean) value
// of the integers in the sequence
// of numbers terminated by a sentinel
//
double average  (int nums[],int pos);


/////////////////////////////////////////////////////////////////////////
/////////////////////////////   Client  /////////////////////////////////
/////////////////////////////////////////////////////////////////////////

int main()
{
    int years[SPACE];
    int pos = 0;
    int a_year;
    bool mem = true;

    do {
        if(pos<SPACE)
        {
            cin >> a_year;
            years[pos++] = a_year;
        }
        else {mem = false;}
        } while (a_year != SENTINEL && mem == true);

    if (mem)
    {
        print_seq(years);
        cout << "avg = " << average(years, pos) << endl;

        return 0;
    }
    else 
    {
        cout << "too much input " << endl;
        return 1;
    }
}

/////////////////////////////////////////////////////////////////////////
///////////////////////////   Abstraction ///////////////////////////////
///////////////////////////     Barrier   ///////////////////////////////
/////////////////////////////////////////////////////////////////////////

//////////////////////////  Implementations /////////////////////////////

// print a sequence of numbers
// terminated by a sentinel
void print_seq(int nums[SPACE])
{
        int pos = 0;
        while (nums[pos] != SENTINEL) {
                cout << pos << " " << 
	        nums[pos] << endl;
                pos++;
        }
}

// returns the average (mean) value
// of the integers in the sequence
double average(int nums[SPACE], int pos)
{
    double cum_sum = 0;
    // Add each input to total
    for(int i=0; i<pos-1; i++)
    {
        int value = nums[i];
        cum_sum += value;
    }
    pos = double(pos);
    // Divide total by SPACE to get average

    return cum_sum / (pos-1);
}
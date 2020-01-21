// over_avg.cpp
//
// Author : Vladimir Hugec
// Date : March 3, 2018
//
// Purpose: Reads in years from cin until it reads a sentinel of 0 (zero)
//          then prints how many of those years are over the average value 
//          of all the input.

#include <iostream>
using namespace std;

const int SPACE = 30000;
const int SENTINEL = 0;
bool mem;

/////////////////////////////////////////////////////////////////////////
///////////////////////////   Interfaces  ///////////////////////////////
/////////////////////////////////////////////////////////////////////////


// Return the average (mean) value
// of the integers in the sequence
// of numbers terminated by a sentinel
//
double average (int nums[], int pos);
int data(int nums[]);
int overavg(int nums[], double avg, int pos);


/////////////////////////////////////////////////////////////////////////
/////////////////////////////   Client  /////////////////////////////////
/////////////////////////////////////////////////////////////////////////

int main()
{
    int nums[SPACE]; 
    int tot_data = data(nums);
    double avg = average(nums, tot_data);
    int num_over_avg = overavg(nums, avg, tot_data);

    if(!mem) 
        {cout << "too much data" << endl; return 1;}
    else
        {cout << num_over_avg << endl;}

    return 0;
}

/////////////////////////////////////////////////////////////////////////
///////////////////////////   Abstraction ///////////////////////////////
///////////////////////////     Barrier   ///////////////////////////////
/////////////////////////////////////////////////////////////////////////

//////////////////////////  Implementations /////////////////////////////

// Arguments: nums[] [Array of inputed numbers]
//            avg  [floating point average of all inputed numbers]
//            pos [total number of inputed numbers]
// Returns: The number of inputed numbers that are over the average
//
int overavg(int nums[], double avg, int pos)
{
    int num_over_avg = 0;

    for (int i=0; i<pos-1; i++)
        {if (nums[i] > avg) 
            {num_over_avg++;} }

    return num_over_avg;
}
// PURPOSE : Places inputted numbers into array nums[SPACE]
//           and return the total number of inputed numbers
// Arguments: nums[] [Array of inputed numbers]
//            
// Returns: Total number of inputed numbers
//
int data (int nums[])
{
    int pos = 0;
    int a_year;

    do {
        if(pos<=SPACE)
        {
            mem = true;
            cin >> a_year;
            nums[pos++] = a_year;
        }
        else {mem = false;}
        } while (a_year != SENTINEL && mem == true);
    
    return pos;
}

// returns the average (mean) value
// of the integers in the sequence
double average(int nums[], int pos)
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
// avg_age.cpp
//
// Author : Vladimir Hugec
// Date : March 3, 2018
//
// Purpose: Index list of names along with birthdays
//          Calculate average age of a name based on user input
//          retrun the average age of the name

#include <iostream>
#include <iomanip>
using namespace std;

const int SPACE = 50000;
const int SENTINEL = -1;

/////////////////////////////////////////////////////////////////////////
////////////////////////   Global Declarations  /////////////////////////
/////////////////////////////////////////////////////////////////////////

int years[SPACE];
string names[SPACE];
string search_for = "NONE";
bool mem = true;
int doppleganger = 0;

/////////////////////////////////////////////////////////////////////////
///////////////////////////   Interfaces  ///////////////////////////////
/////////////////////////////////////////////////////////////////////////

// Creates an array of names and an array of years using inputed data
// 
// Returns: the number of data points indexed
int index_array();

// Finds the average
double average(int nums);

/////////////////////////////////////////////////////////////////////////
/////////////////////////////   Client  /////////////////////////////////
/////////////////////////////////////////////////////////////////////////

int main()
{
    int num_data = index_array();
    double avg = average(num_data);

        // if the name was found, round to 3 digits and print
    if (doppleganger > 0 && mem == true)
        {cout << "average age = " << setprecision(3) 
              << avg << endl;}
        // else if the program ran out of room in the array, print error
    else if (mem == false)
        {cerr << "too much data" << endl;
        return 1;}
        // else if the name wasnt found, print...
    else if (doppleganger == 0)
        {cout << "name not found" << endl;}

    return 0;
}


//////////////////////////  Implementations /////////////////////////////

// Variable: int num_data [number of data points inputed]
double average(int num_data)
{
    int age_sum = 0;

    for (int i=0; i<num_data; i++)
    {
      //if i in the names array matches the seach name, add 1 to doppleganger
        if (names[i] == search_for)
        {
            doppleganger++;
            age_sum += years[i];
        }
    }
       // turns birthdate into age then returns age
    return 2018 - (age_sum / doppleganger);
}


int index_array()
{
    int pos = 0;//[position in the names & years arrays & tot # of data points
    int a_year;
    string a_name;

    while (mem && search_for == "NONE")
    {
        // if data exceeds capacity, set mem = false
        if (pos+1>SPACE)
            {mem = false;}
        else 
            {mem = true;}

        if (mem)
        {
            cin >> a_year >> a_name;
    
        // if the data is the sentinel, the next line is the name searched for 
            if (a_year == SENTINEL)
            {
                cout << "Enter a name: ";
                cin >> a_name;
                cout << a_name << endl;
                search_for = a_name;
            }
            
    // else store the relative data in relative position based on pos variable
            else
            {   
                years[pos] = a_year;
                names[pos] = a_name;
                pos++;
            }
        }
    }

    return pos; 
}
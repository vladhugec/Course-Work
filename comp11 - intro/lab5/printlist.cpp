/*
 * printlist.cpp  -- shows how to read a list of numbers into an array
 *
 *      1. read in data
 *      2. then prints out the list with line numbers using another loop
 *
 *   TODO:
 *    [a] change loop to print out only the birthdays in October
 *    [b] after printing out the October bdays, print a count of how many
 *    [c] change to print the count for each month, not the actual bdays
 *    [d] use a function to do [c]
 *    [e] EXTRA: print which month has the MOST birthdays
 */
#include <iostream>

using namespace std;

const int CAPACITY = 1000;         // change as needed

int bdaycounter(int bdays[], int pos, int used);
int main() 
{
        int bdays[CAPACITY];    // birthday data
        int used;               // number of spaces used so far
        int pos;                // position in array

        // read in CAPACITY values
        pos = 0;
        while (pos < CAPACITY) {
                cin >> bdays[pos];
                pos++;
        }
        used = pos;             // position is how far we got
	bdaycounter(bdays, pos, used);
	return 0;
}

int bdaycounter(int bdays[], int pos, int used)
{
  int b_days_in_month[12];
  for (int i = 0; i<12;i++){
    b_days_in_month[i] = 0;
  }
        for (pos = 0; pos < used ; pos++) {
	  if (bdays[pos]>0 && bdays[pos]<200) {
	    b_days_in_month[0]++;
	  }
	  if (bdays[pos]>200 && bdays[pos]<300) {
	        b_days_in_month[1] = b_days_in_month[1] + 1;
	  }
	  if (bdays[pos]>300 && bdays[pos]<400) {
	    b_days_in_month[2] = b_days_in_month[2] + 1;
	  }
	  if (bdays[pos]>400 && bdays[pos]<500) {
	    b_days_in_month[3] = b_days_in_month[3] + 1;
	  }
	  if (bdays[pos]>500 && bdays[pos]<600) {
	    b_days_in_month[4] = b_days_in_month[4] + 1;
	  }
	  if (bdays[pos]>600 && bdays[pos]<700) {
	    b_days_in_month[5] = b_days_in_month[5] + 1;
	  }
	  if (bdays[pos]>700 && bdays[pos]<800) {
	    b_days_in_month[6] = b_days_in_month[6] + 1;
	  }
	  if (bdays[pos]>800 && bdays[pos]<900) {
	    b_days_in_month[7] = b_days_in_month[7] + 1;
	  }
	  if (bdays[pos]>900 && bdays[pos]<1000) {
	    b_days_in_month[8] = b_days_in_month[8] + 1;
	  }
	  if (bdays[pos]>1000 && bdays[pos]<1100) {
	    b_days_in_month[9] = b_days_in_month[9] + 1;
	  }
	  if (bdays[pos]>1100 && bdays[pos]<1200) {
	    b_days_in_month[10] = b_days_in_month[10] + 1;
	  }
	  if (bdays[pos]>1200 && bdays[pos]<1300) {
	    b_days_in_month[11] = b_days_in_month[11] + 1;
	  }
        }
	int month;
	month = 0;
	while (month <= 11){
	  cout << month+1 << " " << b_days_in_month[month] << endl;
	  month++;
	}

        return 0;
}

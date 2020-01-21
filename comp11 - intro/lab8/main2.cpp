#include <iostream>
#include "wnlist.h"
using namespace std;

//
// main2.cpp
//
//	A "WNlist object" stores a list of words and numbers.  The functions
//	that belong to that object are:
//		add(string,num)	-- add pair to the list
//		print()		-- prints the list
//		count()		-- how many items in list
//		count_of_word(str) -- how many entries with given word
//
//
//       TODO:	
//		[2] write loop to read in names and nums until end of data
//			note: no sentinel needed. stops at end of file.
//			then print the count on one line
//			then print the entire list
//		

int main()
{
	WNlist	lab;	// people in my lab
	string	name;
	int	age;

	// this notation will read AND return false at end of data

	while( cin >> name >> age )
	{
		lab.add(name, age);
	}

        lab.print();
        cout << lab.count() << endl;

	return 0;
}

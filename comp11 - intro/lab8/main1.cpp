#include <iostream>
#include "wnlist.h"
using namespace std;

//
// main1.cpp
// 	A program that uses the reusable module for  word-num list: Wnlist
// 	This program shows how to create one of these lists
// 	and call some basic functions that belong to the variable.
//
//	A "WNlist object" stores a list of words and numbers.  The functions
//	that belong to that object are:
//		add(string,num)	-- add pair to the list
//		print()		-- prints the list
//		count()		-- how many items in list
//		count_of_word(str) -- how many entries with given word
//
//       TODO:	[1] add more names
//		[2] write loop to read in names then print them
//		[3] read in names and school (LA|SoE) then print two lists
//		    with counts
//			output should be: School: LA
//					  Count: ##
//					  Students:
//				          name1 year1
//				          name2 year2
//				          name3 year3
//				          ...
//					  School: SoE
//
//				
//		[4] modify [c] so there are no duplicate names in a list
//		

int main()
{
	WNlist	lab;	// people in my lab
	string		name;
	int		n;

	lab.add("me", 19);
        lab.add("Ashley", 21);
	lab.add("Peter", 22);
	lab.add("Molly", 20);
	cout << "There are " << lab.count() << endl << endl;
	cout << "Their names/ages are:" << endl;
	lab.print();
	cout << "Look up what name? ";
	cin  >> name;
	n = lab.count_of_word(name);
	cout << "There are " << n << " " << name << "s in the list" << endl;
	return 0;
}

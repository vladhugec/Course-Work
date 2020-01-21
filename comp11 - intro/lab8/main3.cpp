#include <iostream>
#include "wnlist.h"
using namespace std;

//
// main3.cpp
// 	A program that uses the reusable module: wordlist.
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
//       TODO:	[3] read in names and school (LA|SoE) then print two lists
//		    with counts
//			output should be: School: LA
//					  Count: ##
//					  Students:
//				          name1 year1
//				          name2 year2
//				          name3 year3
//				          ...
//					  School: SoE

int main()
{
  WNlist students_LA;
  WNlist students_SoE;
  string name;
  int year;
  string school;

  while (cin >> name >> year >> school) {
    if (school == "LA") {
      students_LA.add(name,year);
    }
    else if (school == "SoE") {
      students_SoE.add(name,year);
    }
  }

  cout << "School: LA" << endl;
  cout << "Count: " << students_LA.count() << endl;
  cout << "Students:" << endl;
  students_LA.print();
 
  cout << "School: SoE" << endl;
  cout << "Count: " << students_SoE.count() << endl;
  cout << "Students:" << endl;
  students_SoE.print(); 

 
	return 0;
}

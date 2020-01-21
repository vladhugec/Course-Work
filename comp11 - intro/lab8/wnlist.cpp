#include <iostream>
#include "wnlist.h"
using namespace std;

//
// class functions for wnlist
//
// A wordlist is a list of word,number pairs
//

//
// This is the initializer
//
WNlist::WNlist()
{
	space = MAX_PAIRS;
	used  = 0;
}
//
// add a string with an associated value,
//  rets: true if ok, false if not possible
//
bool WNlist::add(string s, int v)
{
	if ( used >= space )
		return false;
	list[used].word = s;
	list[used].num  = v;
	used++;
	return true;
}
//
// returns number of items in the list
//
int WNlist::count()
{
	return used;
}
//
// prints the list to cout
//
void WNlist::print()
{
	for (int i=0; i<used; i++ )
		cout << list[i].word << " " << list[i].num << endl;
}
//
// tells how many instances of a word are there
//
int WNlist::count_of_word(string s)
{
	int	num = 0;
	for (int i=0; i<used; i++ )
		if ( list[i].word == s )
			num++;
	return num;
}
//
// tells how many instances of a number are there
//
int WNlist::count_of_num(int n)
{
	int	num = 0;
	for (int i=0; i<used; i++ )
		if ( list[i].num == n )
			num++;
	return num;
}
//
// get_value -- return the value for the
//              given string
//
// note: returns 0 if no matches
//
int WNlist::get_value(string s)
{
	for (int i=0; i<used; i++ )
		if ( list[i].word == s )
			return list[i].num;
	return 0;
}

//
// who_has_num(int n) -- list words for items with given num
//  args: a number
//  does: prints the words for all items with that number
//
void WNlist::who_has_num(int n)
{
  for (int i=0;i<used;i++) {
    if (list[i].num == n) {
      cout << list[i].word << endl;
    }
}
//
// returns the max val of the number part of the pairs
//
int WNlist::get_max()
{
  int max_value = 0;
  for (int i=0;i<used;i++) {
    if (list[i].num>max_value) {
      max_value = list[i].num;
  
      return max_value;
}

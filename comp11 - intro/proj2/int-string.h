#include <sstream>

#ifndef INTSTRING_H
#define INTSTRING_H

using namespace std;

/* Convert a string to an int. Returns the int form of the given string.
   Throw logic error if the string is not numerical. */
int string2int(string s);


/* Converts an int to string.
   Returns the string form of the given int */
string int2string(int num);

#endif
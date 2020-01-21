#include <sstream>
#include "int-string.h"
using namespace std;

/* Convert a string to an int. Returns the int form of the given string.
   Throw logic error if the string is not numerical. */
int string2int(string s) {
    stringstream ss(s);
    int result;
    ss >> result;
    if (ss.fail())
        throw logic_error("ERROR: " + s + " is not a numerical value");
    return result;
}

/* Converts an int to string.
   Returns the string form of the given int */
string int2string(int num) {
    string result;
    ostringstream oss;
    oss << num;
    result = oss.str();
    return result;
}
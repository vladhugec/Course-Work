////////////////////////////////////////////////////////////////////////////
//
//    Tufts University, Comp 160 wordInterpret coding assignment  
//
//    interpret.cpp
//    wordInterpret
//
//    includes function students need to implement
//
////////////////////////////////////////////////////////////////////////////

#include <iostream>
#include <cstdlib>
#include <unordered_map>
#include <string>
#include "interpret.h"

using namespace std;

unsigned long wordInterpret(string inputNums) {
    return addPaths(inputNums);
}

unsigned long addPaths(string subStrNums) {
    //check if input is long enough to prevent out of bound
    unsigned long total = 0;
    if (subStrNums.length() > 1) {
        string fst = subStrNums.substr(0,1);
        string snd = subStrNums.substr(1,1);

        string rem1 = subStrNums.substr(1,string::npos);
        string rem2 = subStrNums.substr(2,string::npos);

        cout << "KEY: "<< subStrNums << endl;
        cout << "     REM1 "<< rem1 << endl;
        cout << "     REM2 "<< rem2 << endl;
        
        if (stoi(fst+snd) < 27) {
            total = (addPaths(rem1) + addPaths(rem2));
            cout << "TOTAL == " << total << endl;
            (*table)[subStrNums] = total;
            return total;
        }
        else {
            total = addPaths(rem1);
            cout << "TOTAL == " << total << endl;
            (*table)[subStrNums] = total;
            return total;
        }
    }

    //BASE CASE length of remaining string is 1
    return 1;
}
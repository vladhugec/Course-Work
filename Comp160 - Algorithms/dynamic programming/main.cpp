////////////////////////////////////////////////////////////////////////////
//
//    Tufts University, Comp 160 wordInterpret coding assignment  
//
//    main.cpp
//    word interpret
//
//    simple main to test wordInterpret
//    NOTE: this main is only for you to test wordInterpret. We will compile
//          your code against a different main in our autograder directory
//
////////////////////////////////////////////////////////////////////////////

#include <iostream>
#include "interpret.h"

using namespace std;


int main(void) {
    string inputNums = "12012";

    unsigned long total = wordInterpret(inputNums);

    if (total != 5) {
        cout << total << " Noo!\n";
    } else {
        cout << total << " Yay!\n";
    }

    string inputNums2 = "1234";

    unsigned long total2 = wordInterpret(inputNums2);

    if (total2 != 3) {
        cout << total2 << " Noo!\n";
    } else {
        cout << total2 << " Yay!\n";
    }

    string inputNums3 = "12345";

    unsigned long total3 = wordInterpret(inputNums3);

    if (total3 != 3) {
        cout << total3 << " Noo!\n";
    } else {
        cout << total3 << " Yay!\n";
    }

    string inputNums4 = "58021742558617695039";

    unsigned long total4 = wordInterpret(inputNums4);

    if (total4 != 0) {
        cout << total4 << " Noo!\n";
    } else {
        cout << total4 << " Yay!\n";
    }

    return 0;
}

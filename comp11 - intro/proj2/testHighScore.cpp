#include <iostream>
#include <fstream>

#include "highScoresList.h"

using namespace std;

int main() {

    HighScoresList hscore;
    hscore.print();
    cout << "------------------" << endl;
    hscore.insert("Vlad",22000);
    hscore.print();
    cout << "------------------" << endl;
    hscore.insert("John",21000);
    hscore.print();
    cout << "------------------" << endl;
    hscore.insert("Eric",4000);
    hscore.print();
    cout << "------------------" << endl;
    hscore.insert("Pete",5);
    hscore.print();
    cout << "------------------" << endl;
    hscore.print();
    hscore.save();


    return(0);
}
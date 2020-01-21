/* postoffice.cpp
 * Vladimir Hugec, COMP11 Spring 2018
 * 04/4/2018
 * main()
 */

#include <iostream>
#include <math.h>
#include <cmath>
#include <string>

#include "postnet.h"

using namespace std;

int main(int argc, char* argv[]) {
    if (argc == 4) {
        Postnet Post(argv[2], argv[3]);
        Postdata Data(Post.zipcode(), Post.destinationCode());
        Data.printLabel();
    }
    else {
        cerr << "Invalid parameters : Check input" << endl;
        return 1;
    }

    return 0;
}



/* postnet.cpp
 * Vladimir Hugec, COMP11 Spring 2018
 * 04/4/2018
 * postnet.h implementations
 */

#include <iostream>
#include <math.h>
#include <cmath>
#include <string>
#include "postnet.h"

using namespace std;

const double EARTH_RADIUS_KM = 6371.0;

// intToBin[location] -> where location represents the 
// digit and array[location] is the barcode conversion of the digit
const string intToBin[10] = {
    "11000","00011","00101","00110","01001",
//     0,      1,      2,      3,      4,
    "01010","01100","10001","10010","10100"}; 
//    5,       6,       7,     8,     9
// binToInt[10] || to intToBin[10] || to stringInts[10]
const char binToInt[10] = {'0','1','2','3','4','5','6','7','8','9'};
const string stringInts[10] = {"0","1","2","3","4","5","6","7","8","9"};
const int strToInt[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
const int intToDecimals[6] = {10000, 1000, 100, 10, 1, 1};

        ///                             ///
        /* POSTNET CLASS IMPLEMENTATIONS */
        ///                             ///

// zipcode constructor
Postnet::Postnet(int zipcode, int destination) {
    zip = zipcode; 
    destinationZip = destination;
}

// barcode constructor
Postnet::Postnet(string source,string destination) {
    if (source.length() > 5) {
        zip = convertToZip(source);
        destinationZip = convertToZip(destination);
    }
    else {
        zip = formInt(source);
        destinationZip = formInt(destination);
    }
}

//returns zipcode -> int
int Postnet::zipcode() {
    return zip;
}

// returns barcode -> string
string Postnet::barcode() {
    string bar = convertToBar(zip);
    return bar;
}
// retuns destination zipcode -> int
int Postnet::destinationCode() {
    return destinationZip;
}

//converts to zip -> int
int Postnet::convertToZip(string barcode) {
    string barArray[6];
    for (int i=5; i>=0; i--) { //save strings of 5-bin-digits
        barArray[i] = barcode.substr((i*5)+1,5);
    }
    int zipArray[5];
    for (int i=0; i<5; i++) {         //find int digit that corresponds to
        for (int j=9; j>=0; j--) {    // the string of 5-bin-digits
            if (barArray[i] == intToBin[j]) {
                zipArray[i] = strToInt[j]; //save it
            }
        }
    }
    int zipcode = 0;
    for (int i=0; i<5; i++) { //make the individual digits into number
        zipcode += zipArray[i] * intToDecimals[i];
    }
    return zipcode;
}

//converts zipcode to barcode -> string
string Postnet::convertToBar(int zipcode){
    int zipArray[5] = {0,0,0,0,0};
    int digits = zipcode;
    for (int i=4; i>=0; i--) {//%10 gives last digit -> /10 to see next digit
        int lastDigit = digits % 10;
        zipArray[i] = lastDigit;
        digits /= 10; // and repeat until you have 5 digits
    } 
    // converts each digit to corresponding Bin values, 
    // concatenate string of those value, and add 'check digit' to the end, +1
    int sumOfDigits = 0;
    for (int i=0; i<5; i++) {
        sumOfDigits += zipArray[i];
    }
    string binDigits = "1";
    for (int i=0; i<5; i++) {
        if (i==4) {
            binDigits = binDigits + intToBin[zipArray[i]];
            if ((sumOfDigits % 10) == 0) {
                binDigits += intToBin[0] + "1";
            }
            else {
                binDigits += intToBin[10 - (sumOfDigits % 10)]+ "1";
            }
        }
        else {
            binDigits += intToBin[zipArray[i]];
        }
    }
    return binDigits;
}

        ///                             ///
        /* POSTDATA CLASS IMPLEMENTATIONS */
        ///                             ///

// Postdata constructor:
//      handles document parsing -> store raw data & save the relavent data
Postdata::Postdata(int urZipcode, int destination){
    // This will parse the entire file
    bool foundUrZip = false; bool foundTheDest = false; bool foundAll = false;
    int dataSeries = 0; //location in array of structs
    while (!foundAll) { //repeat until source and destination are found
        for (int i = 0; i<5000; i++) {
            cin >> rData[dataSeries].zipcodes[i] >> rData[dataSeries].city[i] 
            >> rData[dataSeries].state[i] >> rData[dataSeries].latitude[i] >>
            rData[dataSeries].longitude[i];
                //save the data if its what were looking for
            if (rData[dataSeries].zipcodes[i] == urZipcode) {
                urZip = rData[dataSeries].zipcodes[i]; 
                urCity = rData[dataSeries].city[i];
                urLat = rData[dataSeries].latitude[i]; 
                urLong = rData[dataSeries].longitude[i];
                foundUrZip = true;
            }
            if (rData[dataSeries].zipcodes[i] == destination) {
                theZip = rData[dataSeries].zipcodes[i]; 
                theCity = rData[dataSeries].city[i];
                theLat = rData[dataSeries].latitude[i]; 
                theLong = rData[dataSeries].longitude[i];
                foundTheDest = true;
            } 
            if (foundUrZip && foundTheDest) { //when both found loop breaks
                foundAll = true;
                i = 5001;
            } // i think this is okay to do...
            else if (i == 4999 && dataSeries == 9) {
                dataSeries = 0; // just in case amount of data is enormous
            }                   // start over and rewrite the first location
            else if (i == 4999) {
                dataSeries++; // if all data has not yet been found by this
            }               // point, start in new data series
        }
    }
}

// prints the label
void Postdata::printLabel() {
    Location Source = {getLat(0), getLong(0)};
    Location Dest = {getLat(1), getLong(1)};
    cout << "src:" << " " << urZip << " " << getCity(0) << " " 
    << getState(0) << " " << getLat(0) << " " << getLong(0) << endl;

    cout << "dst:" << " " << theZip << " " << getCity(1) << " " 
    << getState(1) << " " << getLat(1) << " " << getLong(1) << endl;

    cout << "distance:" << " " << round((distance(Source,Dest)) * 10) / 10 
         << " " << "km" << endl;

    cout << "dst barcode:" << endl;
    cout << endl;
    Postnet Post(theZip,urZip);
    print_barcode(Post.barcode());
    cout << endl; cout << endl;
}

//returns name of city
string Postdata::getCity(int destOrNot) {
    if (destOrNot == 0) {
        return urCity;
    }
    else {
        return theCity;
    }
    return "faildd";
}

//returns country
string Postdata::getState(int destOrNot) {
    if (destOrNot == 0) {
        return urState;
    }
    else {
        return theState;
    }
    return "faildd";
}

//returns latitude
double Postdata::getLat(int destOrNot) {
    if (destOrNot == 0) {
        return urLat;
    }
    else {
        return theLat;
    }
    return 0.0;
}

//returns longitude 
double Postdata::getLong(int destOrNot) {
    if (destOrNot == 0) {
        return urLong;
    }
    else {
        return theLong;
    }
    return 0.0;
}


/* Purpose: print an ascii art barcode
 * Parameter: a binary string barcode consisting of (0's and 1's)
 */
void Postdata::print_barcode(string barcode) {
    int barcode_len = barcode.length();

    for (int i = 0; i < barcode_len; i++) {
        // Comp11 students are not expected to understand the line below
        // but if you are curious then google 'ternary operator'.
        // In short, the ternary operator is a conditional operator.
        cout << (barcode[i] == '0' ? " " : "|");
    }
    
    cout << endl;

    for (int i = 0; i < barcode_len; i++) {
        cout << "|";
    }
}

        ///                             ///
        /*  Helper Conversion Fucntions  */
        ///                             ///

//forms an int from string of nums -> int
int formInt(string something) {
    string zipArrayChar[5];
    for (int i=0; i<5; i++) { //pulls individual digits to array
        zipArrayChar[i] = something.substr(i, 1);
    }
    int zipArray[5];
    for (int j=0; j<5; j++) { //match each digit to corresponding
        for (int k=0; k<10; k++) {
            if (zipArrayChar[j] == stringInts[k]) {
                zipArray[j] = strToInt[k]; // store in new int array
            }
        }
    }
    int intZip = 0;
    for (int i=0; i<5; i++) {
        intZip += zipArray[i] * intToDecimals[i];  
        //turn the int array into an int number
        //places number in correct thousands,hundreths,etc. place
    }
    return intZip;
}

//forms a string of [0-9] digits -> string
string formString(int zipcode) {
    string zip = "";
    int zipArray[5] = {0,0,0,0,0};
    int digits = zipcode;

    for (int i=4; i>=0; i--) {
        int lastDigit = digits % 10;
        zipArray[i] = lastDigit;
        digits /= 10;
    }
    for (int i=0; i<5; i++) {
        for (int j=0; j<10; j++) {
            if (zipArray[i] == binToInt[j]) {
                zip += stringInts[j];
            }
        }
    }
    return zip;
}
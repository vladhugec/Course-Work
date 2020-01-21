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
#include <unordered_map>
#include <string>
#include "interpret.h"


using namespace std;

unsigned long addPaths(std::string subStrNums, std::unordered_map<std::string, unsigned long> *table);
std::pair<bool,unsigned long> checkForRepeatSubtree(std::string str, std::unordered_map<std::string, unsigned long> *table);
bool checkZeroCondition(std::string str);

bool checkZeroCondition(std::string str) {
    if (str.length() < 2) {
        return true;
    }
    else {
        string fst = str.substr(0,1);
        string snd = str.substr(1,1);
        if (snd == "0" && stol(fst+snd) > 27) {
            return false;
        }
        return checkZeroCondition(str.substr(1,string::npos));
    }
    return true;
}


// TODO: implement this function
// MUST follow the function signature below
unsigned long wordInterpret(string inputNums) {
    unordered_map<string, unsigned long> table;

    if (inputNums.substr(0,1) != "0" && checkZeroCondition(inputNums)) {
        addPaths(inputNums, &table);
        if (table.find(inputNums) != table.end()) {
            return table.at(inputNums);
        }
    }
    return 0;
}

unsigned long addPaths(string subStrNums, unordered_map<string, unsigned long> *table) {
    unsigned long total = 0;
    //check if input is long enough to prevent out of bound
    if (subStrNums.length() > 1) {
        string fst = subStrNums.substr(0,1);
        string snd = subStrNums.substr(1,1);
        
        string rem1 = subStrNums.substr(1,string::npos);
        string rem2 = subStrNums.substr(2,string::npos);

        pair<bool,unsigned long> rem1Check = checkForRepeatSubtree(rem1, table);
        pair<bool,unsigned long> rem2Check = checkForRepeatSubtree(rem2, table);

        //both problems have been solved before
        if (rem1Check.first && rem2Check.first) {
            if (stoi(fst+snd) < 27 && stoi(fst+snd) % 10 != 0) {
                    total += rem1Check.second + rem2Check.second;
                    (*table)[subStrNums] = total;
                    return total;
            }
            else {
                total += rem1Check.second;
                (*table)[subStrNums] = total;
                return total;
            }
        }
        //the first problem has been solved but not the second
        else if (rem1Check.first && !rem2Check.first) {
            if (stoi(fst+snd) < 27 && stoi(fst+snd) % 10 != 0) {
                    total += rem1Check.second + addPaths(rem2, table);
                    (*table)[subStrNums] = total;
                    return total;
            }
            else {
                total += rem1Check.second;
                (*table)[subStrNums] = total;
                return total;
            }
        }
        //the second problem has been solved but not the first
        else if (!rem1Check.first && rem2Check.first) {
            if (stoi(fst+snd) < 27 && stoi(fst+snd) % 10 != 0) {
                    total += (addPaths(rem1,table) + rem2Check.second);
                    (*table)[subStrNums] = total;
                    return total;
            }
            else {
                total += addPaths(rem1,table);
                (*table)[subStrNums] = total;
                return total;
            }
        }
        //neither problem has been solved
        else if (!rem1Check.first && !rem2Check.first) {
            if (stoi(fst+snd) < 27 && stoi(fst+snd) % 10 != 0) {
                    total += (addPaths(rem1,table) + addPaths(rem2,table));
                    (*table)[subStrNums] = total;
                    return total;
                }
            else {
                total += addPaths(rem1,table);
                (*table)[subStrNums] = total;
                return total;
            }
        }
    }

    if (subStrNums == "0") {
        return 0;
    }

    //BASE CASE length of remaining string is 0
    return 1;
}

pair<bool,unsigned long> checkForRepeatSubtree(string str, unordered_map<string, unsigned long> *table) {
    pair<bool,unsigned long> strInfo;

    if (table->find(str) == table->end()) {
        strInfo.first = false;
        strInfo.second = 0;
    }
    else {
        strInfo.first = true;
        strInfo.second = table->at(str);
    }
    
    return strInfo;
}
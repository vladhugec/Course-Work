////////////////////////////////////////////////////////////////////////////
//
//    Tufts University, Comp 160 wordInterpret coding assignment  
//
//    interpret.h
//    wordInterpret
//
//    includes function students need to implement
//
////////////////////////////////////////////////////////////////////////////


#pragma once

#include <unordered_map>


// function to be implemented
// MUST follow the function signature below
unsigned long wordInterpret(std::string inputNums);

//unsigned long addPaths(std::string subStrNums);

unsigned long addPaths(std::string subStrNums, std::unordered_map<std::string, unsigned long> *table);

std::pair<bool,unsigned long> checkForRepeatSubtree(std::string str, std::unordered_map<std::string, unsigned long> *table);

//std::pair<bool,unsigned long> checkForRepeatSubtree(std::string str);

//unsigned long recurseOnPath(std::string str);
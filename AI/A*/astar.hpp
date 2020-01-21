#ifndef ASTAR_HPP
#define ASTAR_HPP

#include "pancake.hpp"
#include <queue>
#include <iostream>

//ASTAR aglo implemented from the notes in the slides
sArray ASTAR(sNode, bool);

//checks if a state has already been visited by ASTAR
bool checkIfVisited (sNode);

//erases everything from visitedSet
void clearVisitedSet();

//outputs the state
void printState(sArray);

#endif
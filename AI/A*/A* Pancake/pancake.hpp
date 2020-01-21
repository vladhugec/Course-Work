#ifndef PANCAKE_HPP
#define PANCAKE_HPP 

#include <queue>
#include <array>
#include <set>

typedef std::array <int, 5> sArray;

typedef struct node {
    sArray state;
    int cost;
    bool visited;

    bool operator<(const node& next) const {
        return cost < next.cost;
    }

} sNode;

//returns true if two arrays are the same
bool arraysAreEqual(sArray, sArray);

//flips an array at a position
sArray flip(sArray state, int pos);

//returns a new node with a flipped state
sNode newState(int oldState[], int flipAt, int hCost);

//returns true if each element is less than the preceeding element
bool checkForSolution(sArray);

//will check if a node is already a part of the queue
//
//if it is not -> add it to queue
//if it is -> check if its cost is lower than the one in the queue
//          if that is true -> add it to the queue
//else -> dont add to queue
void checkinSet(sNode, std::priority_queue <sNode> *frontier);

//adds all possible flipped states to the priority queue
void expandNode(sNode, std::priority_queue <sNode>*);

//heuristic function -> taken from the assignment specs about possible
//heuristics for the pancake sorting problem
int heuristic(sArray);

//erases everything from checkSet
void clearCheckSet();

#endif
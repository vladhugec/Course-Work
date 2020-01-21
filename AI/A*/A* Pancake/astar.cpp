#include "astar.hpp"
#include "pancake.hpp"

#define FAILURE {0,0,0,0,0}
std::set <std::pair <sArray, int> > visitedSet;

sArray ASTAR(sNode start, bool verbose) {
    //initialize the frontier using the initial state of PROBLEM
    std::priority_queue <sNode> frontier;
    frontier.push(start);

    while (true) {
        if (frontier.empty()) {
            return FAILURE;
        }
        //pop node from frontier with min cost
        sNode currNode = frontier.top();
        frontier.pop();

        if (!checkIfVisited(currNode)) {
            visitedSet.insert(std::make_pair(currNode.state, currNode.cost));

            //if the node contains a goal state then return the corresponding SOLUTION
            if (checkForSolution(currNode.state)) {
                return currNode.state;
            }
            if (verbose) {
                printState(currNode.state);
            }
            expandNode(currNode, &frontier);
        }
    }
    return FAILURE;
}

bool checkIfVisited (sNode node) {
    std::set <std::pair <sArray, int> >::iterator iter;
    bool visited = false;

    for (iter = visitedSet.begin(); iter != visitedSet.end(); ++iter) { 
        std::pair <sArray, int> element = *iter;
        sArray setArray = element.first;

        if (arraysAreEqual(setArray, node.state)){
            visited = true;
        }
    }
    return visited;
}

void printState(sArray arr) {
    std::cout << "STATE -> [";
    for (int i = 0; i < 5; i++) {
        if (i != 4) {
            std::cout << arr[i] << ", ";
        }
        else {
            std::cout << arr[i] << "]" << std::endl;
        }
    }
}

//erases everything from visitedSet
void clearVisitedSet() {
    visitedSet.clear();
}

#undef FAILURE

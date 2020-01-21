#include "pancake.hpp"

// NOTE:::The following bit of code adds a "find" option to the priority queue in STD
//        Since the native priority queue does not have a way of checking whether or not
//        An element is already a part of the queue, I needed to add in this functionality
//        I will check if an element is a part of the set, and if it is a part of the set, 
//        it is already in the queue
std::set <std::pair <sArray, int> > checkSet;
void checkinSet(sNode elem, std::priority_queue <sNode> *frontier) {
    std::set <std::pair <sArray, int> >::iterator iter;

    sArray nodeArray = elem.state;
    int nodeCost = elem.cost;
    bool nodeVisited = elem.visited;
    bool inSet = false;

    for (iter = checkSet.begin(); iter != checkSet.end(); ++iter) { 

        std::pair <sArray, int> element = *iter;
        sArray setArray = element.first;
        int setCost = element.second;

        //element is already in frontier
        if (arraysAreEqual(setArray, nodeArray)){
            inSet = true;
            //if cost is less in the node, then add it to frontier
            if (nodeCost < setCost) {
                frontier->push(elem);
                checkSet.insert(std::make_pair(elem.state, elem.cost));
            }
        }
    }
    //if not in frontier, add to frontier
    if (!inSet) {
        frontier->push(elem);
        checkSet.insert(std::make_pair(elem.state, elem.cost));
    }
}

//erases everything from checkSet
void clearCheckSet() {
    checkSet.clear();
}

sArray flip(sArray state, int pos) {
    //copy array into new array
    sArray flipped = state;
    int temp; 
    int i = 0;

    while (i < pos) {  
        temp = flipped[i];
        flipped[i] = flipped[pos];  
        flipped[pos] = temp;
        i++;  
        pos--;  
    }  
    return flipped;
}

sNode newState(sArray arr, int flipAt, int hCost) {
    sArray newArr = flip(arr, flipAt);
    int totalCost = hCost + flipAt + 1;

    sNode newNode = {newArr, totalCost, false};

    return newNode;
}

bool checkForSolution(sArray state) {
    bool valid = true;

    for (int i = 0; i < 5; i++) {
        if (i == 4 && valid) {
            return valid;
        }
        else if (state[i] < state[i+1]) {
            return false;
        }
    }

    return valid;
}

void expandNode(sNode currNode, std::priority_queue <sNode> *frontier) {
    sNode child1 = newState(currNode.state, 1, 1 + currNode.cost + heuristic(flip(currNode.state, 1)));
    sNode child2 = newState(currNode.state, 2, 2 + currNode.cost + heuristic(flip(currNode.state, 2)));
    sNode child3 = newState(currNode.state, 3, 3 + currNode.cost + heuristic(flip(currNode.state, 3)));
    sNode child4 = newState(currNode.state, 4, 4 + currNode.cost + heuristic(flip(currNode.state, 4)));

    checkinSet(child1, frontier);
    checkinSet(child2, frontier);
    checkinSet(child3, frontier);
    checkinSet(child4, frontier);
}

int heuristic(sArray arr) {
    int cost = 0;
    for (int i = 0; i < 5; i++) {
        if (i != 4) {
            if (arr[i] + 1 == arr[i+1]) {
                cost++;
            }
            if (arr[i] - 1 == arr[i+1]) {
                cost++;
            }
        }
    }
    return cost;
}

bool arraysAreEqual(sArray s1, sArray s2) {
    bool areEqual = true;

    for (int i = 0; i < 5; i++) {
        if (s1[i] != s2[i]) {
            areEqual = false;
        }
    }

    return areEqual;
}

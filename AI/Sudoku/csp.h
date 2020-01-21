#ifndef CSP_H
#define CSP_H

#include <iostream>
#include <map>
#include <set>
#include <queue>
#include <iterator>
#include <string>

class csp {
    private:
        // set of numbers 1-9 i.e {1,2,3,4,5,6,7,8,9}
        std::set <int> domain;

        // map of Variables and each vars associated domains
        //     i.e "A1" -> {1,2,3,4,5,6,7,8,9}
        //     i.e "A2" -> {1}
        std::map <std::string, std::set <int> > varDom;

        // map of Blocks and each index associated in Block
        //     i.e "1" -> {A1,A2,A3,B1,B2,B3,C1,C2,C3}
        std::map <int, std::set <std::string> > blocks;

    public:
        //builds a full domain of numbers 1-9
        void setDomain();

        //returns a full domain of numbers 1-9
        std::set <int> getDomain();

        //sets an indexes domain in the varDom map
        //  "A1" -> {1,2,3,4,5,6,7,8,9}
        void setVariableDomain(std::string, std::set <int>);

        //removes an int from the domain
        //  i.e "A1" -> {1,2,3} --> "A1" -> {1,2}
        void removeValFromDomain(csp*, std::string, int);

        //DEBUGING FUNC -> prints each index with corresponding domain
        void printVarDom();

        //prints the varDom in sudoku style
        void printSoduku();

        //creates a map of all the blocks and the indexes that correspond to each block
        void setBlocks();

        //returns the block map
        std::map <int, std::set <std::string> > getBlocks();

        //returns the block that corresponds to a certain index
        //   i.e WhichBlock(&CSP, "A1") -> retuns block #1
        //       WhichBlock(&CSP, "C7") -> returns block #3
        std::set <std::string> whichBlock(csp*, std::string);

        //returns the map of all indexs to their respective domains
        std::map <std::string, std::set <int> > getVariableDomain();

        //creates a set of indexs that are all neighboring eachother
        std::set <std::string> getNeighbors(csp*, std::string, std::string);
        
        // returns true if the two input indexes neighbor eachother
        bool isNeighbor(std::string,std::string);

        // returns a queue of pairs of neighbors needed for the AC-3 algorithm
        std::queue < std::pair <std::string, std::string> > setArcs();

        //returns true if each index has only 1 element in its domain
        bool checkForSolution();

};

#endif

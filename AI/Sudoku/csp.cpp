#include "csp.h"

void csp::setDomain() {
    for (int i = 1; i < 10; i++) {
        this->domain.insert(i);
    }
}

std::set <int> csp::getDomain() {
    return this->domain;
}

void csp::setVariableDomain(std::string index, std::set <int> dom) {
    this->varDom.insert({index, dom});
}

void csp::removeValFromDomain(csp* CSP, std::string index, int val) {
    std::set <int> indexDom = this->varDom.at(index);
    this->varDom.erase(index);
    indexDom.erase(val);
    this->varDom.insert({index, indexDom});
}

//DEBUG FUNC
void csp::printVarDom() {
    std::map <std::string, std::set <int>>::iterator mapIter;
    std::set <int>::iterator setIter;

    for (mapIter = this->varDom.begin(); mapIter != this->varDom.end(); ++mapIter) {  
        std::cerr << '\t' << mapIter->first << " -> {";
        for (setIter = mapIter->second.begin(); setIter != mapIter->second.end(); ++setIter) { 
            std::cerr << *setIter << ", ";
        } 
        std::cerr << "} " << std::endl;
    }
}

void csp::printSoduku() {
    std::map <std::string, std::set <int>>::iterator mapIter;
    std::set <int>::iterator setIter;

    int col_count = 0;
    int row_count = 0;
    std::cout << "--------------------" << std::endl;
    for (mapIter = this->varDom.begin(); mapIter != this->varDom.end(); ++mapIter) {
        if (mapIter->second.size() == 1) {
            for (setIter = mapIter->second.begin(); setIter != mapIter->second.end(); ++setIter) { 
                std::cout << *setIter; col_count++;
            }
        }  
        else {
            std::cout << ' '; col_count++;
        }
        if ((col_count % 3 == 0) && (col_count != 9)) {
            std::cout << "||";
        }
        else {
            std::cout << "|";
        }
        if (col_count == 9) {
            col_count = 0;
            row_count++;
            std::cout << std::endl;
        }
        if (row_count == 3) {
            std::cout << "--------------------" << std::endl;
            row_count = 0;
        }
    }
}


//WARNING: THIS FUNCTION HAS GROSS CODE
//         I couldn't think of a way to get this done that was easier
//         Than this, it is quite an eye-sore though...
void csp::setBlocks() {
    //Block 1 -> ABC 123 X
    std::set <std::string> block1;
    for (char c = 'A'; c <= 'C'; c++) {
        for (int i = 1; i <= 3; i++) {
            std::string index = c + std::to_string(i);
            block1.emplace(index);
        }
    }
    this->blocks.insert({1, block1});

    //Block 2 -> ABC 456 X
    std::set <std::string> block2;
    for (char c = 'A'; c <= 'C'; c++) {
        for (int i = 4; i <= 6; i++) {
            std::string index = c + std::to_string(i);
            block2.emplace(index);
        }
    }
    this->blocks.insert({2, block2});

    //Block 3 -> ABC 789 X
    std::set <std::string> block3;
    for (char c = 'A'; c <= 'C'; c++) {
        for (int i = 7; i <= 9; i++) {
            std::string index = c + std::to_string(i);
            block3.emplace(index);
        }
    }
    this->blocks.insert({3, block3});

    //Block 4 -> DEF 123
    std::set <std::string> block4;
    for (char c = 'D'; c <= 'F'; c++) {
        for (int i = 1; i <= 3; i++) {
            std::string index = c + std::to_string(i);
            block4.emplace(index);
        }
    }
    this->blocks.insert({4, block4});

    //Block 5 -> DEF 456 X
    std::set <std::string> block5;
    for (char c = 'D'; c <= 'F'; c++) {
        for (int i = 4; i <= 6; i++) {
            std::string index = c + std::to_string(i);
            block5.emplace(index);
        }
    }
    this->blocks.insert({5, block5});

    //Block 6 -> DEF 789
    std::set <std::string> block6;
    for (char c = 'D'; c <= 'F'; c++) {
        for (int i = 7; i <= 9; i++) {
            std::string index = c + std::to_string(i);
            block6.emplace(index);
        }
    }
    this->blocks.insert({6, block6});

    //Block 7 -> GHI 123
    std::set <std::string> block7;
    for (char c = 'G'; c <= 'I'; c++) {
        for (int i = 1; i <= 3; i++) {
            std::string index = c + std::to_string(i);
            block7.emplace(index);
        }
    }
    this->blocks.insert({7, block7});

    //Block 8 -> GHI 456
    std::set <std::string> block8;
    for (char c = 'G'; c <= 'I'; c++) {
        for (int i = 4; i <= 6; i++) {
            std::string index = c + std::to_string(i);
            block8.emplace(index);
        }
    }
    this->blocks.insert({8, block8});

    //Block 9 -> GHI 789
    std::set <std::string> block9;
    for (char c = 'G'; c <= 'I'; c++) {
        for (int i = 7; i <= 9; i++) {
            std::string index = c + std::to_string(i);
            block9.emplace(index);
        }
    }
    this->blocks.insert({9, block9});
}

std::map <int, std::set <std::string> > csp::getBlocks() {
    return this->blocks;
}

std::set <std::string> csp::whichBlock(csp *CSP, std::string Xi) {
    std::set <std::string> block;

    for (int i = 1; i <= 9; i++) {
        block = CSP->getBlocks().at(i);

        if (block.count(Xi) == 1) {
            return block;
        }
    }
    return block;
}

std::set <std::string> csp::getNeighbors(csp *CSP, std::string Xi, std::string Xj) {
    std::set <std::string> neighbors = CSP->whichBlock(CSP, Xi);
    std::string row = Xi.substr(0, 1);
    std::string col = Xi.substr(1, 1);

    // add to set all indexes in the same row as input to neighbors set
    for (int i = 1; i <= 9; i++) {
        std::string index = row + std::to_string(i);
        if (index != Xj) {
            neighbors.emplace(index);
        }
    }

    // add to set all indexes in the same column as input to neighbors set
    for (char c = 'A'; c <= 'I'; c++) {
        std::string index = c + col;
        if (index != Xj) {
            neighbors.emplace(index);
        }
    }

    return neighbors;
}

bool csp::isNeighbor(std::string x, std::string y) {
    std::string rowX = x.substr(0, 1);
    std::string colX = x.substr(1, 1);
    std::string rowY = y.substr(0, 1);
    std::string colY = y.substr(1, 1);
    bool neighbors = false;

    // if they are the same value, they cant be neighbors
    if (x == y) {
        return false;
    }

    //if they are in the same row or col -> they are neighbors
    if ((rowX == rowY) || (colX == colY)) {
        neighbors = true;
    }
    else {
        std::map <int, std::set <std::string> >::iterator mapIter;
        std::set <std::string> blockset;

        //itereate through all 9 blocks and look for a block that both inputs are in
        for (mapIter = this->blocks.begin(); mapIter != this->blocks.end(); ++mapIter) {
            blockset = mapIter->second;
            
            // if both values are in the same block -> return true
            if ((blockset.count(x) == 1) && (blockset.count(y) == 1)) {
                    neighbors = true;
            }
        }
    }

    return neighbors;
}

bool csp::checkForSolution() {
    std::map <std::string, std::set <int> >::iterator mapIter;
        std::set <int> domSet;

        //iterate through the entire puzzle
        for (mapIter = this->varDom.begin(); mapIter != this->varDom.end(); ++mapIter) {
            domSet = mapIter->second;

            if (domSet.size() > 1) {
                return false;
            }
        }
    return true;
}

std::queue < std::pair <std::string, std::string> > csp::setArcs() {
    std::queue < std::pair <std::string, std::string> > qArcs;

    for (char a = 'A'; a <= 'I'; a++) {
        for (int z = 1; z < 10; z++) {
            for (char l = 'A'; l <= 'I'; ++l) {
                for (int i = 1; i < 10; i++) {
                    std::string Xi = a + std::to_string(z);
                    std::string Xj = l + std::to_string(i);

                    //if they are not neighbors there is no need to add that arc
                    //since the two indexes do not directly influence one another
                    if (isNeighbor(Xi, Xj)) {
                        qArcs.push(std::make_pair(Xi, Xj));
                    }
                }
            }
        }
    }
    return qArcs;
}

std::map <std::string, std::set <int> > csp::getVariableDomain() {
    return this->varDom;
}

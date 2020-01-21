#include "csp.h"
#include "ac3.h"

void getPuzzleInput(csp*);
void solvePuzzle(csp*);

int main(int argc, char const *argv[])
{
    csp CSP = csp();
    CSP.setDomain();
    CSP.setBlocks();
    
    getPuzzleInput(&CSP);
    std::cout << "YOUR SUDOKU PUZZLE" << std::endl;
    CSP.printSoduku();

    solvePuzzle(&CSP);
    CSP.printSoduku();

    return 0;
}

void getPuzzleInput(csp* CSP) {
    std::string row_input;
    char row = 'A';

    for (int i = 0; i < 9; i++) {
        bool valid_input = false;

        while (!valid_input) {
            std::cout << "Enter row #" << i+1 << " (e.g. xxx4xx5x7) : ";
            std::cin >> row_input;
            std::cout << std::endl;

            if (row_input.length() == 9) {
                valid_input = true;
            }
            else {
                std::cout << "Invalid Input, please re-enter row #" << i+1 << std::endl << std::endl;
            }
        }

        for (int j = 0; j < 9; j++) {
            std::string index = row + std::to_string(j+1);
            std::string value = row_input.substr(j, 1);

            if (value == "x") {
                CSP->setVariableDomain(index, CSP->getDomain());
            }
            else {
                int val = std::stoi(value);
                CSP->setVariableDomain(index, {val});
            }
        }
        row++;
    }
    std::cout << std::endl;
}

void solvePuzzle(csp* CSP) {
    bool solved = false;
    int count = 0;
    std::cout << std::endl << "---SOLVING PUZZLE---" << std::endl;

    while (!solved) {
        ac3(CSP);
        solved = CSP->checkForSolution();
        count++;
        if (count == 2) {
            std::cout << "...working..." << std::endl;
        }
        if (count == 3) {
            std::cout << "...working harder..." << std::endl;
        }
        if (count == 4) {
            std::cout << "...still working..." << std::endl;
        }
        if (count == 5) {
            std::cout << "...be patient..." << std::endl;
        }
        if (count == 6) {
            std::cout << "...patience is a virtue..." << std::endl;
        }
        if (count == 15) {
            std::cout << "...this one might take a little bit..." << std::endl;
        }
        if (count == 75) {
            std::cout << "...I CANNOT SOLVE THIS PUZZLE :(..." << std::endl;
            break;
        }
    }
    std::cout << std::endl << std::endl << std::endl;
}

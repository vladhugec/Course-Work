#include "astar.hpp"
#include "pancake.hpp"

sArray userInput();
void fileInput(bool);
void outputResult(sArray);

int main (int argc, char const *argv[]) {

    bool verbose = false;
    bool batchTesting = false;
    if (argc > 1) {
        for (int i = 1; i < argc; i++) {
            if (strncmp(argv[i],"-v", 2) == 0) {
                //verbose mode initiated
                std::cout << std::endl << "VERBOSE MODE ---- Will output each step in the flip process." << std::endl << std::endl;
                verbose = true;
            }
            if (strncmp(argv[i],"-t", 2) == 0) {
                //batch testing mode initiated
                std::cout << std::endl << "BATCH TESTING MODE ---- Will test all inputs from inputted file" << std::endl << std::endl;
                batchTesting = true;
            }
        }
    }
    else if (!verbose && !batchTesting && argc > 1) {
        std::cout << "Invalid command line parameters ---- will proceed in default mode." << std::endl;
    }

    //initiate search
    if (batchTesting) {
        fileInput(verbose);
    }
    else {
        sArray arr = userInput();
        sNode start = {arr, 0, false};
        sArray final = ASTAR(start, verbose);
        outputResult(final);
    }

    return 0;
}

sArray userInput() {
    //prompt input
    std::string pStack;
    while (pStack.length() != 5) {
        std::cout << "ENTER a 5 digit pancake stack to be sorted (e.g 43521) : ";
        std::cin >> pStack;

        if (pStack.length() != 5) {
            std::cout << std::endl << "Please enter only 5 digits.." << std::endl << std::endl;
        }
    }

    //create array out of input
    sArray arr;
    for (int i = 0; i < 5; i++) {
        arr[i] = std::stoi(pStack.substr(i, 1));
    }

    return arr;
}

void fileInput(bool verbose) {
    std::string pStack;
    sNode start;
    sArray final;
    sArray arr;

    while (std::getline(std::cin, pStack)) {
        std::cout << "--------------------------------------------------" << std::endl;

        //create array out of input
        for (int i = 0; i < 5; i++) {
            arr[i] = std::stoi(pStack.substr(i, 1));
        }

        start = {arr, 0, false};
        final = ASTAR(start, verbose);
        outputResult(final);
        clearVisitedSet();
        clearCheckSet();
        std::cout << "--------------------------------------------------" << std::endl;
    }
}

void outputResult(sArray final) {
    //check for program failure
    bool failure = arraysAreEqual(final, {0,0,0,0,0});

    // if program has not failed -> output final state
    if (!failure) {
        std::cout << "FINAL ";
        printState(final);
    }
    else {
        std::cout << "A problem has occurred, unable to stack pancakes correctly." << std::endl;
    }
}

#include <iostream>
#include <cmath>

#include "game.h"
#include "highScoresList.h"
#include "int-string.h"

using namespace std;

const string RIGHT = "d";
const string LEFT = "a";
const string UP = "w";
const string DOWN = "s";
const string PRINTtop5 = "h";
const string QUIT = "q";
const string CLEAR = "clear";
const string KEEP = "keep";
const string SHOW = "show";
const string sNULL = "NULL";

const double PROBof4 = 0.1; //10% chance of spawning a 4, 90% of a 2
const double PROBof2 = 0.9;
const double PROBofSpawn = 0.25;

//Game class constructor
// does constructing of what is written below
Game::Game() {
    srand(time(NULL));
    boardSize = getBoardSize();
    board = new Position[boardSize*boardSize];
    initializeBoard();
    gameOver = false;
    iName = sNULL;
    pairCount = 0;
    mergerPos = 0;
    score = 0;
    HighScoresList hscore;
}

//Game class destructor
// does the destructing of that which was constructed
Game::~Game() {
    delete [] board;
    board = NULL;
    gameOver = true;
    score = 0;
}
// play - 
// returns nothing 
// takes nothing -
// runs the game logic
void Game::play() {
    cout << "Starting Game! Highscore is: " << hscore.highestScore() << endl;
    printBoard();
    while (!gameOver) {
        getInput();
        resolveInput();
    }
    if (gameOver == true) {
        cout << "Game over! Enter your name to record your score: ";
        string userName;
        cin >> userName;
        hscore.insert(userName, score);
        hscore.printTop5();
        hscore.save();
    }
}
// gameLoop()
// returns nothing
//   Loops the game
void Game::gameLoop() {
    spawnRandom(); 
    checkGameOver();
    printBoard();
}

// initializeBoard()
// this initializes the board with 2 randomly placed 2's
void Game::initializeBoard() {
    int maxSpawn = 2; int spawnd = 0;
    while (spawnd < maxSpawn) { // make sure at least 2 have spawnd
        for (int r = 0; r < boardSize; r++) {
            for (int c = 0; c < boardSize; c++) {
                int randomNum = rand() % 100;
                int pos = (r*boardSize) + c;

                board[pos].row = r;
                board[pos].col = c;
                if (randomNum <= PROBofSpawn * 100) {
                    randomNum = rand() % 100;

                    if (randomNum <= PROBof2 * 100 
                    && spawnd < maxSpawn) {
                        board[pos].value = 2;
                        spawnd++;
                    }
                    else {
                        board[pos].value = 0;
                    }
                }
                else {
                    board[pos].value = 0;
                }
            }
        }
    }
}


// print_number() - takes a string
// this does printing of numbers
void Game::print_number(string num) {
    printf("%-6s", num.c_str());
}

// resolveInput()
// this handles the resolving of the input
//   calls functions based on user input
void Game::resolveInput() {
    if (input == RIGHT || input == LEFT) {
        horizontalInput();
        gameLoop();
    }
    else if (input == UP || input == DOWN) {
        verticalInput();
        gameLoop();
    }
    else if (input == PRINTtop5) {
        hscore.printTop5();
    }
    else if (input == QUIT) {
        gameOver = true;
    }
    else if (input == CLEAR) {
        hscore.clear();
        cout << "MEM CLEAR HAS RAN" << endl;
    }
    else if (input == KEEP) {
        hscore.keepTop10();
    }
    else if (input == SHOW && iName != sNULL) {
        hscore.printUser(iName);
    }
    else {
        cout << "Command not recognized. Please try again." << endl;
    } 
}

// movementLogic()
// this sets up the logic of what the current board spot
//   gets compared to
// this is based on direction of input
void Game::movementLogic(int row, int col, int mergedCount) {
    int nextRow = -1, nextCol = -1, direc = -1, bound = -1;
    if (input == UP) {
        nextRow = row-1;
        nextCol = col;
        direc = nextRow; // this is so I can check if direc >/< the boundry
        bound = -1; // boundary
    }
    else if (input == DOWN) {
        nextRow = row+1;
        nextCol = col;
        direc = nextRow;
        bound = boardSize;
    }
    else if (input == LEFT) {
        nextRow = row;
        nextCol = col-1;
        direc = nextCol;
        bound = -1;
    }
    else if (input == RIGHT) {
        nextRow = row;
        nextCol = col+1;
        direc = nextCol;
        bound = boardSize ;
    }
    
    move(row, col, nextRow, nextCol, direc, bound, mergedCount);
}

// move()
//   this handles the actual movement on the board
// takes the current position (row col) and next position (nRow nCol)
// and compares the current to the next
void Game::move(int row, int col, int nRow, int nCol, int direc, 
                int bound, int mergedCount) {
    int nextPos = -1; int currPos = -1; bool moved = false;
    if (nRow >= 0 && nCol >= 0) { //check if board spot exists
        if (nRow < boardSize && nCol < boardSize) {
            int pos = 0;
            // loop through array and search for actual location in array
            // of row col / nrow ncol
            while (nextPos == -1 || currPos == -1) {
                if (board[pos].row == nRow && board[pos].col == nCol) {
                    nextPos = pos;
                }
                else if (board[pos].row == row && board[pos].col == col) {
                    currPos = pos;
                }
                pos++;
                if (pos > boardSize*boardSize) {
                    break;
                }
            }
            if (board[nextPos].value == 0 && board[currPos].value > 0) {
                board[nextPos].value = board[currPos].value;
                board[currPos].value = 0;
                moved = true;
            }
            if (board[nextPos].value == board[currPos].value 
            && mergedCount < pairCount){
                board[nextPos].value += board[currPos].value;
                board[currPos].value = 0;
                mergedCount++; 
                moved = true;
                updateScore(board[nextPos].value);
            }
            // if you havent reached the boundry and youve made a move
            // call movementLogic with the position (row col) of where
            // you just moved
            if (direc != bound && moved == true) {
                movementLogic(nRow, nCol, mergedCount);
            }
        }
    }
}
// countMerges()
// this resets the mergedCount to 0 if the row or col input gets changed
int Game::countMerges(int pos, int mergedCount) {
    if (pos == 0) {
        mergerPos = pos;
    }
    if (pos > mergerPos) {
        mergerPos = pos;
        mergedCount = 0;
    }
    return mergedCount;

}

// horizontalInput()
//   this creates a loop for the entire board for horizontal directions
//  calls movementLogic with each specific row and col to be moved
//  calls horizontal pairs and saves the pair count for specific row
//  calls countMerges to keep track of pairs merged in row
//    to make sure mergedCount doesnt exceed the pair count
void Game::horizontalInput() {
    if (input == RIGHT) {
        int mergedCount = 0;
        for (int r = 0; r < boardSize; r++) {
            pairCount = horizontalPairs(r);
            mergedCount = countMerges(r, mergedCount);
            for (int c = boardSize-1; c >= 0; c--) {
                movementLogic(r, c, mergedCount);
            }
        }
    }
    else if (input == LEFT) {
        int mergedCount = 0;
        for (int r = 0; r < boardSize; r++) {
            pairCount = horizontalPairs(r);
            mergedCount = countMerges(r, mergedCount);
            for (int c = 0; c < boardSize; c++) {
                movementLogic(r, c, mergedCount);
            }
        }
    }
}
// verticalInput()
//   this creates a loop for the entire board for vertical directions
//      calls same functions as horizontalInput
//      but it calls the vertical varients
void Game::verticalInput() {
    if (input == UP) {
        int mergedCount = 0;
        for (int c = 0; c < boardSize; c++) {
            pairCount = verticalPairs(c);
            mergedCount = countMerges(c, mergedCount);
            for (int r = 0; r < boardSize; r++) {
                movementLogic(r, c, mergedCount);
            }
        }
    }
    else if (input == DOWN) {
        int mergedCount = 0;
        for (int c = 0; c < boardSize; c++) {
            pairCount = verticalPairs(c);
            mergedCount = countMerges(c, mergedCount);
            for (int r = boardSize - 1; r >= 0; r--) {
                movementLogic(r, c, mergedCount);
            }
        }
    }
}

// horizontalPairs() takes integer Row
//   this RETURNS the number of pairs found in a specific row
int Game::horizontalPairs(int row) {
    int startPos = (row * boardSize);
    int endPos = (row * boardSize) + boardSize;
    int pairs = 0; int pairLoc[boardSize*boardSize];
      //initialize pair locations found to NULL (NULL = no pair there)
    for (int i = 0; i < boardSize*boardSize; i++) {
        pairLoc[i] = 0;
    }

    for (int c = startPos; c < endPos; c++) {
        if (board[c].value != 0 ) {
            int testPos = c + 1;
            while (board[testPos].value == 0 || pairLoc[c] != 0) {
                testPos++;
                if (testPos > endPos){
                    break;
                }
            }
            if (board[c].value == board[testPos].value) {
                pairs++;
                pairLoc[testPos] = 1; // set location to 1 (1 = pair there)
            }
        }
    } 
    return pairs;
}
// horizontalPairs() - takes integer column
//   this RETURNS the number of pairs found in a specific column
int Game::verticalPairs(int col) {
    int startPos = col;
    int endPos = boardSize*boardSize;
    int pairs = 0; int pairLoc[boardSize*boardSize];
      //initialize pair locations to NULL same as horizontalPairs()
    for (int i = 0; i < boardSize*boardSize; i++) {
        pairLoc[i] = 0;
    }

    for (int r = startPos; r < endPos; r += boardSize) {
        if (board[r].value != 0) {
            int testPos = r + boardSize;
            while (board[testPos].value == 0 || pairLoc[r] != 0) {
                testPos += boardSize;
                if (testPos > endPos){
                    break;
                }
            }
            if (board[r].value == board[testPos].value) {
                pairs++;
                pairLoc[testPos] = 1;
            }
        }
    }
    return pairs;
}

// getInput()
// this gets the input from user
void Game::getInput(){
    cin >> input;
    if (input == SHOW) {
        cin >> iName;
    }
}
// printBoard()
// this print the board
void Game::printBoard() {
    cout << "Current score: " << score << endl;
    int count = 0;
    for (int i = 0; i < boardSize*boardSize; i++) {
        if (board[i].value == 0) {
            print_number("-");
            count++;
        }
        else {
            print_number(int2string(board[i].value));
            count++;
        }
        if (count == boardSize) {
            cout << endl;
            count = 0;
        }
    }
}
// this gets the boardsize from the user
// RETURNS the boardsize got gotten from the user
int Game::getBoardSize() {
    int boardSize;
    cout << "Enter desired dimension of board: "; cin >> boardSize;
    cout << endl;
    if (boardSize <= 2){
        cerr << "Error: Dimension of board must be greater than 2" << endl;
        exit(1);
    }
    return boardSize;
}

// updateScore() - takes integer of Points scored
// updates the score in Game class
void Game::updateScore(int points) {
    score += points;
}

// spawnRandom()
// spawns a random 2 or 4 after a board move has been completed
void Game::spawnRandom() {
    int spawnd = 0; int maxSpawn = 1;
    while (spawnd < 1) { //make sure at least 1 thing has spawned
        for (int r = 0; r < boardSize; r++) {
            for (int c = 0; c < boardSize; c++) {

                int randomNum = rand() % 100;
                int pos = (r*boardSize) + c;

                if (randomNum <= PROBofSpawn * 100 
                && spawnd < maxSpawn && board[pos].value == 0) {
                    randomNum = rand() % 100;

                    if (randomNum <= PROBof4 * 100 
                    && spawnd < maxSpawn) {
                        board[pos].value = 4;
                        spawnd++;
                    }
                    else if (randomNum <= PROBof2 * 100 
                    && spawnd < maxSpawn) {
                        board[pos].value = 2;
                        spawnd++;
                    }
                }
            }
        }
    }
}

// checkGameOver() 
// checks if the game is over or not
//  if it is -> set gameOver = true
void Game::checkGameOver() {
    int numZeros = 0;
    int numPairs = 0;
    for (int r = 0; r < boardSize; r++) {
        numPairs += horizontalPairs(r);
        numPairs += verticalPairs(r);
        for (int c = 0; c < boardSize; c++) {
            int pos = ((r*boardSize) + c);

            if (board[pos].value == 0) {
                numZeros++;
            }
        }
    }
    if (numZeros == 0 && numPairs == 0) {
        gameOver = true;
    }
}


#ifndef GAME_H
#define GAME_H

#include <string>
#include "highScoresList.h"

class Game {
    public:
        Game();
        ~Game();
        void play();
    private:
         // function declarations
        void print_number(std::string num);
        void resolveInput();
        void getInput();
        int getBoardSize();
        void initializeBoard();
        void printBoard();
        void spawnRandom();
        void checkGameOver();
        void gameLoop();
        void updateScore(int points);
        void movementLogic(int row, int col, int mergedCount);
        void move(int row, int col, int nRow, int nCol, int direc, int bound, int mergedCount);
        int countMerges(int pos, int mergedCount);
        void horizontalInput();
        void verticalInput();
        int horizontalPairs(int row);
        int verticalPairs(int col);

         // variable declarations
        struct Position {
            int row;
            int col;
            int value;
        };
        Position *board;
        int score;
        bool gameOver;
        int boardSize;

         // holds varying inputs
        std::string input;
        std::string iName;

         // holds the count for a particular row when moving
        int pairCount;
        int mergerPos;

         // highscores list helper object
        HighScoresList hscore;


};

#endif

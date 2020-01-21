/* 
 * minesweeper.cpp 
 *
 * A text-based minesweeper game that runs in the terminal.
 *
 * COMP11 Spring 2018
 * Modified by: Vladimir Hugec
 * Date: March 9, 2018
 */

#include <iostream>
#include <time.h> 
#include <cstdlib>

using namespace std;

#ifdef TESTING
#define RANDOMSEED 1
#else
#define RANDOMSEED time(NULL)
#endif

/* Constants */
const int DIMEN = 9;
const double PROB = 0.3;
const char MINE = 'x';
const char UNSEEN = '-';

 // converter for int -> char
const char digits[] = {'0', '1', '2', '3', '4', '5', '6', '7', '8'};


/* Game struct definition */
struct Game {
    char board[DIMEN][DIMEN];
    char mines[DIMEN][DIMEN];
    bool game_over;
    int  num_mines;
};

/* Function prototypes */

  // Initializes Game struct
Game init_game();

 // Initializes board with UNSEEN char
Game populate_plain_board(Game game);

 // Prints the user-seen game board
void print_game_board(Game game);

 // FOR DEBUGGING
 // Prints the board with the location of the mines
void print_mines_board(Game game);

 // Initializes user-unseen game board with locations of mines
Game populate_mines(Game game);

 // Merges unseen-mine board with seen-user board IN THE CASE of GAME OVER
void board_w_mines(Game &game);

 // Returns the number of mines surrounding a given square
int num_mines_in_vicinity(Game game, int row, int col);

 // Returns a bool, true if user-selected square is a mine, false otherwise
bool was_a_mine(Game game, int row, int col);

 // Returns a bool, true if the user has uncovered all non-mine squares
bool check_for_win(Game &game);

 // Checks whether or not user input is valid
bool check_input(Game game, int row, int col);


int main() {
    srand(RANDOMSEED); // Seed pseudo-random # generator with current time
    Game game = init_game();
    print_game_board(game);
//  print_mines_board(game); //DEBUGGING PURPOSES // comment out as needed 
    while (!game.game_over) { //start game loop
////////////////////* MAIN GAME LOOP STARTS BELOW *///////////////////////////
int row; int col;
cout << "Enter the row and column of the square to uncover: ";
cin >> row >> col;

if (check_input(game, row, col)) { // continue game if input is valid, then:       
// if the user didnt hit a mine, update board with # surrounding mines
    if (not was_a_mine(game, row, col)) {
        game.board[row][col] = digits[num_mines_in_vicinity(game, row, col)];
        print_game_board(game); }

    else { // when the player loses:
        board_w_mines(game); print_game_board(game);
        cout << "GAME OVER. YOU LOST!" << endl;
        game.game_over = true; }

    if (check_for_win(game)) {
        print_game_board(game); 
        cout << "GAME OVER. YOU WON!" << endl;
        game.game_over = true; }
    }
}
/////////////////////////* END MAIN LOOP *////////////////////////////////////
return 0; }

/* 
 * init_game()
 * Purpose: Creates and initializes a Game struct
 * Return value: an initialized Game struct
 */
Game init_game() {
    Game game;
    game.game_over = false;
    game.num_mines = 0;
    game = populate_plain_board(game);
    game = populate_mines(game); 

    return game;
}

/*
 * populate_plain_board()
 * Purpose: initializes game.board by setting all values to UNSEEN
 * Parameter: a Game struct
 * Return value: an updated Game struct
 */
Game populate_plain_board(Game game) {
    for (int i = 0; i < DIMEN; i++) {
        for (int j = 0; j < DIMEN; j++) {
            game.board[i][j] = UNSEEN;
        }
    }
    return game;
}

/*
 * populate_mines()
 * Purpose: Initializes game.mines with locations of mines
 * Parameter: a Game struct
 * Return value: an updated Game struct
 */
Game populate_mines(Game game) {
    for (int i=0; i < DIMEN; i++) {
        for (int j=0; j < DIMEN; j++) {
            if (rand() % 100 <= PROB * 100) {
                game.mines[i][j] = MINE;
                game.num_mines++; }
            else {game.mines[i][j] = UNSEEN;} 
        }
    }
    return game;
}

/*
 * board_w_mines(Game &game)
 * Purpose: Merges unseen-mine board with seen-user board IN CASE of GAME OVER
 * Parameter: a Game struct
 * Return value: an updated Game struct
 */
void board_w_mines(Game &game) {
    for (int i=0; i<DIMEN; i++) {
        for (int j=0; j<DIMEN; j++) {
            if (game.board[i][j] == UNSEEN && game.mines[i][j] == MINE) {
                game.board[i][j] = MINE;
            }
        }
    }
}

/*
 * num_mines_in_vicinity()
 * Purpose: Find the number of mines surrounding a single square
 * Parameter: a Game struct, row #, col #
 * Return value: the number of mines surrounding a given square
 */
int num_mines_in_vicinity(Game game, int row, int col) {
    int start_row; int start_col; int end_row; int end_col;
    int num_of_mines = 0;
    
     // Forms "pseudo-subarray" of sqaures surrounding inputed square
    if (row - 1 >= 0) {start_row = row-1;} else {start_row=row;}
    if (row + 1 < DIMEN) {end_row = row+1;} else {end_row=row;}
    if (col - 1 >= 0) {start_col = col-1;} else {start_col=col;}
    if (col + 1 < DIMEN) {end_col = col+1;} else {end_col=col;}

     // Loop thru subarray, add 1 to num_of_mines if mine is found
    for (int i=start_row; i<=end_row; i++) {
        for (int j=start_col; j<=end_col; j++) {
            if (game.mines[i][j] == MINE) {
                num_of_mines++;
            }
        }
    }
    return num_of_mines;
}

/*
 * was_a_mine()
 * Purpose: Check whether user-selected square is a mine
 * Parameter: a Game struct, row #, col #
 * Return value: Returns a bool, true if user-selected square is a mine
 */
bool was_a_mine(Game game, int row, int col) {
    bool hit;

    if (game.mines[row][col] == MINE) 
        {hit = true;}
    else 
        {hit = false;}

    return hit;
}

/*
 * check_for_win()
 * Purpose: Check whether user has won the game
 * Parameter: a Game struct
 * Return value: Returns bool, true if user has uncovered all non-mine squares
 */
bool check_for_win(Game &game) {
    int missed_spot = 0;
    for (int i=0; i<DIMEN; i++) {
        for (int j=0; j<DIMEN; j++) {
            if (game.board[i][j] == UNSEEN) {
                if (game.mines[i][j] != MINE)
                    {missed_spot++;}
                }
            }
        }
    return bool(!missed_spot);
}

/*
 * check_input()
 * Purpose: Checks whether or not user input is valid
 * Parameter: a Game struct, row #, col #
 * Return value: Returns a bool [true=valid,false=else]
 */
bool check_input(Game game, int row, int col) {
    bool rows; bool cols;

    if (row >=0 && row < 9) {rows = true;} else {rows = false;}
    if (col >=0 && col < 9) {cols = true;} else {cols = false;}

    if (!cols || !rows) {
        cout << "Bad input. Row and col must be >= 0 and < 9. " << endl;
        return false; 
        }

    if (game.board[row][col] != UNSEEN) {
        cout << "Bad input. Select another square. " << endl;
        return false;
        }

    return true;  
}

/*
 * print_game_board()
 * Purpose: print the game board, with row and column numbers on the sides.
 * Parameter: a Game struct
 */
void print_game_board(Game game)
{
    cout << "  ";
    for (int i = 0; i < DIMEN; i++) {
        cout << i << " ";
    }
    cout << endl;

    for (int i = 0; i < DIMEN; i++) {
        cout << i << " ";
        for (int j = 0; j < DIMEN; j++) {
            cout << game.board[i][j] << " ";
        }
        cout << endl;
    }
}
/*  ** ** FOR DEBUGGING ONLY ** **
 * ///////////////////////////////
 * print_mines_board()
 * Purpose: print the mines board, with row and column numbers on the sides.
 * Parameter: a Game struct
 */
void print_mines_board(Game game)
{
    cout << "  ";
    for (int i = 0; i < DIMEN; i++) {
        cout << i << " ";
    }
    cout << endl;

    for (int i = 0; i < DIMEN; i++) {
        cout << i << " ";
        for (int j = 0; j < DIMEN; j++) {
            cout << game.mines[i][j] << " ";
        }
        cout << endl;
    }
}
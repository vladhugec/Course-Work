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
  // s = 'select', f ='flag', u = 'unflag'
const char markers[] = {'f', 'u', 's'};


/* Game struct definition */
struct Game {
    char board[DIMEN][DIMEN];
    char mines[DIMEN][DIMEN];
    bool game_over;
    int  num_mines;
};

struct Sub_Game {
    int start_row;
    int end_row;
    int start_col;
    int end_col;
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
bool check_input(Game &game, char marker, int row, int col);

 // Updates the user board with vicinity mines
void update_board(Game &game, int row, int col);
 
 // Clears further discovered zeroes
void check_for_zeros(Game &game);

 // Make a sub array
Sub_Game make_Sub_Game(int row, int col);


int main() {
    srand(RANDOMSEED); // Seed pseudo-random # generator with current time
    Game game = init_game();
    print_game_board(game);
//  print_mines_board(game); //DEBUGGING PURPOSES // comment out as needed 
    while (!game.game_over) { //start game loop
////////////////////* MAIN GAME LOOP STARTS BELOW *///////////////////////////
char marker; int row; int col;
cout << "Enter a marker, the row, and column of the square to uncover: ";
cin >> marker >> row >> col;

if (check_input(game, marker, row, col)) { // continue game if input is valid, then:       
// if the user didnt hit a mine, update board with # surrounding mines
    if (marker == markers[0]) {
        game.board[row][col] = markers[0];
        print_game_board(game);
    }
    else if (marker == markers[1]) {
        game.board[row][col] = UNSEEN;
        print_game_board(game);
    }
    else {
        if (not was_a_mine(game, row, col)) {
            update_board(game,row,col);
            check_for_zeros(game);
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
}
/////////////////////////* END MAIN GAME LOOP *///////////////////////////////
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
    int num_of_mines = 0;
    Sub_Game sub_game = make_Sub_Game(row,col);
    
     // Loop thru subarray, add 1 to num_of_mines if mine is found
    for (int i=sub_game.start_row; i<=sub_game.end_row; i++) {
        for (int j=sub_game.start_col; j<=sub_game.end_col; j++) {
            if (game.mines[i][j] == MINE) {
                num_of_mines++;
            }
        }
    }
    return num_of_mines;
}

Sub_Game make_Sub_Game(int row, int col) {
    Sub_Game sub_game;
    if (row - 1 >= 0) 
        {sub_game.start_row = row-1;} 
    else 
        {sub_game.start_row=row;}

    if (row + 1 < DIMEN) 
        {sub_game.end_row = row+1;} 
    else 
        {sub_game.end_row=row;}

    if (col - 1 >= 0) 
        {sub_game.start_col = col-1;} 
    else 
        {sub_game.start_col=col;}

    if (col + 1 < DIMEN) 
        {sub_game.end_col = col+1;} 
    else 
        {sub_game.end_col=col;}

    return sub_game;
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
 * Parameter: a Game struct, marker, row #, col #
 * Return value: Returns a bool [true=valid,false=else]
 */
bool check_input(Game &game, char marker, int row, int col) {
    bool rows; bool cols; bool marks;

    if (row >=0 && row < 9) {rows = true;} else {rows = false;}
    if (col >=0 && col < 9) {cols = true;} else {cols = false;}

    if (!cols || !rows) {
        cout << "Bad input. Row and col must be >= 0 and < 9. " << endl;
        return false; 
        }

    marks = false;
    for (int i=0; i<3; i++) {
        if (markers[i] == marker) {
            marks = true; } 
    }
    if (!marks) {
        cout << "Invalid marker input. Please try again." << endl;
        return false;
    }

    if (marker == markers[0] && game.board[row][col] == markers[0]) {
        cout << "Input already flagged. Select another square." << endl;
        marks = false; }
    else if (marker == markers[1] && game.board[row][col] == markers[1]) {
        cout << "Input already unflagged. Select another square." << endl;
        marks = false;}
    else if (marker == markers[2] && game.board[row][col] == markers[0]) {
        cout << "Input flagged. Select another square " << 
                                    "or unflag current input" << endl;
        marks = false;}

    if (!marks) 
        {return false;}

    return true;  
}

void update_board(Game &game, int row, int col) {
    char vic_mines = digits[num_mines_in_vicinity(game,row,col)];
    game.board[row][col] = vic_mines;

    if (vic_mines == digits[0]) {
        Sub_Game sub_game = make_Sub_Game(row, col);
        for (int i=sub_game.start_row; i<=sub_game.end_row; i++) {
            for (int j=sub_game.start_col; j<= sub_game.end_col; j++) {
                game.board[i][j] = digits[num_mines_in_vicinity(game,i,j)];
            }
        }
    }   
}

void check_for_zeros(Game &game) {
    for (int i=0; i < DIMEN; i++) {
        for (int j=0; j < DIMEN; j++) {
            if (game.board[i][j] == digits[0]) {
                update_board(game,i,j);
            }
        }
    }
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
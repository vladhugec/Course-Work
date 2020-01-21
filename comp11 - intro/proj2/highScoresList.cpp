#include <iostream>
#include <fstream>

#include "highScoresList.h"

using namespace std;
/* Constructor */
HighScoresList::HighScoresList() {
    head = NULL;
    tail = NULL;
    currPos = NULL;
    tempPos = NULL;
    load();
}
HighScoresList::~HighScoresList() {
    currPos = head;
    while (currPos != NULL) {
        tempPos = currPos;
        currPos = currPos -> nextScore;
        delete tempPos;
    }
}
/* 
 * load()
 * Reads the HIGH_SCORE_FILE and loads the contents of the file
 * into the linked list.
 * If the file does not exist, do nothing. 
 */
void HighScoresList::load() {
    ifstream inFile;
    inFile.open(HIGH_SCORE_FILE);
    if (not inFile) {
        return;
    }
    string user;
    int score;
    int rank = 1;
    while (inFile >> user) {
        if (user == SENTINEL) 
            break;

        inFile >> score;

        newScoreNode(score, user, rank);
        rank++;
    }

    inFile.close();
}

/* 
 * save()
 * Writes the names and scores to the HIGH_SCORE_FILE, followed by the 
 * sentinel.
 * This will overwrite what was originally in HIGH_SCORE_FILE.
 */
void HighScoresList::save() {
    ofstream outFile;
    outFile.open(HIGH_SCORE_FILE);
    
    if (head == NULL) {
        outFile << SENTINEL << endl;
    }
    else {
        currPos = head;
        while (currPos != NULL) {
            outFile << currPos -> name << " " << currPos -> score << endl;
            currPos = currPos -> nextScore;
        }
        if (currPos == NULL) {
            outFile << SENTINEL << endl;
        }
    }
    outFile.close();
}
/* 
 * newScoreNode()
 * this will create a new node with attributes passed in from Load()
 */
void HighScoresList::newScoreNode(int score, std::string name, int rank) {
    scoreNode newHighScore = new Score;
    newHighScore -> nextScore = NULL;
    newHighScore -> score = score;
    newHighScore -> name = name;
    newHighScore -> rank = rank;

    if (head != NULL) { // if list exists, set currPos to start of the list
        currPos = head; 
        while (currPos -> nextScore != NULL) { // while not at end of list
            currPos = currPos -> nextScore; //currPos moves to the next spot in list
        }
        currPos -> nextScore = newHighScore;
        tail = newHighScore;
    }
    else {  // if list doesnt exist, newHighScore is start of list
        head = newHighScore;
    }
}  
/* 
 * highestScore()
 * returns the highest score in linked list
 * since list is in order, highest is just the first one
 */
int HighScoresList::highestScore() {
    int highestScore = 0;

    if (head != NULL) {
        currPos = head;
        highestScore = currPos -> score;
    }

    return highestScore;
}
/* 
 * print()
 * this will print out the entire linked list
 */
void HighScoresList::print() {
    if (head != NULL) {
        currPos = head;
        while (currPos != NULL) {
            cout << currPos -> name << " " << currPos -> score << endl;
            currPos = currPos -> nextScore;
        }
    }
    else {
        cout << "No recorded scores!" << endl;
    }
}
/* 
 * printTop5()
 * this will print out the first 5 scores in linked list
 */
void HighScoresList::printTop5() {
    if (head != NULL) {
        currPos = head;
        int printCount = 0;
        while (currPos != NULL && printCount < 5) {
            cout << currPos -> name << " " << currPos -> score << endl;
            currPos = currPos -> nextScore;
            printCount++;
        }
    }
}
/* 
 * keepTop10()
 * this will delete #11 and on
 * only keeps first 10 in list
 */
void HighScoresList::keepTop10() {
    if (head != NULL) {
        currPos = head;
        while (currPos -> rank <= 10) {
            currPos = currPos -> nextScore;
            if (currPos -> rank == 10) {
                tempPos = currPos;
            }
        }

        currPos = tempPos -> nextScore;
        tempPos -> nextScore = NULL; // 10th one is now the end

        while (currPos != NULL) {
            tempPos = currPos;
            currPos = currPos -> nextScore;
            delete tempPos;
        }
    }
    else {
        cout << "Scoreboard is empty!" << endl;
    }
}
/* 
 * insert()
 * this takes a username and a score and inserts into 
 * the correct position in linked list
 */
void HighScoresList::insert(std::string user, int score) {
    int inserted = 0;
    scoreNode newScore = new Score;
    newScore -> name = user;
    newScore -> score = score;
    newScore -> rank = 0;
    newScore -> nextScore = NULL;

    if (head != NULL) { // if list exists
        currPos = head;
        tempPos = currPos -> nextScore;
        while (tempPos != NULL) {
            if (inserted == 0) {
                if (newScore -> score >= currPos -> score) {
                    head = newScore;
                    newScore -> nextScore = currPos;
                    newScore -> rank = 1;
                    inserted = 1;
                }
                if (newScore -> score < currPos -> score 
                && newScore -> score >= tempPos -> score) {
                    currPos -> nextScore = newScore;
                    newScore -> nextScore = tempPos;
                    newScore -> rank = tempPos -> rank;
                    inserted = 2;
                }
                if (tempPos -> nextScore == NULL 
                && newScore -> score < tempPos -> score) {
                    tempPos -> nextScore = newScore;
                    newScore -> rank = (tempPos -> rank) + 1;
                    inserted = 3; 
                }
            }
            if (inserted != 0) {
                if (inserted == 1) {
                    currPos -> rank++;
                    if (tempPos -> nextScore == NULL) {
                        tempPos -> rank++;
                    }
                }
                else if (inserted == 2) {
                    tempPos -> rank++;
                }
            }
            currPos = tempPos;
            tempPos = tempPos -> nextScore;
        }
        if (tempPos == NULL && inserted == 0) {
            currPos -> nextScore = newScore;
            newScore -> rank = 2;
        }
    }
    else {
        head = newScore;
        newScore -> rank = 1;
    }
}
/* 
 * clear()
 * this will clear all scores from highscoreslist
 */
void HighScoresList::clear() {
    if (head != NULL) {
        currPos = head;
        while (currPos != NULL) {
            tempPos = currPos;
            currPos = currPos -> nextScore;
            delete tempPos;
        }
        head = NULL;
    }
    else {
        cout << "High Scores already cleared." << endl;
    }
}
/* 
 * printUser() takes a username
 * and prints all scores associated with name
 */
void HighScoresList::printUser(std::string user) {
    bool found = false;

    if (head != NULL) {
        currPos = head;
        while (currPos != NULL) {
            if (currPos -> name == user) {
                cout << currPos -> score << endl;
                found = true;
            }
            currPos = currPos -> nextScore;
        }
    }
    if (!found) {
        cout << "User not recognized." << endl;
    }
}

//void HighScoresList::deleteUser(std::string user) { }


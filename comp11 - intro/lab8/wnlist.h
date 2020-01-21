#ifndef	WNLIST_H
#define	WNLIST_H
#include <iostream>
using namespace std;
//
// class description of a wordnumlist class
// purpose:  to store a list of words and numbers
// 

class WNlist
{
    private:
        struct Pair {
                string	word;
                int	num;
        };

        static const int MAX_PAIRS = 50000;
        
	Pair	list[MAX_PAIRS];	// the data
	int	space;			// array space
	int	used;			// number taken

    public:
	WNlist();
	bool	add(string, int);	// add an entry
	int	count();		// how many in list
	void	print();		// print entire list
	int	count_of_word(string);	// how many have this name
	int	count_of_num(int);	// how many have this num
	void	who_has_num(int);	// list words that have given num 
	int	get_value(string);	// look up num for name
	int	get_max();		// return max val in num field
};
#endif

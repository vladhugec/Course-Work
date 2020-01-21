# sentiment_2.py
# Written by: Vladimir Hugec, vhugec01
# 02/25/2018
# 
# Purpose: given lexicon file, seperates keyword data and
#          sentiment score values and stores them respectively
#          then prints each word, followed by a single whitespace character
#          and each sentiment score is followed by a newline.

from string import ascii_letters
import sys

# Purpose: open file, call storage function and pass results 
#         through printer function
def main():
    file_name = sys.argv[1]
    printer(storage(file_name))
     
# Purpsoe: splits and stores lexicon data into respective lists
#    num_of_keys: number of lines in the lexicon file
# Returns: dictionary key_val_dict
def storage(file_name):
    num_of_keys = sum(1 for line in open(file_name))
    key_val_dict = {}
    with open(file_name) as file:
        linecount = 0
        while linecount < num_of_keys:
            for line in file:
                key_val = line.split()
                key_val_dict[key_val[0]] = float(key_val[1])
                linecount += 1
    return key_val_dict

# Purpose: given dictionary, prints each word, followed by a single
#          whitespace character and each sentiment score is followed
#          by a newline
def printer(dictionary):
    for keyword in dictionary:
        print(keyword, dictionary[keyword])

# Purpose: create list of words from words found in text file
# arguments: a file name
def file_to_wordlist(f):
    word_list = []
    with open(f) as file:
        for line in file:
            word_list += line.split()
    return word_list

# arguments: list of words
# returns: same list but with all non-alphabet characters removed
def clean_wordlist(wl):
    cleaned_words = ''
    for word in wl:
        cleaned_words += clean_word(word) + ' '
    return cleaned_words

# arguments: individual words from list
# returns: same word but with all non-alphabet characters removed
def clean_word(mystr):
    return (''.join(char for char in mystr if char in ascii_letters)).lower()

if __name__ == '__main__':
    main()
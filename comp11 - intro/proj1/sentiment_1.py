# sentiment_1.py
# Written by: Vladimir Hugec, vhugec01
# 02/25/2018
# 
# Purpose: given file, return raw text of file with no punctuation on one line

from string import ascii_letters
import sys

# Purpose: open file, call clean word function, print cleaned_words
def main():
    file_name = sys.argv[1]
    cleaned_words = clean_wordlist(file_to_wordlist(file_name))
    print(cleaned_words)

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

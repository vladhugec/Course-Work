# similarity.py
# Written by: Vladimir Hugec, vhugec01
# 02/25/2018
# 
# Purpose: given a lexicon file, and two text files, program
#          determines the five most common words between the .txt files
#          that are also present in the lexicon.

from string import ascii_letters
import sys
from math import *

# Purpose: call functions and pass parameters
def main():
    cleaned_words_from_file1 = clean_wordlist(file_to_wordlist(sys.argv[1]))
    cleaned_words_from_file2 = clean_wordlist(file_to_wordlist(sys.argv[2]))
    lexicon_dict = lexiconstorage(sys.argv[3])

    print_top_5(cleaned_words_from_file1,cleaned_words_from_file2,lexicon_dict)


# Purpose: prints the top 5 words found in both the lexicon and both files
# Arguments: cwf1 [cleaned word file 1] list of cleaned words from file 1
#			cwf2 [cleaned word file 2] list of cleaned words from file 2
#			ld [lexicon dictionary]
def print_top_5(cwf1, cwf2, ld):
    words_in_com = compare(incommon_words(cwf1, cwf2), ld).split()
    ranked_com_words, num_of_word = rank_common_words(words_in_com, cwf1, cwf2)

    for i in range(5):
        repeat = True
        try:
            ranked_com_words[i]
        except IndexError:
            repeat = False
        if repeat == True:
            if i == 0:
                print('Most Common Words: ', end=' ')
            print(ranked_com_words[i]+'['+str(num_of_word[ranked_com_words[i]])
                                                                 +']', end=' ')
        else:
            print()
            return

# Purpose: rank words based on how many times they appear in both files
# Arguments: cwf1 [cleaned word file 1] list of cleaned words from file 1
#			cwf2 [cleaned word file 2] list of cleaned words from file 2
#			sim [words_in_common] word list with words found in both files
# Returns: sim[words_in_common] ranked in order of most appearences
def rank_common_words(sim, cwf1, cwf2):
    nums_each_word1 = num_each_word(cwf1)
    nums_each_word2 = num_each_word(cwf2)

    word_cumnum_times = {}
    for word in sim:
        word_cumnum_times[word] = nums_each_word1[word] + nums_each_word2[word]

    switch = True
    while switch:
        switch = False
        for i in range(len(sim)-1):
            word = sim[i]
            nextword = sim[i+1]
            if word_cumnum_times[word] < word_cumnum_times[nextword]:
                flip = sim[i]
                sim[i] = sim[i+1]
                sim[i+1] = flip
                switch = True

    return(sim, word_cumnum_times)

# Purpose: make a list of words in common between 2 files
# Arguments: cwf1 [cleaned word file 1] list of cleaned words from file 1
#			cwf2 [cleaned word file 2] list of cleaned words from file 2
# Returns: matchedwords  [word list with words found in both files]
def incommon_words(cwf1, cwf2):
    words_file1 = {}
    for word in cwf1:
        words_file1[word] = 0

    for word in cwf2:
        if word in words_file1:
            words_file1[word] += 1
        else:
            pass

    matchedwords = []
    for each in words_file1:
        if words_file1[each] > 0:
            matchedwords.append(each)

    return matchedwords

# Purpose: count the number of times each word appears in the file
# Arguments: w [the cleaned file word list from respective file]
# Returns: matchedwords  [word list with words found in both files]
def num_each_word(w):
    repeated_words = {}
    for i in range(len(w)):
        for word in w:
            if word == w[i]:
                if word in repeated_words:
                    repeated_words[word] += 1
                else:
                    repeated_words[word] = 1
    
    repeat_words = {}
    for word in repeated_words:
        repeat_words[word] = int(sqrt(repeated_words[word]))

    return repeat_words

# Purpose: compare words in a text file to words found in lexicon
# Arguments: w [list of all words found in text file]
#            ld [lexi_dict = dictionary of words and their sentiment scores]
# Returns: word_duplicates [string of words present in both lexicon and file]
def compare(w, ld):
    word_duplicates = ''
    for word in w:
        if word in ld:
            word_duplicates += word + ' '

    return word_duplicates
 
# Purpsoe: splits and stores lexicon data into respective lists
# arguments: name of lexicon file
#    num_of_keys: number of lines in the lexicon file
# Returns: the dictionary lexi_dict
def lexiconstorage(lex_name):
    num_of_keys = sum(1 for line in open(lex_name))
    lexi_dict = {}

    with open(lex_name) as file:
        linecount = 0
        while linecount < num_of_keys:
            for line in file:
                key_val = line.split()
                lexi_dict[key_val[0]] = float(key_val[1])
                linecount += 1

    return lexi_dict

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
    cleaned_words = []
    for word in wl:
        cleaned_words.append(clean_word(word))

    return cleaned_words

# arguments: individual words from list
# returns: same word but with all non-alphabet characters removed
def clean_word(mystr):
    return (''.join(char for char in mystr if char in ascii_letters)).lower()

if __name__ == '__main__':
    main()
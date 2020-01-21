# sentiment_5.py
# Written by: Vladimir Hugec, vhugec01
# 02/25/2018
# 
# Purpose: given lexicon file, seperates keyword data and
#          sentiment score values and stores them respectively
#          then calculates average of associated sentiment scores
#          of words present in both the original file and the lexicon

from string import ascii_letters
import sys

# Purpose: call functions and pass parameters and print final result
def main():
    words = clean_wordlist(file_to_wordlist(sys.argv[1]))
    lexi_dict = lexiconstorage(sys.argv[2])
    word_duplicates, duplicate_count = compare(words, lexi_dict)
    avg_sent_score = avg_sentiment(word_duplicates, duplicate_count, lexi_dict)
    print(avg_sent_score)

# Purpose: Sum and average associated sentiment scores of words present in
#          both the lexicon and the text file
# arguments: word_duplicates [string of words present in both lexicon and file]
#            duplicate_count [number of words in word_duplicates]
#            lexi_dict [dictionary of words with associated sentiment scores]
# retruns: The average sentiment score
def avg_sentiment(wd, dc, ld):
    if dc == 0:
        return("Sentiment Unknown")
    else:
        sum_sent_score = 0
        dupli_word_list = wd.split()
        for word in dupli_word_list:
            sum_sent_score += ld[word]
        avg_sent_score = sum_sent_score/dc
        avg_sent_score = round(avg_sent_score, 3)
        return avg_sent_score

# Purpose: compare words in a text file to words found in lexicon
# Arguments: w [all words found in text file]
#            ld [lexi_dict = dictionary of words and their sentiment scores]
# Returns: word_duplicates [string of words present in both lexicon and file]
#          duplicate_count [total number of words in word_duplicates]
def compare(w, ld):
    word_duplicates = ''
    duplicate_count = 0
    for word in w:
        if word in ld:
            word_duplicates += word + ' '
            duplicate_count += 1
        else:
            pass
    return(word_duplicates, duplicate_count)
    
     
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
                key_val_dict[key_val[0]] = float(key_val[1])
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
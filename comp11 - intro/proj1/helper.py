# helper.py
# Written by: Kalina Allen, kallen07
# 02/23/2018
# 
# Purpose: provide a helpful function for comp11 project 1

from string import ascii_letters

# arguments: 
#     mystr: string to clean
# returns: s but with all non-alphabet characters removed
def clean_word(mystr):
    return (''.join(char for char in mystr if char in ascii_letters)).lower()

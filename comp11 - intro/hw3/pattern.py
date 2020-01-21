#
# pattern.py 
#
# Purpose: Based on user input prints a pattern of a specified width
#          with specified characters
#
#       Modified by: Vladimir Hugec
#       Date: Feb 10, 2018

def main():
    """
    Prompts user for parameters and runs parameters through fucntion
    draw_pattern. Will print error messages if applicatble.
    """
    string1 = input('First? ')
    string2 = input('Second? ')
    width = input('Width? ')

       # Test if input is type float
    try:
        int(width)
    except ValueError:
        exit()

       # Test if input is negative
    if int(width) <= 0:
        exit()

       # Iterate through the list of the pattern produced by
       # draw pattern
    for num in draw_pattern(int(width)):
        print(string1 * num + string2 * (int(width) - num))


def draw_pattern(w):
    """
    draw_pattern will return a list with the values that will draw the pattern
    First String?  X
    Second String?  -
    Width?  5
    
    Output:
       A list: [5, 3, 1, 3, 5]

    Width?  6
    Output: [6, 4, 2, 0, 2, 4, 6]
    """
    p = [w]    			# create list 'p' gets value width
    if w % 2 == 0:			# is width divisible by 2?
        if w > 2:			
            p = p + draw_pattern(w-2)  # add recursion pattern(w-2) to list p 
            return p + p[0::-1]   # and return list p plus the reverse of list
        else:						# 'p'
            return [0]			# returns the mid-point of pattern
    else:
        if w > 2:
            p = p + draw_pattern(w-2)
            return p + p[0::-1]
        else:
            return [1]
        
main()

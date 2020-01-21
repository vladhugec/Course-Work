# 
# reverse.py
# 
# Purpose: given a text document, reverse.py reverses the text line by line
#          and outputs to a new text doc 'reversed_(yourdoc).txt'
#           
# For COMP11 Spring 2018
#
#       Modified by: Vladimir Hugec
#       Date: Feb 14, 2018


def main():
    """
    main(): Gets file-name from user, passes the name of the file into getline
            function.
            Propts user for next file, restart program if user wishes to
            reverse another file.
    """
    filename = input("Enter a filename: ")   # gets filename from user
    
    getline(filename)    # passes filename through getline()
    
        # prompt user for next file Y or N?
        # restart main() if answer is yes
    repeat = input("Enter another filename [Y/N]? ")
    if repeat == 'Y' or repeat == 'y':
        main()
    else:
        exit()
        
def getline(f):
    """
    Opens the file passed in by user from main(), for each line in the doc 
    getline() reads the line of the text, counts the number of characters 
    in the line, and calls the reverse() function

    getline passes to reverse(): 
       a single line from the doc
       the number of characters in the line
       user specified name of file
       empty string that temporarily stores reversed string
    """

    file = open(f, "r")    # open the user specified text document
    
    rev_line = ''    # create empty storage string

        # for each line in the doc, read the line, count number of characters 
        # call the reverse() function and pass all variables through
    for line in file.readlines():
        num_of_let = int(len(line))
        reverse(line, num_of_let, rev_line, f)
    
    file.close()    # close the user's file
        
def reverse(l, nol, rev, f):
    """
    reverse() reverses the line of text (in string 'l')
    and stores reversed text in variable: rev
    and calls the tofile() function when line reversal is complete

    [variable 'nol' is num_of_let / number of characters in the line]
    [vaiable 'f' is the user's file name]
    """

        # if the character is a newline, ignore it
    if l[nol-1] == '\n':
        pass
    else: 
        # else concatenate the (nol-1)th letter in the string 'l' with 'rev'
        # store as 'rev' 
        rev = rev + l[nol-1]
    
        # if the num_of_let is exactly 1, there are no letters left in line
        # to be reversed -> call tofile() func. and pass reversed string 
        # 'rev' and filename 'f' into tofile()
    if nol == 1:
        tofile(rev, f)
    else:
        # else call reverse() subtracting 1 from the number of letters left
        # to be reversed
        reverse(l, nol-1, rev, f)
        
def tofile(rev, f):
    """
    tofile() creates a file titled 'reversed_[yourdocument].txt' and 
    appends the reversed line into the new document
    """

    nf = True    # create variable, set to True ['nf' is 'newfile']

        # if the file already exists, 'nf' will remain true
    try:
        newfile = open("reversed_"+f,'a')
    except IOError:
        # else 'nf' gets False
        nf = False

        # if 'nf' is False
    if nf == False:
        # create a new file in create/write mode
        newfile = open("reversed_"+f,'w+')
    else:
        # else open existing file in append mode
        newfile = open("reversed_"+f,'a')

        # write the contents of 'rev', the reversed string, to the
        # new file. End line with '\n' and close the file
    newfile.write('{}\n'.format(rev))
    newfile.close()    
            
    
if __name__ == '__main__':
    main()

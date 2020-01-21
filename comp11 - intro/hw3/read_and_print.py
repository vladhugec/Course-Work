"""
read_and_print.py

Purpose: given a file, this program prints the contents of the file and then 
         the number of words in each line.

By: Julie Jiang
For COMP11 Spring 2018
"""


def print_each_line(filename):
    """Prints the file line by line"""

    # Here's the preferred way to open a file
    # The "with" keyword handles file opening and closing for you
    with open(filename, "r") as file:
        for line in file:
            print(line, end="") 


def print_num_words_in_each_line(filename):
    """Prints the number of words in each line of the file"""

    file = open(filename, "r")  # Here's another way to open a file
 
    for line in file:
        num_words = len(line.split())
        print(num_words)

    file.close()   # Remember to close the file once you're done!

def main():
    """
    Gets a file from the user, prints the file line by line,
    and then prints the number of words in each line.
    """

    filename = input("Enter a filename: ")
    print_each_line(filename)
    print_num_words_in_each_line(filename)

if __name__ == "__main__":
    main()


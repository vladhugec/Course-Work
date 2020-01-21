# iftest.py
#
#      purpose: report number of digits in an integer
#        shows: use of output, input, conditional statements
#         note: has bugs, needs work
#
#  modified by: Vladimir Hugec
#         date: 1/31/18
#

# Get number from user
number = (input("Give me a number, any number positive or negative: "))

# Convert to list
number = list(number)

# Check if user inputed a negative (-) sign
if number.count('-') == 1:
	intnum = len(number) - 1
	print("Your number is a negative number with", intnum, "digets")

# Check if user inputed a positive (+) sign and counts the number of items in
# the list
elif number.count('+') == 1:
	intnum = len(number) - 1
	print("Your number is a positive number with", intnum, "digets")

# If it has no signs, print number of items in list 
else:
	intnum = len(number)
	print("Your number is a positive number with", intnum, "digets")

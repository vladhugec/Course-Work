###
### add8.py
### Adds eight floating point numbers together
### 
### Author: Vladimir Hugec
### Date: 1/30/18


innum = input("Please input eight decimal numbers separated by a space: ")
print("Your numbers are: ", innum)

numsplit = innum.split(" ")
numint = list(map(float, numsplit))

print("Your sum is: ", sum(numint))



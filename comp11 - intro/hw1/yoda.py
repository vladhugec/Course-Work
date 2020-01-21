###
### yoda.py
### 
### Given an input of four words, returns the same four words in reverse order
###
### Author: Vladimir Hugec
### Date: 1/30/18

notyoda = input("Enter a four word sentence: ")
notyetyoda = notyoda.split(" ")
nearlyyoda = list(reversed(notyetyoda))

yoda = ''
for f in nearlyyoda:
	yoda = yoda + " " + f

print(yoda)

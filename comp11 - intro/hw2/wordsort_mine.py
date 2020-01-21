###
### wordsort[MINE].py
###
### Sorts any number of inputed words in alphabetical order
###
### Author: Vladimir Hugec
### Date: Feb 5, 2018
###

def main():
  words = input("Enter a list of words (seperated by a space): ")
  prealpha = split(words)
  alpha = cut(prealpha)
  AAlpha = str(alphab3t(alpha))
  print("Your words alphabetized are: " + AAlpha)

def split(a):
  for z in a:
    b = a.split()
  return(b)

def cut(c):
  count = int(len(c))
  splitupwords = []
  for eachword in c:
    splitupwords.append(eachword)
  return(splitupwords)
  
def alphab3t(a):
    splitupwords = []
    splitupwords = a
    switch = True
    while switch:
        switch = False
        for wrd in range(len(splitupwords)-1):
            if splitupwords[wrd] > splitupwords[wrd+1]:
                flipflop = splitupwords[wrd]
                splitupwords[wrd] = splitupwords[wrd+1]
                splitupwords[wrd+1] = flipflop
                switch = True
    return(splitupwords)
    
if __name__ == '__main__':
    main()

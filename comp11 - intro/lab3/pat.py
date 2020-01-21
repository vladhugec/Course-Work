# pat.py
# COMP 0011 Lab 3
# Prints a pattern, v1.0 is 7 lines of snow.
# Edited by:
# Date:


def specs1x(n):
    print("SNOW--SNOW--SNOW--SNOW--SNOW--")

def specs2x(s, t):
    t = int(t)
    if t <= 0:
        return
    print(t, s+s+s+s+s+s+s)
    specs2x(s, t-1)

def specs3x(s, r, t):
    t = int(t)
    if t <= 0:
        return
    if t % 2 == 0:
        print(t, s+r+s+r+s+r+s)
    else:
        print(t, r+s+r+s+r+s+r)
    specs3x(s, r, t-1)

def allspecs(s,r,t,w):
    t, w = int(t), int(w)
    if t <= 0:
        return
    count = 1
    while count <= w:
        if t % 2 == 0:
            if count == 1:
                print(t, s+r,end='')
            else:
                print(s+r,end='')
        else:
            if count == 1:
                print(t, r+s,end='')
            else:
                print(s+r, end='')

        if count == w:
            print()

        count += 1
    
    count = 1
    allspecs(s,r,t-1,w)
        
def main():
    weather = input()
    weather_parts = weather.split()
    part1 = True
    part2 = True
    part3 = True
    part4 = True

    try:
        weather_parts[0]
    except IndexError:
        part1 = False
    try:
        weather_parts[1]
    except IndexError:
        part2 = False
    try:
        weather_parts[2]
    except IndexError:
        part3 = False
    try:
        weather_parts[3]
    except IndexError:
        part4 = False

    if part4 == True:
        s, r, t, w = weather.split()
        allspecs(s, r, t, w)
    elif part3 == True and part4 == False:
        s, r, t = weather.split()
        specs3x(s, r, t)
    elif part2 == True and part4 == False and part3 == False:
        s, r = weather.split()
        specs2x(s, r)
    elif part1 == True and part4 == False and part3 == False and part2 == False:
        s = weather.split()
        specs1x(s)

if __name__ == "__main__":
    main()

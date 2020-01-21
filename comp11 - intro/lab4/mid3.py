'''
    mid3.py
    Used in Lab 4
    Created by: Margaret Gorguissian
    Modified by: 
    Date: 
'''

def mid3(a, b, c):
    if (a >= b and a <= c) or (a <= b and a >= c):
        return a
    if (b <= a and b >= c) or (b >= a and b <= c):
        return b
    else:
        return c

def main():
    a, b, c = input().split()
    
    mid = mid3(float(a), float(b), float(c))

    print(mid)

if __name__ == '__main__':
    main()

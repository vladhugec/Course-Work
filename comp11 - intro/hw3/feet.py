# 
# feet.py   
#                    
# Purpose: feet.py will convert user inputed numbers of feet and inches
#          to meters and centimeters 
#                        
# For COMP11 Spring 2018                                            
#                                                           
#       Modified by: Vladimir Hugec         
#       Date: Feb 10, 2018 

def main():
    """ 
    Purpose: Recieve Feet and Inches as input from user, run inputs though 
    the print_meters_centimeters function and print the result.
    Prompt the user to restart the program, if 'Y' then main() will run again
    """
    
    repeat = True            # set repeat to True
    while repeat == True:    # while repeat still equals True
        feet = float(input("Feet? "))        # prompt user for Feet? input
        inches = float(input("Inches? "))    # prompt user for Inches? input
        
           # pass user input through function, store in result variable
        print_meters_centimeters(feet, inches)
        
           # prompt user Y/N for continue
        repeat = input("Continue? [Y/N] ")
        if repeat == 'Y' or repeat == 'y':
            repeat = True        # if user says yes, repeat is True and program
                                 # will restart
        else:
            repeat = False        # otherwise repeat is False and program ends

def print_meters_centimeters(feet, inches):
    """
    print_meters_centimeters() takes in a user specified number of feet and
    inches and checks to see if the inputs are valid.
    Then converts to meters and centimeters, rounding to nearest whole number
    Prints the conversion followed respectively by the correct pluralization
    of 'meters' and 'centimeters' 
    """
      
       # check if user input of inches is between 0 and 12
    if 0 <= inches < 12:
        i = True               # if True, i gets value True
        
    elif inches < 0:           # else if inches are negative, i gets False
        i = False            
        
    else:
        inches = inches - 12     # else subtract 12 from inches
        feet += 1                # and add 1 too feet
        i = True
    
    if feet >= 0:                # if feet are negative, f gets False
        f = True
    else:
        f = False
    
    if i == True and f == True:        # if all are valid, then:
        inches += feet * 12       # multiply feet by 12 and add to total inches
        meters = inches * .0254    # convert inches to raw meters
    
        xtimes = 0                # create counter, set to 0
        while meters > 1:        # if there is more than 1 meter
            meters -= 1            # subtract 1 from meters
            xtimes += 1            # add 1 to xtimes

        centi = round(meters * 100)  # (#)of centi is (#)of meters left * 100
        meters = xtimes             # (#)of meters is the number of times the 
                                    # while loop subtracted 1 from raw meters
    
            # these are conditions for printing the correct pluralization of 
            # 'meters' and 'centimeters'
        if meters > 1:
            if centi > 1:
                print(meters, "meters,", centi, "centimeters")
            else:
                print(meters, "meters,", centi, "centimeter")
        elif meters == 1:
            if centi > 1:
                print(meters, "meter,", centi, "centimeters")
            else:
                print(meters, "meter,", centi, "centimeter")
        else:
            if centi > 1 or centi == 0:
                print("0 meters,", centi, "centimeters")
            else:
                print("0 meters,", centi, "centimenter")
    
        # these conditions are invalid input error conditions
    elif i == False or f == False:
        print("Error: input should be nonnegative")

if __name__ == '__main__':
    main()

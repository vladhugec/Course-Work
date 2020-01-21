###
### chkdate.py
###
### given an input of three numbers representing a date (day, month, year) return whether or nat the 
### date is valid
###
### Author: Vladimir Hugec 
### Date: Feb 6, 2018
###

def main():
    
    month, day, year = input().split()
    valid = datechk(month, day, year)
    print(valid)

##
## define datechk() function
##   checks for invalid/nonexistant dates
##

def datechk(month, day, year):
    
         # convert str to int
    month, day, year = int(month), int(day), float(year)
    
         # define month values
    jan, feb, mar, apr, may, jun = 1, 2, 3, 4, 5, 6
    jul, aug, sep, oct, nov, dec = 7, 8, 9, 10, 11, 12
        
         # check if number of days is valid
    if month == feb:
        leap = (year/4) - (year//4) # check if year is divisible by 4
        leapomit = (year/100)-(year//100) # check if year is divisible by 100
        leapomit2 = (year/400)-(year//400) # check if year is divisible by 400
        
                # checks for a leap year
        if leap == 0: # check if year is divisible by 4
        # but if the year is divisible by 100 and not by 400 then there is no leap year
            if leapomit == 0 and leapomit2 != 0:
                numofdays = 28 # define number of days in february
            else:
                numofdays = 29 # define number of days in february
        else:
            numofdays = 28 # define number of days in february
            
               # checks if number of days is valid in february
        if day <= numofdays: # if the input days is less than or equal to
            day = True # the variable numofdays then 'day' is valid
        else:
            day = False # otherwise it is invalid
            
               # checks for validity variable 'day' in months with 31 days
    elif month == jan or mar or may or jul or oct or dec:
        numofdays = 31
        if day <= numofdays: # if the input days is less than or equal to
            day = True # the variable numofdays then 'day' is valid
        else:
            day = False # otherwise it is invalid
            
                # checks for validity variable 'day' in months with 30 days
    else:
        numofdays = 30
        if day <= numofdays: # if the input days is less than or equal to
            day = True # the variable numofdays then 'day' is valid
        else:
            day = False # otherwise it is invalid
    
              # check if year is valid
    if 1 <= year <= 9999: # if the input days is less than or equal to
        year = True # the variable numofdays then 'day' is valid
    else:
        year = False # otherwise it is invalid
        
              # check if month is valid
    if 1 <= month <= 12: # if the input days is less than or equal to
        month = True # the variable numofdays then 'day' is valid
    else:
        month = False # otherwise it is invalid
            
       # checks whether or not any of the values were found to be invalid
    if month and day and year: # if all three variables are valid then
        return("Y") # return "Y"
    else:
        return("N") # otherwise return "N"
    
if __name__ == '__main__':
    main()

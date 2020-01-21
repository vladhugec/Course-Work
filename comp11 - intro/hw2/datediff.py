###
### datediff.py
###
### given an input of two dates, return the number of days in between
###
### Author: Vladimir 
### Hugec Date: Feb 6, 2018 
### Hw2 -- Extra Practice
###

def main():
    
       # intro and instructions
    print("~~ Number of days between two dates calculator ~~")
    print("    ~~ Please enter dates with no spaces ~~")
    print('\n')
    
       # prompt user for start and end dates
    start_date = input("Enter start date [mm/dd/yyyy]: ")
    end_date = input("Enter end date [mm/dd/yyyy]: ")
    
       # run the user input through datediff function
    start_end_diff = datediff(start_date, end_date)
    
       # output the number of days between dates
    print('\n')
    print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    print("There are: ", start_end_diff, " days between your two dates")
    print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    print('\n')
    
    restart = input("Would you like to enter new dates [y or n]? : ")
    while restart != 'y' and restart != 'n' and restart != 'Y' and restart != 'N':
        restart = input("Please enter a 'Y' for yes, an 'N' for no")
    if restart == 'y' or restart == 'Y':
        print('\n')
        main()
    else:
        return
    
       # define the function datechk
def datechk(month, day, year):
    
           # convert str to int
    month, day, year = int(month), int(day), float(year)
    
           # define month values
    jan, feb, mar, apr, may, jun = 1, 2, 3, 4, 5, 6
    jul, aug, sep, oct, nov, dec = 7, 8, 9, 10, 11, 12
        
           # check if number of days is valid
    if month == feb:
        leap = (year/4) - (year//4) # check if year is divisible by 4
        leapomit = (year/100)-(year//100) # check if is divisible by 100
        leapomit2 = (year/400)-(year//400) # check if is divisible by 400
        
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
        return(True) # return True
    else:
        return(False) # otherwise return "False"
        
def monthstodays(x, y, z, q):
    x, y, z, q = int(x), int(y), int(z), int(q)
    
    month_diff = int(abs(x - y)) # check for total difference in months
    days_from_months_total = 0
    count = 0
    if x > y:
        x1 = y
        y = x
        x = x1
    while count <= month_diff:
        if x <= y:
            if x == 1 or x == 3 or x == 5 or x == 7 or x == 10 or x == 12:
                if count == 0:
                    num_of_days_x = 31 - z
                else:
                    num_of_days_x = 31
                    
                if count == month_diff:
                    num_of_days_x = q
                else:
                    num_of_days_x = 31
            elif x == 4 or x == 6 or x == 8 or x == 9 or x == 11:
                if count == 0:
                    num_of_days_x = 30 - z
                else:
                    num_of_days_x = 30
                    
                if count == month_diff:
                    num_of_days_x = q
                else:
                    num_of_days_x = 30
                    
            elif x == 2:
                if count == 0:
                    num_of_days_x = 28 - z
                else:
                    num_of_days_x = 28
                    
                if count == month_diff:
                    num_of_days_x = q
                else:
                    num_of_days_x = 28
                    
            x += 1
            days_from_months_total += num_of_days_x
            
        count += 1
        
    return(days_from_months_total)
    
def datediff(a, b):
       # split input string 'a' into start_[month, day, year]
    start_month, start_day, start_year = a.split("/")
    end_month, end_day, end_year = b.split("/")
    
       # conver string to int
    start_month, start_day = int(start_month), int(start_day)
    end_month, end_day = int(end_month), int(end_day)
    start_year, end_year = int(start_year), int(end_year)
        
       # check if both user inputed start and end dates are valid
    start_date_validity = datechk(start_month, start_day, start_year)
    end_date_validity = datechk(end_month, end_day, end_year)
    
       # if either date is invalid, return to main
    if start_date_validity == False or end_date_validity == False:
        print("~~ Your dates are invalid, please check your dates again ~~")
        return main()
    
       # define numerical difference between end and start dates
    year_diff = abs(end_year-start_year)
    month_diff = abs(end_month-start_month)
    day_diff = abs(end_day-start_day)
    
    
    leap_day_count = 0
    if year_diff > 0:
        count = 0
        
        while count <= year_diff:
            
               # check if is divisible by 4
            leap = ((start_year+count)/4) - ((start_year+count)//4)
               # check if year is divisible by 100
            leapomit = ((start_year+count)/100) - ((start_year+count)//100)
               # check if is divisible by 400
            leapomit2 = ((start_year+count)/400) - ((start_year+count)//400)
        
                # checks for a leap year
            if leap == 0: # check if year is divisible by 4
            # but if the year is divisible by 100 and not by 400 then there is no leap year
                if leapomit == 0 and leapomit2 != 0:
                    leap_day_count = leap_day_count
                else:
                    leap_day_count += 1 # define number of days in february
            count += 1
    
    leap_day_count = str(leap_day_count)
        
    year_leap_days = year_diff * 365 + int(leap_day_count)
        
    if end_month == start_month:
        days_total = year_leap_days + day_diff
        return(days_total)
            
    elif end_month > start_month:
        days_total = year_leap_days + monthstodays(start_month, end_month, start_day, end_day) - 1
        return(days_total)
            
    elif end_month < start_month:
        days_total = year_leap_days - monthstodays(start_month, end_month, start_day, end_day) - 1
        return(days_total)
    
if __name__ == '__main__':
    main()

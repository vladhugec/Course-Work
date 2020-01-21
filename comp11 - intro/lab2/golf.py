#
# golf.py
# purpose: practice using booleans and constants
# modified by:
# on:
#

#
# days of the week
#
SUN , MON, TUE, WED, THR, FRI, SAT = 0 , 1, 2, 3, 4, 5, 6

#
# constants for pricing rules
#
BASE_PRICE            = 9.0     # in dollars
HIGH_TEMP_CUTOFF      = 80      # degrees F
LOW_TEMP_CUTOFF       = 65      # degrees F
PER_DEGREE_PENALTY    = 0.10    # in dollars
EVENING_START         = 17      # hour of the day using 24-hour time
WEEKEND_EVE_SURCHARGE = 1.00    # in dollars
RAIN_PENALTY          = 2.00    # in dollars
WIND_GRANULARITY      = 15      # in miles per hour
WIND_PENALTY          = 1.50    # in dollars

#
# Get data from user
#
tempF = int(input("Temperature (fahrenheit)? "))

dayOfWeekPrompt = "Day of week (0 = Sun, 1 = Mon, ..., 6 = Sat)? " 
dayOfWeek = int(input(dayOfWeekPrompt))
hour    = int(input("Hour of day (0..23)? "))
rainP   = int(input("Is it raining (0 = no, 1 = yes)? "))
windMPH = int(input("Wind speed (MPH)? "))

# compute price based on these values

price = BASE_PRICE

if tempF <= HIGH_TEMP_CUTOFF and tempF >= LOW_TEMP_CUTOFF:
	price = BASE_PRICE

# if the temp is at a normal range, from (65 to 80 degrees), leave the price as
# it is

elif tempF > HIGH_TEMP_CUTOFF:
	price = BASE_PRICE - (tempF - HIGH_TEMP_CUTOFF) * PER_DEGREE_PENALTY

## if the temp is higher than normal (>80 degrees), subtract the per degree
## penalty from the price for every degree over 80 degrees

elif tempF < LOW_TEMP_CUTOFF:
	price = BASE_PRICE - (LOW_TEMP_CUTOFF - tempF)*PER_DEGREE_PENALTY
## if the temp is below the low temp cutoff (65 degrees), subtract the per
## degree penalty from the price for every degree under 65 degrees


if dayOfWeek == SUN or dayOfWeek == SAT or hour >= EVENING_START:
	price = price + WEEKEND_EVE_SURCHARGE
## if it is a weekend (saturday or sunday) or if the time is in the evening
## (past 5:00 PM) add the weekend/evening surcharge to the price


if rainP:
	price = price - RAIN_PENALTY
## if it is raining outside, subtract the price by the rain penalty
 
# report price to user
print("Price:", price)

###
### change.py
###
### Given amount of change in cents, program calculates correct values of
### quarters, dimes, nickels and pennies
###
### Author: Vladimir Hugec
### Date: 1/30/18

raw_change = int(input("Enter amount in cents: "))

if raw_change // 25 < 1:
	quart_change = 0
else:
	quart_change = raw_change // 25

sumlessquart = raw_change - quart_change * 25

if sumlessquart // 10 < 1:
	dime_change = 0
else:	
	dime_change = sumlessquart // 10

sumlessdimequart = sumlessquart - dime_change * 10

if sumlessdimequart // 5 < 1:
	nickel_change = 0
elif sumlessdimequart // 5 >= 1:
	nickel_change = sumlessdimequart // 5

sumlessdimequartnic = sumlessdimequart - nickel_change * 5

if sumlessdimequartnic // 1 < 1:
	penny_change = 0
else:
	penny_change = sumlessdimequartnic // 1

print(quart_change,"Quarters,", dime_change,"Dimes,", nickel_change, "Nickels,", penny_change, "Pennies")

import csv

with open("JAR-16-20296A.csv", 'r') as csvfile:
    #get and save all ip addresses
    ipAddresses = list(col[0] for col in csv.reader(csvfile))

    #this var holds ip addresses with '[.]' replaced with '.'
    ipParsed = []

    #replace all instances of '[.]' with '.'
    for ip in ipAddresses:
        stringip = str(ip)
        stringip = stringip.replace('[.]', '.')
        ipParsed += [stringip]

    #remove all empty elements
    ipParsed = filter(None, ipParsed)

    #print all ip adresses to terminal
    for each in ipParsed:
        #remove non ip adresss values
        if each[0].isdigit():
            #further remove non ip adress values
            if '.' in each:
                print(each)
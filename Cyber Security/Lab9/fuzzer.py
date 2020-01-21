# fuzzer.py
# Vladimir Hugec
# Comp116 - Cyber Security

import requests
import os

def fuzz(url, script):
    #create payload
    #payload = {"fullname": script, "beverage": "water"}
    payload = {"title": script, "post": "water"}
    #send payload & recieve response
    response = requests.post(url, data=payload)
    #if payload is echoed in response -> site is vulnerable
    vulnerable = script in response.text
    if vulnerable:
        print("The page at", url, "has a reflected Cross-Site Scripting (XSS) vulnerability with the payload --> ", script)

path = input("Enter location of Fuzz Lists folder on workstation: ")
#url = "http://www.cs.tufts.edu/comp/20/hackme.php"
url = "https://35.192.152.44/board.php"

for each in os.listdir(path):
    f = path + "/" + each
    with open (f, "r") as fuzz_file:
        for line in fuzz_file:
           fuzz(url, line)
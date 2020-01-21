#!/usr/bin/python3

from scapy.all import *
import pcapy
import argparse
import re
import base64

incident_counter = 0
ftp_user = "None"
ftp_pass = "None"

def alarm(incident, src_ip, port, payload):
  global incident_counter
  incident_counter += 1

  if incident == "HTTP" or incident == "FTP":
    print "ALERT #", incident_counter, ": Usernames and passwords sent in-the-clear (",port,") (",payload,")!"
  else:
    print "ALERT #", incident_counter, ": ", incident, "scan is detected from ", src_ip, "(",port,")!"

def packetcallback(packet):
  try:
    #alarm for HTTP packets sent in the clear
    if packet[TCP].dport == 80:
      pack = raw(packet)
      auth = re.search("Basic.[a-zA-z0-9]*|=\s", pack)

      if auth != None:
        x,y = auth.group().split()

        #the following line was a solution I found to add padding to a base64 string by Saurav Gupta
        #https://stackoverflow.com/questions/38683439/how-to-decode-base64-in-python3
        y += "=" * ((4 - len(y) % 4) % 4) #add equal sign padding for decoding

        decoded = base64.b64decode(y)
        user,pswd = decoded.split(':')
        payload = "username:"+user+", "+"password:"+pswd
        alarm("HTTP", packet[IP].src, "HTTP", payload)

    #alarm for FTP packets sent in the clear
    elif packet[TCP].dport == 21:
      global ftp_pass
      global ftp_user

      pack = raw(packet)
      user = re.search("USER.[a-zA-z0-9]*", pack)
      pss = re.search("PASS.[a-zA-z0-9]*", pack)

      if pss != None:
        ftp_pass = pss.group()

      if user != None:
        ftp_user = user.group()

      if ftp_user != "None" and ftp_pass != "None":
        usr,name = ftp_user.split(" ")
        pss,wd = ftp_pass.split(" ")
        payload = "username:"+name+", "+"password:"+wd
        alarm("FTP", packet[IP].src, "FTP", payload)

    #alarm for alleged FIN scan
    elif (packet[TCP].flags.F and not packet[TCP].flags.P and not packet[TCP].flags.S 
          and not packet[TCP].flags.R and not packet[TCP].flags.A and not packet[TCP].flags.U 
          and not packet[TCP].flags.E and not packet[TCP].flags.C):
      alarm("FIN", packet[IP].src, packet[TCP].dport, "NULL")

    #alarm for alleged NULL scan
    elif not packet[TCP].flags:
      alarm("NULL", packet[IP].src, packet[TCP].dport, "NULL")

    #alarm for alleged XMAS scan
    elif packet[TCP].flags.F and packet[TCP].flags.P and packet[TCP].flags.U:
      alarm("Xmas", packet[IP].src, packet[TCP].dport, "NULL")
  except:
    pass


parser = argparse.ArgumentParser(description='A network sniffer that identifies basic vulnerabilities')
parser.add_argument('-i', dest='interface', help='Network interface to sniff on', default='eth0')
parser.add_argument('-r', dest='pcapfile', help='A PCAP file to read')
args = parser.parse_args()
if args.pcapfile:
  try:
    print("Reading PCAP file %(filename)s..." % {"filename" : args.pcapfile})
    sniff(offline=args.pcapfile, prn=packetcallback)    
  except:
    print("Sorry, something went wrong reading PCAP file %(filename)s!" % {"filename" : args.pcapfile})
else:
  print("Sniffing on %(interface)s... " % {"interface" : args.interface})
  try:
    sniff(iface=args.interface, prn=packetcallback)
  except pcapy.PcapError:
    print("Sorry, error opening network interface %(interface)s. It does not exist." % {"interface" : args.interface})
  except:
    print("Sorry, can\'t read network traffic. Are you root?")
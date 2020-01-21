Name: Vladimir Hugec

- Briefly explain what this tool is (that is, an overview)
	Alarm.py is a detection script for stealthy port scans namely FIN, NULL, and XMAS. And also detects cleartext usernames and passwords sent over HTTP and FTP

- Briefly explain how the tool works.
	The tool scans packets (either from a .pcap file or those intercepted from ethernet or wireless connections) and based on the contents of those packets, the tool will trigger an alarm if it believes a port scan is being conducted or if it detects any cleartext username/password pairs.

-Identify what aspects of the work have been correctly implemented and what have not.
	To my knowledge all aspects have been implemented. Although I believe my program has a few false positives when it comes to FIN scans but I couldn't figure out how to solve that.

- Identify anyone with whom you have collaborated or discussed the assignment.
	I did not collaborate with anyone, however for decoding the base64 string in the HTTP Authorization header, it wasn't decoding more than half of the strings and after a little bit of searching on the web I found that it was because during the regex extraction I dropped the '=' signs leading to the padding being off on the string, a one line solution was given on StackOverflow that I implemented in my code.

-Say approximately how many hours you have spent completing the assignment.
	~6 hours


For this lab, you must also address the following questions:

- Are the heuristics used in this assignment to determine incidents "even that good"?
	I believe they could be useful, although my methods may signal some false positives, they can still give you an insight into what kind of connections are incoming into your network/computer. Just how useful these insights are I really am not sure.

-If you have spare time in the future, what would you add to the program or do differently with regards to detecting incidents?
	I think it would be interesting if the program tried to scan the source IP of the incident back. As in, if the program detects a NULL scan, it would also can the source of the NULL scan, perhaps deterring any potential attackers.

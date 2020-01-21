README.md
fuzzer.py

Name: Vladimir Hugec

Uses python3, requests and os libraries

fuzzer.py will inject various browser executable codes in an HTTP response and will check if that code is then echoed back to the client.
If the code is found within the response, then the site is vulnerable and the fuzzer program will print that code

This is the basic version of the fuzzer assignment. The fuzzer will only test the hackme.php site but will use all the fuzzing lists that Daniel Miessler provided and prompt the user for the location of that folder.

No colaboration was done

Hours: ~3.5-4
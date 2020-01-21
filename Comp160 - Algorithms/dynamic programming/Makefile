#############################################################################
#
#    Makefile
#
#############################################################################


CXX      = g++
CXXFLAGS = -std=c++11 -g -O2 -Wall -Wextra

interpret: main.o interpret.o
	$(CXX) $(CXXFLAGS) -o interpret main.o interpret.o

main.o: main.cpp interpret.h
	$(CXX) $(CXXFLAGS) -c main.cpp

interpret.o: interpret.cpp interpret.h
	$(CXX) $(CXXFLAGS) -c interpret.cpp

clean:
	rm -f interpret *.o *~
#
# Makefile for the UM Test lab
# 
CC = gcc

IFLAGS  = -O2 -I/lcii40-O2 -I/comp/40/include -I/usr/sup/cii40/include/cii
CFLAGS  = -g -std=gnu99 -Wall -Wextra -Werror -pedantic $(IFLAGS)
LDFLAGS = -g -L/comp/40/lib64 -L/usr/sup/cii40/lib64
LDLIBS  = -l40locality -lcii40 -lm

EXECS   = writetests um

all: $(EXECS)

writetests: umlabwrite.o umlab.o bitpack.o
	$(CC) $(LDFLAGS) $^ -o $@ $(LDLIBS)

um: UM_segData.o UM_ALU.o UM_main.o
	$(CC) $(LDFLAGS) $^ -o $@ $(LDLIBS)


# To get *any* .o file, compile its .c file with the following rule.
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(EXECS)  *.o


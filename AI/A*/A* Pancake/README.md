A* IMPLEMENTATION FOR PANCAKE PROBLEM
    By: Vladimir Hugec

HOW TO COMPILE:
    clang++ -std=c++11 -stdlib=libc++ main.cpp astar.cpp pancake.cpp


Command-Line Options:
    '-v' -> VERBOSE MODE
                Outputs each visited Node
    '-t' -> BATCH TESTING MODE
                Requires a file (provided: batch.txt) with different tests
    
    Examples: ./a.out -v
              ./a.out -t < FILE.txt
              ./a.out -v -t < FILE.txt


HOW TO RUN:
    Option 1: Input Pancake stack directly
        run -> ./a.out
              -> ./a.out -v

    Option 2: Pipe from file (the provided batch.txt will run every permutation of [1,2,3,4,5] for testing purposes)
        run -> ./a.out -t < batch.txt
            -> ./a.out -v -t < batch.txt

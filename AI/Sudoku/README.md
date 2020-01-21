SUDOKU PUZZLE SOLVER
    by Vladimir Hugec


Comp-131 A.I.
Prof. Fabrizio Santini


HOW TO COMPILE:
    clang++ -std=c++11 -stdlib=libc++  main.cpp csp.cpp ac3.cpp



HOW TO RUN:
    Option 1: 
              I submitted 3 sudoku puzzles in .txt form along with the code submission
              your two puzzles: easypuzzle.txt and evilpuzzle.txt
              and the 1 example puzzle from the book: puzzle1.txt

              run: ./a.out < PUZZLE_NAME.txt

    Option 2:
              run: ./a.out

              Enter the puzzle reading an entire row at a time with no spaces and lowercase
              x's for blank squares and press enter then enter the next row:
                    e.g. xxx2xx56x



NOTES:
      I implemented this solver using the Arc Consistency strategy found in the course textbook.

      The solver is capable of solving both the example puzzle found in the textbook as well as
      the easypuzzle you provided in the assignment spec.

      However, unfortunatly arc consistency was not enough to solve the evilpuzzle provided and the
      extra credit, conflict resolution backjumping was not implemented.



FURTHER NOTES / P.S.
      Thank you for a great course, I learned a lot and while in the end I chose to complete the Sudoku
      solver and the A* assignment instead of the neural network, this was simply because of my packed schedule
      working in the second half of the summer. Im currently building the neural network assignment you created
      in python (to learn python :]) but I knew I wasnt going to finish it in time so I completed the A* assignment
      instead.
        -Vladimir Hugec 

              
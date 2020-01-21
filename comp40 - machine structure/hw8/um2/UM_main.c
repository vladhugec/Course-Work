#include "UM_ALU.h"
#include "UM_segData.h"

#include <stdlib.h>
#include <stdio.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#define WORD uint32_t

int main(int argc, char const *argv[])
{
        /* VALIDATE INPUT */
        if (argc != 2) {
                perror("Error - invalid input");
                return EXIT_FAILURE;
        }
        /* check elements of file if file passes input check */
        struct stat s;
        if (stat(argv[1], &s) == -1) {
                perror("Error - invalid file");
                return EXIT_FAILURE;
        }
        else if ((s.st_size % 4) != 0) {
                perror("Error - incomplete file");
                return EXIT_FAILURE;
        }

        FILE *program = fopen(argv[1], "rb");
        UM_data data = make_UM_data();
        initialize_UM_data(program, data);

        int go = 1;
        /*int go:
                == 0 --> error occured, exit EXIT_FAILURE
                == 1 --> all good, keep going
                == 2 --> halt instruction
        */
        do{
                WORD instruction = UM_data_get_zeroSeg_instruction(data, UM_data_get_pCounter(data));
                go = UM_ALU_Execute(instruction, data);
        }
        while (go == 1);
        
        if (go == 0) {
                UM_data_free(&data);
                fclose(program);
                return EXIT_FAILURE;
        }

        UM_data_free(&data);
        fclose(program);
    
        return EXIT_SUCCESS;
}

#undef WORD

/*
        UM_ALU.h

        HEADER FILE FOR:
          Universal Machine Arithmatic Logic Unit

          Performs computations on registers based on 32 bit instructions

        By: Vladimir Hugec and Mason Pollack
        11/15/18
        Comp40-HW6
*/
#ifndef UM_ALU_H
#define UM_ALU_H

#include <inttypes.h>
#include <stdio.h>
#include "UM_segData.h"
#include <assert.h>
#include <stdbool.h>

typedef uint32_t Um_instruction;

typedef enum Um_opcode {
        CMOV = 0, SLOAD, SSTORE, ADD, MUL, DIV,
        NAND, HALT, ACTIVATE, INACTIVATE, OUT, IN, LOADP, LV
} Um_opcode;

typedef enum Um_register { r0 = 0, r1, r2, r3, r4, r5, r6, r7 } Um_register;

extern int UM_ALU_Execute(Um_instruction word, UM_data data);

#endif
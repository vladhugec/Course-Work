/*
 * umlab.c
 * 
 * The functions defined in this lab should be linked against umlabwrite.c
 * to produce a unit test writing program. Any additional functions and unit
 * tests written for the lab go here.
 */

#include <stdint.h>
#include <stdio.h>

#include <assert.h>
#include <seq.h>
#include "bitpack.h"


typedef uint32_t Um_instruction;
typedef enum Um_opcode {
        CMOV = 0, SLOAD, SSTORE, ADD, MUL, DIV,
        NAND, HALT, ACTIVATE, INACTIVATE, OUT, IN, LOADP, LV
} Um_opcode;


/* Functions that return the two instruction types */

Um_instruction three_register(Um_opcode op, int ra, int rb, int rc) {
        uint64_t word = 0;
        word = Bitpack_newu(word, 3, 0, rc);
        word = Bitpack_newu(word, 3, 3, rb);
        word = Bitpack_newu(word, 3, 6, ra);
        word = Bitpack_newu(word, 4, 28, op);

        return word;
}

extern void writeBEndian(void *word, FILE *output) {
        for (int i = 0; i < 4; i++) {
                int byte = Bitpack_getu((uintptr_t)word, 8, 8*(3-i));
                putc(byte, output);
        }
}

Um_instruction loadval(unsigned ra, unsigned val) {
        uint64_t word = 0;
        word = Bitpack_newu(word, 25, 0, val);
        word = Bitpack_newu(word, 3, 25, ra);
        word = Bitpack_newu(word, 4, 28, LV);

        return word;
}


/* Wrapper functions for each of the instructions */

typedef enum Um_register { r0 = 0, r1, r2, r3, r4, r5, r6, r7 } Um_register;

static inline Um_instruction cMov(Um_register a, Um_register b, Um_register c){
        return three_register(CMOV, a, b, c);
}

static inline Um_instruction SLoad(Um_register a, 
                                Um_register b, Um_register c){
        return three_register(SLOAD, a, b, c);
}

static inline Um_instruction SStore(Um_register a, 
                                Um_register b, Um_register c) {
        return three_register(SSTORE, a, b, c);
}

static inline Um_instruction add(Um_register a, 
                                Um_register b, Um_register c) 
{
        return three_register(ADD, a, b, c);
}

static inline Um_instruction multiply(Um_register a, 
                                Um_register b, Um_register c) {
        return three_register(MUL, a, b, c);
}

static inline Um_instruction divide(Um_register a, 
                                Um_register b, Um_register c) {
        return three_register(DIV, a, b, c);
}

static inline Um_instruction nand(Um_register a, 
                                Um_register b, Um_register c) {
        return three_register(NAND, a, b, c);
}

static inline Um_instruction halt(void) 
{
        return three_register(HALT, 0, 0, 0);
}

static inline Um_instruction activate(Um_register b, Um_register c) {
        return three_register(ACTIVATE, 0, b, c);
}

static inline Um_instruction inactivate(Um_register c) {
        return three_register(INACTIVATE, 0, 0, c);
}

static inline Um_instruction output(Um_register c) {
        return three_register(OUT, 0, 0, c);
}

static inline Um_instruction input(Um_register c) {
        return three_register(IN, 0, 0, c);
}

static inline Um_instruction loadP(Um_register b, Um_register c) {
        return three_register(LOADP, 0, b, c);
}

/* Functions for working with streams */

static inline void emit(Seq_T stream, Um_instruction inst)
{
        assert(sizeof(inst) <= sizeof(uintptr_t));
        Seq_addhi(stream, (void *)(uintptr_t)inst);
}

extern void Um_write_sequence(FILE *output, Seq_T stream) {
        while (Seq_length(stream) > 0) {
                void *word = Seq_remlo(stream);
                writeBEndian(word, output);
        }
}

/* Unit tests for the UM */

void emit_halt_test(Seq_T stream)
{
        emit(stream, halt());
}

void emit_verbose_halt_test(Seq_T stream)
{
        emit(stream, halt());
        emit(stream, loadval(r1, 'B'));
        emit(stream, output(r1));
        emit(stream, loadval(r1, 'a'));
        emit(stream, output(r1));
        emit(stream, loadval(r1, 'd'));
        emit(stream, output(r1));
        emit(stream, loadval(r1, '!'));
        emit(stream, output(r1));
        emit(stream, loadval(r1, '\n'));
        emit(stream, output(r1));
}

extern void emit_add_test(Seq_T stream) {
        emit(stream, add(r1, r2, r3));
        emit(stream, halt());
}

//output : 6
extern void emit_print_six_test(Seq_T stream) {
        emit(stream, loadval(r1, 48));
        emit(stream, loadval(r2, 6));
        emit(stream, add(r3, r1, r2));
        emit(stream, output(r3));
        emit(stream, halt());
}
/*FALSE*/
/*ouput: po*/
extern void emit_cMov_test(Seq_T stream) {
        emit(stream, loadval(r0, 0));
        emit(stream, loadval(r1, 111));
        emit(stream, loadval(r3, 112));
        emit(stream, cMov(r3, r1, r0));
        emit(stream, output(r3));
        emit(stream, output(r1));
        emit(stream, halt());
}
/* TRUE */
/*ouput: oo*/
extern void emit_cMov_test2(Seq_T stream) {
        emit(stream, loadval(r0, 1));
        emit(stream, loadval(r1, 111));
        emit(stream, loadval(r3, 112));
        emit(stream, cMov(r3, r1, r0));
        emit(stream, output(r3));
        emit(stream, output(r1));
        emit(stream, halt());
}

//works outputting nothing...
extern void seg_load_test(Seq_T stream) {
        emit(stream, loadval(r1, 0));
        emit(stream, loadval(r2, 0));
        emit(stream, loadval(r3, 1));
        emit(stream, activate(r4, r3));
        emit(stream, SLoad(r0, r3, r2));
        // emit(stream, output(r0));
        // emit(stream, output(r1));
        // emit(stream, output(r2));
        emit(stream, halt());
}

//DOESNT WORK
extern void seg_store_test(Seq_T stream) {
        emit(stream, loadval(r1, 0));
        emit(stream, loadval(r2, 0));
        emit(stream, loadval(r3, 111));
        emit(stream, loadval(r3, 1));
        emit(stream, activate(r4, r3));
        //emit(stream, output(r3));
        emit(stream, SStore(r1, r3, r0));
        emit(stream, SLoad(r4, r3, r0));
        emit(stream, output(r4));

}

//output: p
extern void multiply_test(Seq_T stream)
{
        emit(stream, loadval(r1, 4));
        emit(stream, loadval(r2, 28));   
        emit(stream, multiply(r3, r1, r2));
        emit(stream, output(r3));
        emit(stream, halt());
}

/*expected output: o*/
extern void division_test(Seq_T stream)
{
        emit(stream, loadval(r1, 1));
        emit(stream, loadval(r2, 111));   
        emit(stream, divide(r3, r2, r1));
        emit(stream, output(r3));
        emit(stream, halt());
}


//output: o
extern void nand_test(Seq_T stream)
{

        emit(stream, loadval(r1, 0));
        emit(stream, loadval(r2, 0));
        emit(stream, loadval(r7, 111));   
        emit(stream, nand(r3, r1, r2));  /*gets all 1 bits*/
        emit(stream, nand(r4, r3, r3));  /*gets all 0 bits*/
        emit(stream, add(r5, r4, r7));
        emit(stream, output(r5));
        emit(stream, halt());
}

/*???*/
extern void map_seg_test(Seq_T stream)
{
        emit(stream, loadval(r1, 1));
        emit(stream, activate(r2, r1));
        emit(stream, halt());
}
/*???*/
extern void unmap_seg_test(Seq_T stream)
{
        emit(stream, loadval(r1, 1));
        emit(stream, activate(r2, r1));
        emit(stream, inactivate(r2));
        emit(stream, activate(r2, r1));
        emit(stream, halt());
}


//DOESNT WORK
extern void input_output_test(Seq_T stream)
{
        emit(stream, input(r1));
        emit(stream, output(r1));
}



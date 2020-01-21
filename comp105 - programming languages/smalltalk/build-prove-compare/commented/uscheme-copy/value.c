#include "all.h"
/*
 * Implementation of micro-Scheme's value interface
 * 
 * The value interface has special support for Booleans
 * and for unspecified values. As usual, the value
 * interface also has support for printing.
 * 
 * Boolean values and Boolean testing
 * 
 * The first part of the value interface supports
 * Booleans.
 * <value.c>=
 */
bool istrue(Value v) {
    return v.alt != BOOLV || v.u.boolv;
}

Value truev, falsev;

void initvalue(void) {
    truev  = mkBoolv(true);
    falsev = mkBoolv(false);
}
/*
 * Unspecified values
 * 
 * The interface defines a function to return an
 * unspecified value. ``Unspecified'' means we can pick
 * any value we like. For example, we could just always
 * use \vnil. Unfortunately, if we do that, careless
 * persons will grow to rely on finding \vnil, and they
 * shouldn't. To foil such carelessness, we choose an
 * unhelpful value at random. [*]
 * <value.c>=
 */
Value unspecified (void) {
    switch ((rand()>>4) & 0x3) {
        case 0:  return truev;
        case 1:  return mkNum(rand());
        case 2:  return mkSym(strtoname("this value is unspecified"));
        case 3:  return mkPrimitive(-12, NULL);
        default: return mkNil();
    }
}
/*
 * With any luck, careless persons' code might make our
 * interpreter dereference a [[NULL]] pointer, which is
 * no worse than such persons deserve.
 */


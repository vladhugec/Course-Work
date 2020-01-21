#include "all.h"
/*
 * Implementation of error signaling
 * 
 * [*] The state of the error module includes the error
 * mode and the two [[jmp_buf]]s.
 * <error.c>=
 */
jmp_buf errorjmp;
jmp_buf testjmp;

static ErrorMode mode = NORMAL;
/*
 * Function [[set_error_mode]] sets the error mode. \
 * implabelset_error_mode
 * <error.c>=
 */
void set_error_mode(ErrorMode new_mode) {
  assert(new_mode == NORMAL || new_mode == TESTING);
  mode = new_mode;
}
/*
 * <error.c>=
 */
Printbuf errorbuf;
void runerror(const char *fmt, ...) {
    va_list_box box;

    if (!errorbuf)
        errorbuf = printbuf();

    assert(fmt);
    va_start(box.ap, fmt);
    vbprint(errorbuf, fmt, &box);
    va_end(box.ap);

    switch (mode) {
    case NORMAL:
        fflush(stdout);
        char *msg = bufcopy(errorbuf);
        fprintf(stderr, "Run-time error: %s\n", msg);
        fflush(stderr);
        free(msg);
        bufreset(errorbuf);
        longjmp(errorjmp, 1);

    case TESTING:
        longjmp(testjmp, 1);

    default:
        assert(0);
    }
}
/*
 * Function [[synerror]] is like [[runerror]], but with
 * additional logic for printing source-code locations.
 * Source-code locations are printed except from
 * standard input in the [[WITHOUT_LOCATIONS]] mode. \
 * implabelsynerror
 * <error.c>=
 */
static ErrorFormat toplevel_error_format = WITH_LOCATIONS;

void synerror(Sourceloc src, const char *fmt, ...) {
    va_list_box box;

    switch (mode) {
    case NORMAL:
        assert(fmt);
        fflush(stdout);
        if (toplevel_error_format == WITHOUT_LOCATIONS
        && !strcmp(src->sourcename, "standard input"))
            fprint(stderr, "syntax error: ");
        else
            fprint(stderr, "syntax error in %s, line %d: ", src->sourcename, src
                                                                        ->line);
        Printbuf buf = printbuf();
        va_start(box.ap, fmt);
        vbprint(buf, fmt, &box);
        va_end(box.ap);

        fwritebuf(buf, stderr);
        freebuf(&buf);
        fprintf(stderr, "\n");
        fflush(stderr);
        longjmp(errorjmp, 1);

    default:
        assert(0);
    }
}
/*
 * <error.c>=
 */
void set_toplevel_error_format(ErrorFormat new_format) {
  assert(new_format == WITH_LOCATIONS || new_format == WITHOUT_LOCATIONS);
  toplevel_error_format = new_format;
}
/*
 * Implementations of error helpers
 * 
 * As promised in \crefpageimpcore.error-helpers, here
 * are auxiliary functions that help detect common
 * errors. Function [[checkargc]] checks to see if the
 * number of actual arguments passed to a function is
 * the number that the function expected. \implabel
 * checkargc
 * <error.c>=
 */
void checkargc(Exp e, int expected, int actual) {
    if (expected != actual)
        runerror("in %e, expected %d argument%s but found %d",
                 e, expected, expected == 1 ? "" : "s", actual);
}
/*
 * If a list of names contains duplicates,
 * [[duplicatename]] returns a duplicate. It is used to
 * detect duplicate names in lists of formal parameters.
 * Its cost is quadratic in the number of parameters,
 * which for any reasonable function, should be very
 * fast. \implabelduplicatename
 * <error.c>=
 */
Name duplicatename(Namelist xs) {
    if (xs != NULL) {
        Name n = xs->hd;
        for (Namelist tail = xs->tl; tail; tail = tail->tl)
            if (n == tail->hd)
                return n;
        return duplicatename(xs->tl);
    }
    return NULL;
}
/*
 * The tail call could be turned into a loop, but it
 * hardly seems worth it. (Quirks of the C standard
 * prevent C compilers from optimizing all tail calls,
 * but any good C compiler will identify and optimize a
 * direct tail recursion like this one.)
 */


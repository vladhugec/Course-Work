#include "all.h"
/*
 * Building variadic functions on top of vbprint
 * 
 * Function [[bprint]] is a wrapper around [[vbprint]].
 * It calls [[va_start]] to initialize the list of
 * arguments in [[box]], passes the arguments to
 * [[vbprint]], and calls [[va_end]] to finalize the
 * arguments. The calls to [[va_start]] and [[va_end]]
 * are mandated by the C standard. \implabelbprint
 * <print.c>=
 */
void bprint(Printbuf output, const char *fmt, ...) {
    va_list_box box;

    assert(fmt);
    va_start(box.ap, fmt);
    vbprint(output, fmt, &box);
    va_end(box.ap);
}
/*
 * Function [[print]] buffers, then prints. It keeps a
 * buffer in a cache. \implabelprint
 * <print.c>=
 */
void print(const char *fmt, ...) {
    va_list_box box;
    static Printbuf stdoutbuf;

    if (stdoutbuf == NULL)
        stdoutbuf = printbuf();

    assert(fmt);
    va_start(box.ap, fmt);
    vbprint(stdoutbuf, fmt, &box);
    va_end(box.ap);
    fwritebuf(stdoutbuf, stdout);
    bufreset(stdoutbuf);
    fflush(stdout);
}
/*
 * Function [[fprint]] caches its own buffer. \implabel
 * fprint
 * <print.c>=
 */
void fprint(FILE *output, const char *fmt, ...) {
    static Printbuf buf;
    va_list_box box;

    if (buf == NULL)
        buf = printbuf();

    assert(fmt);
    va_start(box.ap, fmt);
    vbprint(buf, fmt, &box);
    va_end(box.ap);
    fwritebuf(buf, output);
    fflush(output);
    freebuf(&buf);
}
/*
 * Implementations of vbprint and installprinter
 * 
 * Function [[vbprint]]'s primary job is to decode the
 * format string and to find all the conversion
 * specifiers. Each time it sees a conversion specifier,
 * it calls the corresponding [[Printer]]. The
 * [[Printer]] for a conversion specifier [[c]] is
 * stored in [[printertab[(unsigned char)c]]]. \implabel
 * vbprint
 * <print.c>=
 */
static Printer *printertab[256];

void vbprint(Printbuf output, const char *fmt, va_list_box *box) {
    const unsigned char *p;
    bool broken = false;
                       /* made true on seeing an unknown conversion specifier */
    for (p = (const unsigned char*)fmt; *p; p++) {
        if (*p != '%') {
            bufput(output, *p);
        } else {
            if (!broken && printertab[*++p])
                printertab[*p](output, box);
            else {
                broken = true;  /* box is not consumed */
                bufputs(output, "<pointer>");
            }
        }
    }
}
/*
 * The [[va_arg]] interface is unsafe, and if a printing
 * function takes the wrong thing from [[box]], a memory
 * error could ensue. So if [[vbprint]] ever sees a
 * conversion specifier that it doesn't recognize,
 * it stops calling printing functions.
 */

/*
 * Function [[installprinter]] simply stores to the
 * private table. \implabelinstallprinter
 * <print.c>=
 */
void installprinter(unsigned char c, Printer *take_and_print) {
    printertab[c] = take_and_print;
}
/*
 * As in standard [[vprintf]], the conversion specifier
 * [[ prints a percent sign, without consuming any
 * arguments.
 * <print.c>=
 */
void printpercent(Printbuf output, va_list_box *box) {
    (void)box;
    bufput(output, '%');
}
/*
 * The printers for strings and numbers are textbook
 * examples of how to use [[va_arg]].
 * <print.c>=
 */
void printstring(Printbuf output, va_list_box *box) {
    const char *s = va_arg(box->ap, char*);
    bufputs(output, s);
}

void printdecimal(Printbuf output, va_list_box *box) {
    char buf[2 + 3 * sizeof(int)];
    snprintf(buf, sizeof(buf), "%d", va_arg(box->ap, int));
    bufputs(output, buf);
}

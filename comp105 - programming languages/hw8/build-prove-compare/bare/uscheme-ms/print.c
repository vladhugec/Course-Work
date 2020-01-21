#include "all.h"
/* print.c 1062b */
void bprint(Printbuf output, const char *fmt, ...) {
    va_list_box box;

    assert(fmt);
    va_start(box.ap, fmt);
    vbprint(output, fmt, &box);
    va_end(box.ap);
}
/* print.c 1063a */
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
/* print.c 1063b */
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
/* print.c 1064a */
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
/* print.c 1064b */
void installprinter(unsigned char c, Printer *take_and_print) {
    printertab[c] = take_and_print;
}
/* print.c 1064d */
void printpercent(Printbuf output, va_list_box *box) {
    (void)box;
    bufput(output, '%');
}
/* print.c 1065a */
void printstring(Printbuf output, va_list_box *box) {
    const char *s = va_arg(box->ap, char*);
    bufputs(output, s);
}

void printdecimal(Printbuf output, va_list_box *box) {
    char buf[2 + 3 * sizeof(int)];
    snprintf(buf, sizeof(buf), "%d", va_arg(box->ap, int));
    bufputs(output, buf);
}

#include "all.h"
/*
 * Implementation of a print buffer
 * 
 * This classic data structure needs no introduction.
 * <printbuf.c>=
 */
struct Printbuf {
    char *chars;  // start of the buffer
    char *limit;  // marks one past end of buffer
    char *next;   // where next character will be buffered
    // invariants: all are non-NULL
    //             chars <= next <= limit
    //             if chars <= p < limit, then *p is writeable
};
/*
 * A buffer initially holds 100 characters.
 * <printbuf.c>=
 */
Printbuf printbuf(void) {
   Printbuf buf = malloc(sizeof(*buf));
   assert(buf);
   int n = 100;
   buf->chars = malloc(n);
   assert(buf->chars);
   buf->next  = buf->chars;
   buf->limit = buf->chars + n;
   return buf;
}
/*
 * We free a buffer using Hanson's \citeyearpar
 * hanson:interfaces-implementations indirection trick.
 * <printbuf.c>=
 */
void freebuf(Printbuf *bufp) {
   Printbuf buf = *bufp;
   assert(buf && buf->chars);
   free(buf->chars);
   free(buf);
   *bufp = NULL;
}
/*
 * Calling [[grow]] makes a buffer 30% larger, or at
 * least 1 byte larger.
 * <printbuf.c>=
 */
static void grow(Printbuf buf) {
    assert(buf && buf->chars && buf->next && buf->limit);
    unsigned n = buf->limit - buf->chars;
    n = 1 + (n * 13) / 10;   // 30% size increase
    unsigned i = buf->next - buf->chars;
    buf->chars = realloc(buf->chars, n);
    assert(buf->chars);
    buf->next  = buf->chars + i;
    buf->limit = buf->chars + n;
}
/*
 * We write a character, at [[buf->next]], growing if
 * needed.
 * <printbuf.c>=
 */
void bufput(Printbuf buf, char c) {
    assert(buf && buf->next && buf->limit);
    if (buf->next == buf->limit) {
        grow(buf);
        assert(buf && buf->next && buf->limit);
        assert(buf->limit > buf->next);
    }
    *buf->next++ = c;
}
/*
 * To write a string, we grow until we can call
 * [[memcpy]].
 * <printbuf.c>=
 */
void bufputs(Printbuf buf, const char *s) {
    assert(buf);
    int n = strlen(s);
    while (buf->limit - buf->next < n)
        grow(buf);
    memcpy(buf->next, s, n);
    buf->next += n;
}
/*
 * To discard all the characters, [[bufreset]].
 * <printbuf.c>=
 */
void bufreset(Printbuf buf) {
    assert(buf && buf->next);
    buf->next = buf->chars;
}
/*
 * To use the buffer, we want to know how many
 * characters are in it.
 * <printbuf.c>=
 */
static int nchars(Printbuf buf) {
    assert(buf && buf->chars && buf->next);
    return buf->next - buf->chars;
}
/*
 * Copy a buffer to a fresh block.
 * <printbuf.c>=
 */
char *bufcopy(Printbuf buf) {
   assert(buf);
   int n = nchars(buf);
   char *s = malloc(n+1);
   assert(s);
   memcpy(s, buf->chars, n);
   s[n] = '\0';
   return s;
}
/*
 * Write a buffer's characters to an open file handle.
 * <printbuf.c>=
 */
void fwritebuf(Printbuf buf, FILE *output) {
    assert(buf && buf->chars && buf->limit);
    assert(output);
    int n = fwrite(buf->chars, sizeof(*buf->chars), nchars(buf), output);
    assert(n == nchars(buf));
}

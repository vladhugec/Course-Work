#include "all.h"
/* printbuf.c 1058f */
struct Printbuf {
    char *chars;  // start of the buffer
    char *limit;  // marks one past end of buffer
    char *next;   // where next character will be buffered
    // invariants: all are non-NULL
    //             chars <= next <= limit
    //             if chars <= p < limit, then *p is writeable
};
/* printbuf.c 1059a */
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
/* printbuf.c 1059b */
void freebuf(Printbuf *bufp) {
   Printbuf buf = *bufp;
   assert(buf && buf->chars);
   free(buf->chars);
   free(buf);
   *bufp = NULL;
}
/* printbuf.c 1059c */
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
/* printbuf.c 1059d */
void bufput(Printbuf buf, char c) {
    assert(buf && buf->next && buf->limit);
    if (buf->next == buf->limit) {
        grow(buf);
        assert(buf && buf->next && buf->limit);
        assert(buf->limit > buf->next);
    }
    *buf->next++ = c;
}
/* printbuf.c 1060a */
void bufputs(Printbuf buf, const char *s) {
    assert(buf);
    int n = strlen(s);
    while (buf->limit - buf->next < n)
        grow(buf);
    memcpy(buf->next, s, n);
    buf->next += n;
}
/* printbuf.c 1060b */
void bufreset(Printbuf buf) {
    assert(buf && buf->next);
    buf->next = buf->chars;
}
/* printbuf.c 1060c */
static int nchars(Printbuf buf) {
    assert(buf && buf->chars && buf->next);
    return buf->next - buf->chars;
}
/* printbuf.c 1060d */
char *bufcopy(Printbuf buf) {
   assert(buf);
   int n = nchars(buf);
   char *s = malloc(n+1);
   assert(s);
   memcpy(s, buf->chars, n);
   s[n] = '\0';
   return s;
}
/* printbuf.c 1060e */
void fwritebuf(Printbuf buf, FILE *output) {
    assert(buf && buf->chars && buf->limit);
    assert(output);
    int n = fwrite(buf->chars, sizeof(*buf->chars), nchars(buf), output);
    assert(n == nchars(buf));
}

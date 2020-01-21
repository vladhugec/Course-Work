#include "all.h"
/* lex.c 1052c */
struct Parstream {
    Linestream lines;     /* source of more lines */
    const char *input;
                       /* what's not yet read from the most recent input line */
    /* invariant: unread is NULL only if lines is empty */

    struct {
       const char *ps1, *ps2;
    } prompts;
};
/* lex.c 1052d */
Parstream parstream(Linestream lines, Prompts prompts) {
    Parstream pars = malloc(sizeof(*pars));
    assert(pars);
    pars->lines = lines;
    pars->input = "";
    pars->prompts.ps1 = prompts == STD_PROMPTS ? "-> " : "";
    pars->prompts.ps2 = prompts == STD_PROMPTS ? "   " : "";
    return pars;
}
/* lex.c 1052e */
Sourceloc parsource(Parstream pars) {
    return &pars->lines->source;
}
/* lex.c 1054a */
/* prototypes of private functions that help with [[getpar]] 1055d */
static Name readatom(const char **ps);
/* prototypes of private functions that help with [[getpar]] 1056b */
static Parlist reverse_parlist(Parlist p);
/* prototypes of private functions that help with [[getpar]] 1057b */
static int  isdelim(char c);
static Name strntoname(const char *s, int n);
/* prototypes of private functions that help with [[getpar]] 1057d */
static bool brackets_match(char left, char right);
static Par getpar_in_context(Parstream pars, bool is_first, char left) {
    if (pars->input == NULL)
        return NULL;
    else {
        char right;      // will hold right bracket, if any
        /* advance [[pars->input]] past whitespace characters 1055a */
        while (isspace((unsigned char)*pars->input))
            pars->input++;
        switch (*pars->input) {
        case '\0':  /* on end of line, get another line and continue */
        case ';':
            pars->input = getline_(pars->lines,
                                   is_first ? pars->prompts.ps1 : pars->
                                                                   prompts.ps2);
            return getpar_in_context(pars, is_first, left);
        case '(': case '[': 
            /* read and return a parenthesized [[LIST]] 1055e */
            {
                char left = *pars->input++;
                                         /* remember the opening left bracket */

                Parlist elems_reversed = NULL;
                Par q;
                   /* next par read in, to be accumulated into elems_reversed */
                while ((q = getpar_in_context(pars, false, left)))
                    elems_reversed = mkPL(q, elems_reversed);

                if (pars->input == NULL)
                    synerror(parsource(pars),

              "premature end of file reading list (missing right parenthesis)");
                else
                    return mkList(reverse_parlist(elems_reversed));
            }
        case ')': case ']': case '}':
            right = *pars->input++;
                                 /* pass the bracket so we don't see it again */
            if (is_first) {
                synerror(parsource(pars), "unexpected right bracket %c", right);
                assert(0); /* not reached, but the compiler doesn't know this */
            } else if (left == '\'') {
                synerror(parsource(pars), "quote ' followed by right bracket %c"
                                                                               ,
                         right);
                assert(0); /* not reached, but the compiler doesn't know this */
            } else if (!brackets_match(left, right)) {
                synerror(parsource(pars), "%c does not match %c", right, left);
                assert(0); /* not reached, but the compiler doesn't know this */
            } else {
                return NULL;
            }
        case '{':
            pars->input++;
            synerror(parsource(pars), "curly brackets are not supported");
            assert(0); /* not reached, but the compiler doesn't know this */
        default:
            if (read_tick_as_quote && *pars->input == '\'') {

         /* read a [[Par]] and return that [[Par]] wrapped in [[quote]] 1055b */
                {
                    pars->input++;
                    Par p = getpar_in_context(pars, false, '\'');
                    if (p == NULL)
                        synerror(parsource(pars),
                                      "premature end of file after quote mark");
                    assert(p);
                    return mkList(mkPL(mkAtom(strtoname("quote")), mkPL(p, NULL)
                                                                             ));
                }
            } else {
                /* read and return an [[ATOM]] 1055c */
                return mkAtom(readatom(&pars->input));
            }
        }   
    }
}
/* lex.c 1054b */
Par getpar(Parstream pars) {
    assert(pars);
    return getpar_in_context(pars, true, '\0');
}
/* lex.c 1056a */
static Parlist reverse_parlist(Parlist p) {
    Parlist reversed = NULL;
    Parlist remaining = p;
    /* Invariant: reversed followed by reverse(remaining) equals reverse(p) */
    while (remaining) {
        Parlist next = remaining->tl;
        remaining->tl = reversed;
        reversed = remaining;
        remaining = next;
    }
    return reversed;
}                      
/* lex.c 1056c */
static Name readatom(const char **ps) {
    const char *p, *q;

    p = *ps;                          /* remember starting position */
    for (q = p; !isdelim(*q); q++)    /* scan to next delimiter */
        ;
    *ps = q;
                                    /* unconsumed input starts with delimiter */
    return strntoname(p, q - p);      /* the name is the difference */
}
/* lex.c 1056d */
static int isdelim(char c) {
    return c == '(' || c == ')' || c == '[' || c == ']' || c == '{' || c == '}'
                                                                              ||
           c == ';' || isspace((unsigned char)c) || 
           c == '\0' || (read_tick_as_quote && c == '\'');
}
/* lex.c 1057a */
static Name strntoname(const char *s, int n) {
    char *t = malloc(n + 1);
    assert(t != NULL);
    strncpy(t, s, n);
    t[n] = '\0';
    return strtoname(t);
}
/* lex.c 1057c */
static bool brackets_match(char left, char right) {
    switch (left) {
        case '(': return right == ')';
        case '[': return right == ']';
        case '{': return right == '}';
        default: assert(0);
    }
}

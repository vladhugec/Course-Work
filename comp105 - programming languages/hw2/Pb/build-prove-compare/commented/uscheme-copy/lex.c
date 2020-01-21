#include "all.h"
/*
 * Implementation of Parstream
 * 
 * The representation of a [[Parstream]] has three
 * parts:
 * 
 *   • The [[lines]] field is a source of input lines.
 *   • The [[input]] field contains characters from an
 *  input line; if a [[Par]] has already been read
 *  from that line, [[input]] contains only the
 *  characters left over.
 *   • The [[prompts]] structure contains strings that
 *  are printed every time a line is taken from
 *  [[lines]]. When the [[Parstream]] is reading a
 *  fresh [[Par]], it issues [[prompts.ps1]] for the
 *  first line of that [[Par]]. When it has to read a
 *  [[Par]] that spans more than one line, like a
 *  long function definition, it issues
 *  [[prompts.ps2]] for all the rest of the lines.
 *  The names [[ps1]] and [[ps2]] stand for ``prompt
 *  string'' 1 and 2; they come from the Unix shell.
 * 
 * \implabelParstream
 * <lex.c>=
 */
struct Parstream {
    Linestream lines;     /* source of more lines */
    const char *input;
                       /* what's not yet read from the most recent input line */
    /* invariant: unread is NULL only if lines is empty */

    struct {
       const char *ps1, *ps2;
    } prompts;
};
/*
 * To create a [[Parstream]], I initialize the fields
 * using the parameters. Initializing [[input]] to an
 * empty string puts the stream into a state with no
 * characters left over. [*] \implabelparstream
 * <lex.c>=
 */
Parstream parstream(Linestream lines, Prompts prompts) {
    Parstream pars = malloc(sizeof(*pars));
    assert(pars);
    pars->lines = lines;
    pars->input = "";
    pars->prompts.ps1 = prompts == STD_PROMPTS ? "-> " : "";
    pars->prompts.ps2 = prompts == STD_PROMPTS ? "   " : "";
    return pars;
}
/*
 * Function [[parsource]] grabs the current source
 * location out of the [[Linestream]].
 * <lex.c>=
 */
Sourceloc parsource(Parstream pars) {
    return &pars->lines->source;
}
/*
 * Function [[getpar]] presents a minor problem: the
 * [[Par]] type is defined recursively, so [[getpar]]
 * itself must be recursive. But the first call to
 * [[getpar]] is distinct from the others in two ways:
 * 
 *   • If the first call prompts, it should use
 *  [[prompts.ps1]]. Other calls should use
 *  [[prompts.ps2]]
 *   • If the first call encounters a right parenthesis,
 *  then the right parenthesis is unbalanced, and
 *  [[getpar]] should report it as a syntax error.
 *  If another call encounters a right parenthesis,
 *  then the right parenthesis marks the end of a
 *  [[LIST]], and [[getpar]] should scan past it and
 *  return.
 * 
 * I deal with this distinction by writing
 * [[getpar_in_context]], which knows whether it is the
 * first call or another call. Function [[getpar]]
 * attempts to read a [[Par]]. If it runs out of input,
 * it returns [[NULL]]. If it sees a right parenthesis,
 * it returns [[NULL]] if and only if [[is_first]] is
 * false; otherwise, it calls [[synerror]]. \implabel
 * getpar [*]
 * <lex.c>=
 */
/*
 * <prototypes of private functions that help with [[getpar]]>=
 */
static Name readatom(const char **ps);
/*
 * <prototypes of private functions that help with [[getpar]]>=
 */
static Parlist reverse_parlist(Parlist p);
/*
 * <prototypes of private functions that help with [[getpar]]>=
 */
static int  isdelim(char c);
static Name strntoname(const char *s, int n);
/*
 * <prototypes of private functions that help with [[getpar]]>=
 */
static bool brackets_match(char left, char right);
static Par getpar_in_context(Parstream pars, bool is_first, char left) {
    if (pars->input == NULL)
        return NULL;
    else {
        char right;      // will hold right bracket, if any
        /*
         * To scan past whitespace, I use the standard C library
         * function [[isspace]]. That function requires an
         * unsigned character.
         * <advance [[pars->input]] past whitespace characters>=
         */
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
            /*
             * Reading and returning a parenthesized list
             * 
             * After a left parenthesis, I read [[Par]]s until I see
             * a right parenthesis, adding each one to the front of
             * [[elems_reversed]]. When I get to the closing right
             * parenthesis, I reverse the elements in place and
             * return the resulting list.
             * <read and return a parenthesized [[LIST]]>=
             */
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
                /*
                 * When [[getpar]] sees a quote mark ``[[']],'' if it is
                 * reading a language that uses a [[']] operator, it
                 * reads the next [[Par]] (for example, [[(1 2 3)]]) and
                 * then returns that [[Par]] wrapped in [[quote]] (for
                 * example, [[(quote (1 2 3))]]).
                 * <read a [[Par]] and return that [[Par]] wrapped in [[quote]]
                                                                              >=
                 */
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
                /*
                 * Atoms are delegated to function [[readatom]], defined
                 * below.
                 * <read and return an [[ATOM]]>=
                 */
                return mkAtom(readatom(&pars->input));
            }
        }   
    }
}
/*
 * With this code in hand, [[getpar]] is a first call.
 */

/*
 * <lex.c>=
 */
Par getpar(Parstream pars) {
    assert(pars);
    return getpar_in_context(pars, true, '\0');
}
/*
 * To reverse a list, I use a classic trick of
 * imperative programming: I update the pointers in
 * place. The invariant is exactly the same as the
 * invariant of [[revapp]] in \crefscheme.revapp on \
 * cpagerefscheme.revapp. But the code in \cref
 * scheme.revapp allocates new memory; the code here
 * only updates pointers, without allocating.
 * <lex.c>=
 */
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
/*
 * Reading and returning an atom
 * 
 * A lexical analyzer consumes input one character at a
 * time. My code works with a pointer to the input
 * characters. A typical function uses such a pointer to
 * look at the input, converts some of the input to a
 * result, and updates the pointer to point to the
 * remaining, unconsumed input. To make the update
 * possible, I must pass a pointer to the pointer, which
 * has type [[char **]]. [In C++, I would instead pass
 * the pointer by reference.] Here, for example,
 * [[readatom]] consumes the characters that form a
 * single atom.
 * <lex.c>=
 */
static Name readatom(const char **ps) {
    const char *p, *q;

    p = *ps;                          /* remember starting position */
    for (q = p; !isdelim(*q); q++)    /* scan to next delimiter */
        ;
    *ps = q;
                                    /* unconsumed input starts with delimiter */
    return strntoname(p, q - p);      /* the name is the difference */
}
/*
 * A delimiter is a character that marks the end of a
 * name or a token. In bridge languages, delimiters
 * include parentheses, semicolon, whitespace, end of
 * string, and possibly the quote mark.
 * <lex.c>=
 */
static int isdelim(char c) {
    return c == '(' || c == ')' || c == '[' || c == ']' || c == '{' || c == '}'
                                                                              ||
           c == ';' || isspace((unsigned char)c) || 
           c == '\0' || (read_tick_as_quote && c == '\'');
}
/*
 * <lex.c>=
 */
static Name strntoname(const char *s, int n) {
    char *t = malloc(n + 1);
    assert(t != NULL);
    strncpy(t, s, n);
    t[n] = '\0';
    return strtoname(t);
}
/*
 * <lex.c>=
 */
static bool brackets_match(char left, char right) {
    switch (left) {
        case '(': return right == ')';
        case '[': return right == ']';
        case '{': return right == '}';
        default: assert(0);
    }
}

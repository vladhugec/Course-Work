#include "all.h"
/*
 * With these properties in mind, here is my plan:
 * 
 *  1. There will be two parsers: one for expressions
 *  and one for extended definitions.
 *  2. If a parser sees an atom, it must know what to
 *  do.
 *  3. If a parser sees a parenthesized [[Parlist]],
 *  it will consult a table of rows.
 *    □ Each row knows how to parse one syntactic
 *      form. What does it mean ``to know how to
 *      parse''? The row begins with a keyword that
 *      the parser should look for. The row also
 *      includes an integer code that identifies the
 *      form, and finally, the row lists the
 *      components of the form. To see some example
 *      rows, look at the parsing table for Impcore,
 *      in \tabrefcparse.fig.exptable.
 *    □ A row matches an input [[Parlist]] if the
 *      row's keyword is equal to the first element
 *      of the [[Parlist]]. The parser proceeds
 *      through the rows looking for one that matches
 *      its input.
 * 
 *  \promissory
 *  ━━━━━━━━━━━━━━━━━━━━━━━━━�
         ��━━━━━━━━━━━━━━━━━━━━━━━
 * 
 *                                  |
 *       Parsed components          |      Unparsed inputs
 *                                  |
 * 
 *  Sketch of a parsing machine (ParserState)
 *  ━━━━━━━━━━━━━━━━━━━━━━━━━�
         ��━━━━━━━━━━━━━━━━━━━━━━━
 * 
 *  4. Once the parser finds the right row, it gets each
 *  component from the input [[Parlist]], then checks
 *  to make sure there are no leftover inputs.
 *  Finally it passes the components and the integer
 *  code to a reduce function. Impcore uses two such
 *  functions: [[reduce_to_exp]] and
 *  [[reduce_to_xdef]]. Each of these functions takes
 *  a sequence of components and reduces it to a
 *  single node in an abstract-syntax tree. (The name
 *  [[reduce]] comes from shift-reduce parsing, which
 *  refers to a family of parsing techniques of which
 *  my parsers are members.)
 * 
 * I've designed the parser to work this way so that you
 * can easily add new syntactic forms. It's as simple as
 * adding a row to a table and a case to a reduce
 * function. In more detail,
 * 
 *  1. Decide whether you wish to add an expression form
 *  or a definition form. That will tell you what
 *  table and reduce function to modify. For example,
 *  if you want to add a new expression form, modify
 *  [[exptable]] and [[reduce_to_exp]].
 *  2. Choose a keyword and an unused integer code.
 *  As shown below, codes for extended definitions
 *  have to be chosen with a little care.
 *  3. Add a row to your chosen table.
 *  4. Add a case to your chosen reduce function.
 * 
 * I think you'll like being able to extend languages so
 * easily, but there's a cost—the table-driven parser
 * needs a lot of infrastructure. That infrastructure,
 * which lives in file parse.c, is described below.
 * <tableparsing.c>=
 */
/*
 * <private function prototypes for parsing>=
 */
static Namelist parsenamelist(Parlist ps, ParsingContext context);
/*
 * <private function prototypes for parsing>=
 */
static bool rowmatches(struct ParserRow *row, Name first);
/*
 * <private function prototypes for parsing>=
 */
void *name_error(Par bad, struct ParsingContext *context); 
                     /* expected a name, but got something else */
/*
 * When we create a new parser state, all we know is
 * what [[Par]] we're trying to parse. That gives us the
 * input and part of the context. The output is empty.
 * <tableparsing.c>=
 */
struct ParserState mkParserState(Par p, Sourceloc source) {
    assert(p->alt == LIST);
    assert(source != NULL && source->sourcename != NULL);
    struct ParserState s;
    s.input          = p->u.list;
    s.context.par    = p;
    s.context.source = source;
    s.context.name   = NULL;
    s.nparsed        = 0;
    return s;
}
/*
 * The shift operation itself is implemented in two
 * halves. The first half removes an input and ensures
 * that there is room for a component. The second half
 * writes the component and updates [[nparsed]]. The
 * first half is the same for every shift function, and
 * it looks like this:
 * <tableparsing.c>=
 */
void halfshift(ParserState s) {
    assert(s->input);
    s->input = s->input->tl;
    assert(s->nparsed < MAXCOMPS);
}
/*
 * Here's a full shift for an expression. It calls
 * [[parseexp]], with which it is mutually recursive.
 * <tableparsing.c>=
 */
ParserResult sExp(ParserState s) {
    if (s->input == NULL) {
        return INPUT_EXHAUSTED;
    } else {
        Par p = s->input->hd;
        halfshift(s);
        s->components[s->nparsed++].exp = parseexp(p, s->context.source);
        return PARSED;
    }
}
/*
 * Function [[sExps]] converts the entire input into an
 * [[Explist]]. The [[halfshift]] isn't useful here. And
 * a [[NULL]] input is OK; it just parses into an empty
 * [[Explist]].
 * <tableparsing.c>=
 */
ParserResult sExps(ParserState s) {
    Explist es = parseexplist(s->input, s->context.source);
    assert(s->nparsed < MAXCOMPS);
    s->input = NULL;
    s->components[s->nparsed++].exps = es;
    return PARSED;
}
/*
 * Function [[parseexplist]] is defined below with the
 * other parsing functions.
 */

/*
 * Function [[sName]] is structured just like [[sExp]];
 * the only difference is that where [[sExp]] calls
 * [[parseexp]], [[sName]] calls [[parsename]].
 * <tableparsing.c>=
 */
ParserResult sName(ParserState s) {
    if (s->input == NULL) {
        return INPUT_EXHAUSTED;
    } else {
        Par p = s->input->hd;
        halfshift(s);
        s->components[s->nparsed++].name = parsename(p, &s->context);
        return PARSED;
    }
}
/*
 * Notice that [[parsename]], which is defined below,
 * takes the current context as an extra parameter. That
 * context enables [[parsename]] to give a good error
 * message if it encounters an input that is not a valid
 * name.
 */

/*
 * <tableparsing.c>=
 */
ParserResult sNamelist(ParserState s) {
    if (s->input == NULL) {
        return INPUT_EXHAUSTED;
    } else {
        Par p = s->input->hd;
        switch (p->alt) {
        case ATOM:
            synerror(s->context.source, "%p: usage: (define fun (formals) body)"
                                                                               ,
                     s->context.par);
        case LIST:
            halfshift(s);
            s->components[s->nparsed++].names = parsenamelist(p->u.list, &s->
                                                                       context);
            return PARSED;
        }
        assert(0);
    }
}
/*
 * These shift functions aren't used just to move
 * information from input to components. A sequence of
 * shift functions represents what components are
 * expected to be part of a syntactic form. (This
 * technique of using functions as data is developed at
 * length in \chaprefscheme.) To parse a syntactic form,
 * I call the functions in sequence. As an
 * end-of-sequence marker, I use the function [[stop]].
 * It checks to be sure all input is consumed and
 * signals that it is time to stop parsing. Unlike the
 * other shift functions, it does not change the
 * [[state]].
 * <tableparsing.c>=
 */
ParserResult stop(ParserState state) {
    if (state->input == NULL)
        return STOP_PARSING;
    else
        return INPUT_LEFTOVER;
}    
/*
 * Finally, I have a special shift function that doesn't
 * do any shifting. Instead, it sets the context for
 * parsing a function definition. Right after calling
 * [[sName]] with the function name, I call
 * [[setcontextname]].
 * <tableparsing.c>=
 */
ParserResult setcontextname(ParserState s) {
    assert(s->nparsed > 0);
    s->context.name = s->components[s->nparsed-1].name;
    return PARSED;
}
/*
 * \exrefimpcore.ex.localvars asks you to add local
 * variables to Impcore. Shift function [[sLocals]]
 * looks for the keyword [[locals]]. If found, the
 * keyword marks a list of the names of local variables.
 * This list of names is shifted into the [[s->
 * components]] array. If the keyword [[locals]] is not
 * found, there are no local variables, and a [[NULL]]
 * pointer is shifted into the [[s->components]] array.
 * <tableparsing.c>=
 */
ParserResult sLocals(ParserState s) {
    Par p = s->input ? s->input->hd : NULL;  // useful abbreviation
    if (/*
         * The keyword test is just complicated enough that it
         * warrants being put in a named code chunk.
         * <[[Par p]] represents a list beginning with keyword [[locals]]>=
         */
        p != NULL && p->alt == LIST && p->u.list != NULL &&
        p->u.list->hd->alt == ATOM && p->u.list->hd->u.atom == strtoname(
                                                                    "locals")) {
        struct ParsingContext context;
        context.name = strtoname("locals");
        context.par = p;
        halfshift(s);
        s->components[s->nparsed++].names = parsenamelist(p->u.list->tl, &
                                                                       context);
        return PARSED;
    } else {        
        s->components[s->nparsed++].names = NULL;
        return PARSED;
    }
}
/*
 * To parse an input using a row, function [[rowparse]]
 * calls shift functions until a shift function says to
 * stop—or detects an error.
 * <tableparsing.c>=
 */
void rowparse(struct ParserRow *row, ParserState s) {
    ShiftFun *f = &row->shifts[0];

    for (;;) {
        ParserResult r = (*f)(s);
        switch (r) {
        case PARSED:          f++; break;
        case STOP_PARSING:    return;
        case INPUT_EXHAUSTED: 
        case INPUT_LEFTOVER:  
        case BAD_INPUT:       usage_error(row->code, r, &s->context);
        }
    }
}
/*
 * <tableparsing.c>=
 */
struct ParserRow *tableparse(ParserState s, ParserTable t) {
    if (s->input == NULL)
        synerror(s->context.source, "%p: empty list in input", s->context.par);

    Name first = s->input->hd->alt == ATOM ? s->input->hd->u.atom : NULL;

                          // first Par in s->input, if it is present and an atom

    unsigned i;  // to become the index of the matching row in ParserTable t
    for (i = 0; !rowmatches(&t[i], first); i++) 
        ;
    /*
     * Once a row has matched, what we do with it depends on
     * whether it was a [[NULL]] match or a keyword match.
     * If row [[t[i]]] has a keyword, then the first [[Par]]
     * in the input is that keyword, and it needs to be
     * consumed—so we adjust [[s->input]]. And we set the
     * context.
     * <adjust the state [[s]] so it's ready to start parsing using row [[t[i]]]
                                                                              >=
     */
    if (t[i].keyword) {
        assert(first != NULL);
        s->input = s->input->tl;
        s->context.name = first;
    }
    rowparse(&t[i], s);
    return &t[i];
}
/*
 * A row matches if the row's keyword is [[NULL]] or if
 * the keyword stands for the same name as [[first]].
 * <tableparsing.c>=
 */
static bool rowmatches(struct ParserRow *row, Name first) {
    return row->keyword == NULL || strtoname(row->keyword) == first;
}
/*
 * And here is the corresponding parsing function. The
 * parsing function delegates the heavy lifting to other
 * functions: [[exp_of_atom]] deals with atoms, and
 * [[tableparse]] and [[reduce_to_exp]] deal with lists.
 * <tableparsing.c>=
 */
Exp parseexp(Par p, Sourceloc source) {
    switch (p->alt) {
    case ATOM:
        /*
         * Misuse of reserved words is detected by the following
         * check, which prevents such oddities as a user-defined
         * function named [[if]]. A word is reserved if it
         * appears in [[exptable]] or [[xdeftable]].
         * <if [[p->u.atom]] is a reserved word, call [[synerror]] with
                                                                    [[source]]>=
         */
        for (struct ParserRow *entry = exptable; entry->keyword != NULL; entry++
                                                                               )
            if (p->u.atom == strtoname(entry->keyword))
                synerror(source, "%n is a reserved word and may not be used "
                         "to name a variable or function", p->u.atom);
        for (struct ParserRow *entry = xdeftable; entry->keyword != NULL; entry
                                                                             ++)
            if (p->u.atom == strtoname(entry->keyword))
                synerror(source, "%n is a reserved word and may not be used "
                         "to name a variable or function", p->u.atom);
        return exp_of_atom(p->u.atom);
    case LIST: 
        {   struct ParserState s = mkParserState(p, source);
            struct ParserRow *row = tableparse(&s, exptable);
            if (row->code == EXERCISE) {
                synerror(source, "implementation of %n is left as an exercise",
                         s.context.name);
            } else {
                Exp e = reduce_to_exp(row->code, s.components);
                check_exp_duplicates(source, e);
                return e;
            }
        }
    }
    assert(0);
}
/*
 * In later chapters, function [[parseexp]] is resued
 * with different versions of [[exp_of_atom]],
 * [[exptable]], and [[reduce_to_exp]].
 */

/*
 * Next, here are the parsing table and function for
 * extended definitions. The extended-definition table
 * is shared among several languages. Because it is
 * shared, I put it in tableparsing.c, not in parse.c.
 * <tableparsing.c>=
 */
static ShiftFun valshifts[]      = { sName, sExp,
                                                                         stop };
static ShiftFun defineshifts[]   = { sName, setcontextname, sNamelist, sExp,
                                                                         stop };
static ShiftFun useshifts[]      = { sName,
                                                                         stop };
static ShiftFun checkexpshifts[] = { sExp, sExp,
                                                                         stop };
static ShiftFun checkassshifts[] = { sExp,
                                                                         stop };
static ShiftFun checkerrshifts[] = { sExp,
                                                                         stop };
static ShiftFun expshifts[]      = { use_exp_parser };

struct ParserRow xdeftable[] = { 
    { "val",          ADEF(VAL),           valshifts },
    { "define",       ADEF(DEFINE),        defineshifts },
    { "use",          ANXDEF(USE),         useshifts },
    { "check-expect", ATEST(CHECK_EXPECT), checkexpshifts },
    { "check-assert", ATEST(CHECK_ASSERT), checkassshifts },
    { "check-error",  ATEST(CHECK_ERROR),  checkerrshifts },
    /*
     * The conditional sugar doesn't require any new
     * definition forms.
     * <rows added to [[xdeftable]] in exercises>=
     */
    /* add new forms for extended definitions here */
    { NULL,           ADEF(EXP),           expshifts }  /* must come last */
};
/*
 * <tableparsing.c>=
 */
XDef parsexdef(Par p, Sourceloc source) {
    switch (p->alt) {
    case ATOM:
        return mkDef(mkExp(parseexp(p, source)));
    case LIST:;
        struct ParserState s  = mkParserState(p, source);
        struct ParserRow *row = tableparse(&s, xdeftable);
        XDef d = reduce_to_xdef(row->code, s.components);
        if (d->alt == DEF)
            check_def_duplicates(source, d->u.def);
        return d;
    }
    assert(0);
}
/*
 * The case for a top-level [[EXP]] node has just one
 * component, an [[Exp]]. I can't use [[sExp]] here,
 * because that consumes just a single item from the
 * input, as an [[Exp]]. What I need is to treat the
 * entire input as an [[Exp]]. Shift function
 * [[use_exp_parser]] does the work. This function
 * ignores [[s->input]]; instead it uses [[s->
 * context.par]], which gets passed to [[parseexp]].
 * <tableparsing.c>=
 */
ParserResult use_exp_parser(ParserState s) {
    Exp e = parseexp(s->context.par, s->context.source);
    halfshift(s);
    s->components[s->nparsed++].exp = e;
    return STOP_PARSING;
}
/*
 * Whenever I expect a name, I actually parse a full
 * expression. Then, if it isn't a name, I complain.
 * This technique allows maximum latitude in case the
 * programmer makes a mistake. The error-handling
 * function [[name_error]] is described below.
 * <tableparsing.c>=
 */
Name parsename(Par p, ParsingContext context) {
    Exp e = parseexp(p, context->source);
    if (e->alt != VAR)
        return name_error(p, context);
    else
        return e->u.var;
}
/*
 * In addition to the two main parsing functions, there
 * are others. A list of expressions is parsed
 * recursively.
 * <tableparsing.c>=
 */
Explist parseexplist(Parlist input, Sourceloc source) {
    if (input == NULL) {
        return NULL;
    } else {
        Exp     e  = parseexp    (input->hd, source);
        Explist es = parseexplist(input->tl, source);
        return mkEL(e, es);
    }
}
/*
 * A list of names is also parsed recursively, with
 * context information in case of an error.
 * <tableparsing.c>=
 */
static Namelist parsenamelist(Parlist ps, ParsingContext context) {
    if (ps == NULL) {
        return NULL;
    } else {
        Exp e = parseexp(ps->hd, context->source);
        if (e->alt != VAR)
            synerror(context->source,
                     "in %p, formal parameters of %n must be names, "
                     "but %p is not a name", context->par, context->name, ps->hd
                                                                              );
        return mkNL(e->u.var, parsenamelist(ps->tl, context));
    }
}
/*
 * <tableparsing.c>=
 */
void usage_error(int code, ParserResult why_bad, ParsingContext context) {
    for (struct Usage *u = usage_table; u->expected != NULL; u++)
        if (code == u->code) {
            const char *message;
            switch (why_bad) {
            case INPUT_EXHAUSTED:
                message = "too few components in %p; expected %s";
                break;
            case INPUT_LEFTOVER:
                message = "too many components in %p; expected %s";
                break;
            default:
                message = "badly formed input %p; expected %s";
                break;
            }
            synerror(context->source, message, context->par, u->expected);
        }
    synerror(context->source, "something went wrong parsing %p", context->par);
}
/*
 * Finally, if a name was expected but we saw something
 * else instead, the parser calls [[name_error]]. The
 * error message says more about what went wrong and
 * what the context is. To make extending [[name_error]]
 * as easy as possible, I first convert the offending
 * name to an integer code, so that the proper code can
 * be chosen using a [[switch]] statement.
 * <tableparsing.c>=
 */
void *name_error(Par bad, struct ParsingContext *c) {
    switch (code_of_name(c->name)) {
    case ADEF(VAL):
        synerror(c->source, "in %p, expected (val x e), but %p is not a name",
                 c->par, bad);
    case ADEF(DEFINE):
        synerror(c->source,
                   "in %p, expected (define f (x ...) e), but %p is not a name",
                 c->par, bad);
    case ANXDEF(USE):
        synerror(c->source,
                     "in %p, expected (use filename), but %p is not a filename",
                 c->par, bad);
    case SET:
        synerror(c->source, "in %p, expected (set x e), but %p is not a name",
                                                                                
                 c->par, bad);
    case APPLY:
        synerror(c->source,
                    "in %p, expected (function-name ...), but %p is not a name",
                 c->par, bad);
    default:
        synerror(c->source, "in %p, expected a name, but %p is not a name", 
                 c->par, bad);
    }
    return NULL; // not reached
}
/*
 * To discover the proper code, function
 * [[code_of_name]] does a reverse lookup in
 * [[exptable]] and [[xdeftable]].
 * <tableparsing.c>=
 */
int code_of_name(Name n) {
    struct ParserRow *entry;
    for (entry = exptable; entry->keyword != NULL; entry++)
        if (n == strtoname(entry->keyword))
            return entry->code;
    if (n == NULL)
        return entry->code;
    for (entry = xdeftable; entry->keyword != NULL; entry++)
        if (n == strtoname(entry->keyword))
            return entry->code;
    assert(0);
}

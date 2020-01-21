#include "all.h"
/* parse.c 1186a */
struct Usage usage_table[] = {
    { ADEF(VAL),           "(val x e)" },
    { ADEF(DEFINE),        "(define fun (formals) body)" },
    { ANXDEF(USE),         "(use filename)" },
    { ATEST(CHECK_EXPECT), "(check-expect exp-to-run exp-expected)" },
    { ATEST(CHECK_ASSERT), "(check-assert exp)" },
    { ATEST(CHECK_ERROR),  "(check-error exp)" },

    { SET,     "(set x e)" },
    { IFX,     "(if cond true false)" },
    { WHILEX,  "(while cond body)" },
    { BEGIN,   "(begin exp ... exp)" },
    { LAMBDAX, "(lambda (formals) body)" },

    { ALET(LET),     "(let ((var exp) ...) body)" },
    { ALET(LETSTAR), "(let* ((var exp) ...) body)" },
    { ALET(LETREC),  "(letrec ((var exp) ...) body)" },
    /* \uscheme\ [[usage_table]] entries added in exercises 1188d */
    /* add expected usage for each new syntactic form */
    /* \uscheme\ [[usage_table]] entries added in exercises 1214g */
    { ANEXP(BREAKX),     "(break)" },
    { ANEXP(CONTINUEX),  "(continue)" },
    { ANEXP(RETURNX),    "(return exp)" },
    { ANEXP(THROW),      "(throw exp)" },
    { ANEXP(TRY_CATCH),  "(try-catch body handler)" },
    { -1, NULL }
};
/* parse.c 1186c */
static ShiftFun quoteshifts[] = { sSexp,                 stop };
static ShiftFun setshifts[]   = { sName, sExp,           stop };
static ShiftFun ifshifts[]    = { sExp, sExp, sExp,      stop };
static ShiftFun whileshifts[] = { sExp, sExp,            stop };
static ShiftFun beginshifts[] = { sExps,                 stop };
static ShiftFun letshifts[]   = { sBindings, sExp,       stop };
static ShiftFun lambdashifts[]= { sNamelist, sExp,       stop };
static ShiftFun applyshifts[] = { sExp, sExps,           stop };
/* arrays of shift functions added to \uscheme\ in exercises 1187e */
/* define arrays of shift functions as needed for [[exptable]] rows */
/* arrays of shift functions added to \uscheme\ in exercises 1214d */
ShiftFun breakshifts[]  = { stop };
ShiftFun returnshifts[] = { sExp, stop };
ShiftFun tcshifts[]     = { sExp, sExp, stop };

struct ParserRow exptable[] = {
  { "set",    ANEXP(SET),     setshifts },
  { "if",     ANEXP(IFX),     ifshifts },
  { "while",  ANEXP(WHILEX),  whileshifts },
  { "begin",  ANEXP(BEGIN),   beginshifts },
  { "let",    ALET(LET),      letshifts },
  { "let*",   ALET(LETSTAR),  letshifts },
  { "letrec", ALET(LETREC),   letshifts },
  { "lambda", ANEXP(LAMBDAX), lambdashifts },
  { "quote",  ANEXP(LITERAL), quoteshifts }, 
  /* rows added to \uscheme's [[exptable]] in exercises 1188a */
  /* add a row for each new syntactic form of Exp */
  /* rows added to \uscheme's [[exptable]] in exercises 1214e */
  { "break",     BREAKX,    breakshifts },
  { "continue",  CONTINUEX, breakshifts },
  { "return",    RETURNX,   returnshifts },
  { "throw",     THROW,     returnshifts },
  { "try-catch", TRY_CATCH, tcshifts },
  { NULL,     ANEXP(APPLY),   applyshifts }  // must come last
};
/* parse.c 1187a */
bool read_tick_as_quote = true;
/* parse.c 1187b */
Exp reduce_to_exp(int code, struct Component *comps) {
    switch(code) {
    case ANEXP(SET):     return mkSet(comps[0].name, comps[1].exp);
    case ANEXP(IFX):     return mkIfx(comps[0].exp, comps[1].exp, comps[2].exp);
    case ANEXP(WHILEX):  return mkWhilex(comps[0].exp, comps[1].exp);
    case ANEXP(BEGIN):   return mkBegin(comps[0].exps);
    case ALET(LET):
    case ALET(LETSTAR):
    case ALET(LETREC):   return mkLetx(code+LET-ALET(LET), 
                                       comps[0].names, comps[0].exps, comps[1].
                                                                           exp);
    case ANEXP(LAMBDAX): return mkLambdax(mkLambda(comps[0].names, comps[1].exp)
                                                                              );
    case ANEXP(APPLY):   return mkApply(comps[0].exp, comps[1].exps);
    case ANEXP(LITERAL): return mkLiteral(comps[0].value);
    /* cases for \uscheme's [[reduce_to_exp]] added in exercises 1188b */
    /* add a case for each new syntactic form of Exp */
    /* cases for \uscheme's [[reduce_to_exp]] added in exercises 1214f */
    case ANEXP(BREAKX):    return mkBreakx();
    case ANEXP(CONTINUEX): return mkContinuex();
    case ANEXP(RETURNX):   return mkReturnx(comps[0].exp);
    case ANEXP(THROW):     return mkThrow(comps[0].exp);
    case ANEXP(TRY_CATCH): return mkTryCatch(comps[0].exp, comps[1].exp);
    }
    assert(0);
}
/* parse.c 1187c */
XDef reduce_to_xdef(int code, struct Component *out) {
    switch(code) {
    case ADEF(VAL):    return mkDef(mkVal(out[0].name, out[1].exp));
    case ADEF(DEFINE): return mkDef(mkDefine(out[0].name,
                                             mkLambda(out[1].names, out[2].exp))
                                                                              );
    case ANXDEF(USE):  return mkUse(out[0].name);
    case ATEST(CHECK_EXPECT): 
                       return mkTest(mkCheckExpect(out[0].exp, out[1].exp));
    case ATEST(CHECK_ASSERT): 
                       return mkTest(mkCheckAssert(out[0].exp));
    case ATEST(CHECK_ERROR): 
                       return mkTest(mkCheckError(out[0].exp));
    case ADEF(EXP):    return mkDef(mkExp(out[0].exp));
    /* cases for \uscheme's [[reduce_to_xdef]] added in exercises 1188c */
    /* add a case for each new syntactic form of definition */
    default:           assert(0);  // incorrectly configured parser
    }
}
/* parse.c 1188e */
ParserResult sSexp(ParserState s) {
    if (s->input == NULL) {
        return INPUT_EXHAUSTED;
    } else {
        Par p = s->input->hd;
        halfshift(s);
        s->components[s->nparsed++].value = parsesx(p, s->context.source);
        return PARSED;
    }
}
/* parse.c 1188f */
ParserResult sBindings(ParserState s) {
    if (s->input == NULL) {
        return INPUT_EXHAUSTED;
    } else {
        Par p = s->input->hd;
        switch (p->alt) {
        case ATOM:
            usage_error(code_of_name(s->context.name), BAD_INPUT, &s->context);
            return BAD_INPUT; // not reached
        case LIST:
            halfshift(s);
            s->components[s->nparsed++] = parseletbindings(&s->context, p->
                                                                        u.list);
            return PARSED;
        }
        assert(0);
    }
}
/* parse.c 1189b */
Value parsesx(Par p, Sourceloc source) {
    switch (p->alt) {
    case ATOM:
        /* return [[p->u.atom]] interpreted as an S-expression 1189c */
        {
            Name n        = p->u.atom;
            const char *s = nametostr(n);

            char *t;                        // first nondigit in s
            long l = strtol(s, &t, 10);     // value of digits in s, if any
            if (*t == '\0' && *s != '\0')   // s is all digits
                return mkNum(l);
            else if (strcmp(s, "#t") == 0)
                return truev;
            else if (strcmp(s, "#f") == 0)
                return falsev;
            else if (strcmp(s, ".") == 0)
                synerror(source,
                    "this interpreter cannot handle . in quoted S-expressions");
            else
                return mkSym(n);
        }
    case LIST:
        /* return [[p->u.list]] interpreted as an S-expression 1189d */
        if (p->u.list == NULL)
            return mkNil();
        else
            return cons(parsesx(p->u.list->hd, source),
                        parsesx(mkList(p->u.list->tl), source));
    }
    assert(0);
}
/* parse.c 1190a */
struct Component parseletbindings(ParsingContext context, Parlist input) {
    if (input == NULL) {
        struct Component output = { .names = NULL, .exps = NULL };
        return output;
    } else if (input->hd->alt == ATOM) {
        synerror(context->source,
                 "in %p, expected (... (x e) ...) in bindings, but found %p",
                 context->par, input->hd);
        assert(0);  // not reached
    } else {
        /* state and row are set up to parse one binding */
        struct ParserState s = mkParserState(input->hd, context->source);
        s.context = *context;
        static ShiftFun bindingshifts[] = { sName, sExp, stop };
        struct ParserRow row = { .code   = code_of_name(context->name)
                               , .shifts = bindingshifts
                               };
        rowparse(&row, &s);

        /* now parse the remaining bindings, then add the first at the front */
        struct Component output = parseletbindings(context, input->tl);
        output.names = mkNL(s.components[0].name, output.names);
        output.exps  = mkEL(s.components[1].exp,  output.exps);
        return output;
    }
}
/* parse.c 1190b */
Exp exp_of_atom (Name n) {
    if (n == strtoname("#t"))
        return mkLiteral(truev);
    else if (n == strtoname("#f"))
        return mkLiteral(falsev);

    const char *s = nametostr(n);
    char *t;                      // first nondigit in s, if any
    long l = strtol(s, &t, 10);   // number represented by s, if any
    if (*t == '\0' && *s != '\0') // all the characters in s are digits base 10
        return mkLiteral(mkNum(l));
    else
        return mkVar(n);
}
/* parse.c 1200b */
void check_exp_duplicates(Sourceloc source, Exp e) {
    switch (e->alt) {
    case LAMBDAX:
        if (duplicatename(e->u.lambdax.formals) != NULL)
            synerror(source, "formal parameter %n appears twice in lambda",
                     duplicatename(e->u.lambdax.formals));
        return;
    case LETX:
        if (e->u.letx.let != LETSTAR && duplicatename(e->u.letx.xs) != NULL)
            synerror(source, "bound name %n appears twice in %s",
                     duplicatename(e->u.letx.xs),
                     e->u.letx.let == LET ? "let" : "letrec");
        return;
    default:
        return;
    }
}

void check_def_duplicates(Sourceloc source, Def d) {
    if (d->alt == DEFINE && duplicatename(d->u.define.lambda.formals) != NULL)
        synerror(source,
                 "formal parameter %n appears twice in define",
                 duplicatename(d->u.define.lambda.formals));
}
/* parse.c 1201b */
Name namecat(Name n1, Name n2) {
    const char *s1 = nametostr(n1);
    const char *s2 = nametostr(n2);
    char *buf = malloc(strlen(s1) + strlen(s2) + 1);
    assert(buf);
    sprintf(buf, "%s%s", s1, s2);
    Name answer = strtoname(buf);
    free(buf);
    return answer;
}
/* parse.c 188 */
Exp desugarLetStar(Namelist xs, Explist es, Exp body) {
    if (xs == NULL || es == NULL) {
        assert(xs == NULL && es == NULL);
        return body;
    } else {
        return desugarLet(mkNL(xs->hd, NULL), mkEL(es->hd, NULL),
                          desugarLetStar(xs->tl, es->tl, body));
    }
}
/* parse.c ((prototype)) 232 */
Exp desugarLet(Namelist xs, Explist es, Exp body) {
    /* you replace the body of this function */
    runerror("desugaring for LET never got implemented");
    (void)xs; (void)es; (void)body;   // avoid warnings (OMIT)
    return NULL;
}

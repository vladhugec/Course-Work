/*
 * Interfaces
 * 
 * As in micro-Scheme, we gather all the interfaces into
 * a single C header file.
 * <{\Tt all.h} for \uschemeplus>=
 */
#include <assert.h>
#include <ctype.h>
#include <inttypes.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/*
 * <type definitions for \uschemeplus>=
 */
typedef struct Lambda Lambda; 
typedef struct Value Value;
typedef enum {
    NIL, BOOLV, NUM, SYM, PAIR, CLOSURE, PRIMITIVE, FORWARD, INVALID
} Valuealt;

/*
 * <type definitions for \uschemeplus>=
 */
typedef struct Def *Def;
typedef enum { VAL, EXP, DEFINE, DEFS } Defalt; 
typedef struct XDef *XDef;
typedef enum { DEF, USE, TEST } XDefalt; 
typedef struct UnitTest *UnitTest;
typedef enum { CHECK_EXPECT, CHECK_ASSERT, CHECK_ERROR } UnitTestalt;

typedef struct Exp *Exp;
typedef enum {
    LITERAL, VAR, SET, IFX, WHILEX, BEGIN, LETX, LAMBDAX, APPLY, BREAKX,
    CONTINUEX, RETURNX, THROW, TRY_CATCH, HOLE, WHILE_RUNNING_BODY,
    CALLENV, LETXENV
} Expalt;

/*
 * The stack
 * 
 * The stack is a stack of evaluation contexts, each of
 * which is represented by a [[Frame]].
 * <type definitions for \uschemeplus>=
 */
typedef struct Stack *Stack;
typedef struct Frame Frame;
/*
 * The representation of [[Frame]] is exposed. A frame
 * always includes an evaluation context, which is
 * represented by an expression. If the context is a
 * function application, then we extend the frame by
 * storing the original syntax of the application. That
 * syntax is used in error messages when, e.g., a
 * program tries to apply a value that is not a
 * function. [*]
 */

/*
 * The roots are
 * 
 *  1. The global variables, which include both the user
 *  program's variables (the \uschemeplus global
 *  environment) and any global variables internal to
 *  the interpreter (the pending unit tests)
 *  2. Local variables and actual parameters of any \
 *  uschemeplus function, all of which are found on
 *  the stack of evaluation contexts
 *  3. Local variables and actual parameters of any C
 *  function that calls [[allocate]] or a function
 *  that could allocate (such as [[bindalloc]] or
 *  [[bindalloclist]])
 * 
 * The roots are represented as follows:
 * <type definitions for \uschemeplus>=
 */
typedef Value *Register;  /* pointer to a local variable or a parameter
                             of a C function that could allocate */
typedef struct Registerlist *Registerlist;   /* list of Register */
typedef struct UnitTestlistlist *UnitTestlistlist;
                                               /* list of UnitTestlist (list) */
/*
 * <type definitions for \uscheme>=
 */
typedef enum Letkeyword { LET, LETSTAR, LETREC } Letkeyword;
typedef struct UnitTestlist  *UnitTestlist;  // list of UnitTest 
typedef struct Explist  *Explist;            // list of Exp 
typedef struct Deflist  *Deflist;            // list of Def    /*OMIT*/
/*
 * <type definitions for \uscheme>=
 */
typedef struct Valuelist *Valuelist;     // list of Value
typedef Value (Primitive)(Exp e, int tag, Valuelist vs);
/*
 * Why so many arguments to [[Primitive]]? Shouldn't a
 * primitive function just accept a [[Valuelist]] and
 * return a [[Value]]? No. If a primitive fails, it
 * needs to show where the error occurred; the [[Exp e]]
 * is where. And by using an integer [[tag]], a single
 * [[Primitive]] function can implement multiple
 * micro-Scheme primitives. The C function that
 * implements micro-Scheme's arithmetic primitives, for
 * example, makes it easy for those primitives to share
 * the code that ensures both arguments are numbers.
 * Implementations of the primitives appear in Section 
 * [->].
 */

/*
 * ━━━━━━━━━━━━━━━━━━━━━━━━━�
�━━━━━━━━━━━━━━━━━━━━━━━━━━�
                                                                              ��
 * \advanceby 1pt \newskip\myskip \myskip=6pt
 * 
 *    \toprule    Concept     Interpreter
 *    Semantics
 *   \midrule d   Definition  Def, XDef, or UnitTest (\cpageref
 *                            scheme.type.Def,scheme.type.XDef,
                                                           scheme.type.UnitTest)
 *        e       Expression  \scmtypeExp
 *      \aloc     Location    \monoValue *
 *        x       Name        \iitypeName
 *        v       Value       \scmtypeValue
 *       rho      Environment \scmtypeEnv
 *      sigma     Store       Machine memory (the C heap)
 *   [\myskip] \  Expression      \monoboxeval(e, rho) = v, \break with sigma
 *   evale ==>\   evaluation      updated to sigma' \scmfunpageeval
 *   evalr['] v
 *  <d,rho,sigma> Definition      \monoboxevaldef(d, rho, echo) = rho', \break
                                                                            with
 *   --><rho',    evaluation      sigma updated to sigma' \scmfunpageevaldef
 *     sigma'>
 *   [\myskip] x  Definedness \monofind(x, rho) != NULL (\
                                                        cpagerefscheme.find.int)
 *   in dom rho
 *     rho(x)     Location    \monofind(x, rho) (\cpagerefscheme.find.int)
 *                lookup
 *  sigma(rho(x)) Value       \mono*find(x, rho) (\cpagerefscheme.find.int)
 *                lookup
 *   rho{x |->\   Binding     \scmfunbindalloc
 *      aloc}
 *   \aloc\notin  Allocation  \scmfunbindalloc
 *    dom sigma
 *  sigma{\aloc|  Store       \mono*\aloc = v
 *      ->v}      update
 *    [\myskip]   Stream of   \iitypeXDefstream
 *                definitions
 *                Interactive \scmfunreadevalprint
 *                evaluation
 *   \bottomrule
 * 
 * Correspondence between micro-Scheme semantics and
 * code [*]
 * ━━━━━━━━━━━━━━━━━━━━━━━━━�
�━━━━━━━━━━━━━━━━━━━━━━━━━━�
                                                                              ��
 * 
 * The Environment and the Store
 * 
 * [*] In the operational semantics, the store sigma
 * models the machine's memory. It should come as no
 * surprise, then, that we use the machine's memory to
 * represent the store, and we use C pointers (of type
 * [[Value *]]) to represent locations. An environment
 * [[Env]] maps names to pointers; find(x, rho) returns
 * rho(x) if x in dom rho; otherwise it returns
 * [[NULL]].\scmlabelEnv\scmflabelfind[*]
 * <type definitions for \uscheme>=
 */
typedef struct Env *Env;
/*
 * To define the primitives and associate each one with
 * its tag and function, I resort to macro madness. Each
 * primitive appears in file prim.h as a macro [[xx(]]
 * name[[, ]]tag[[, ]]function[[)]]. I use the same
 * macros with two different definitions of [[xx]]: one
 * to create an enumeration with distinct tags, and one
 * to install the primitives in an empty environment.
 * There are other initialization techniques that don't
 * require macros, but this technique ensures there is a
 * single point of truth about the primitives (that
 * point of truth is the file prim.h), which helps
 * guarantee that the enumeration type is consistent
 * with the initialization code.
 * <type definitions for \uscheme>=
 */
enum {
  #define xx(NAME, TAG, FUNCTION) TAG,
  #include "prim.h"
  #undef xx
  UNUSED_TAG
};
/*
 * For real implementations, it is convenient to build
 * names from strings. Unlike C strings, names are
 * immutable, and they can be compared using pointer
 * equality.\iilabelName\intlabelName
 * <shared type definitions>=
 */
typedef struct Name *Name;
typedef struct Namelist *Namelist;   // list of Name
/*
 * A source of extended definitions is called an
 * [[XDefstream]]. To obtain the next definition from
 * such a source, call [[getxdef]]. Function [[getxdef]]
 * returns either a pointer to the next definition or,
 * if the source is exhausted, the [[NULL]] pointer.
 * And if there is some problem converting input to
 * abstract syntax, [[getxdef]] may call [[synerror]] (\
 * cpagerefimpcore.synerror). \iilabelXDefstream\
 * intlabelXDefstream\intlabelgetxdef
 * <shared type definitions>=
 */
typedef struct XDefstream *XDefstream;
/*
 * <shared type definitions>=
 */
typedef enum Prompts { NO_PROMPTS, STD_PROMPTS } Prompts;
/*
 * <shared type definitions>=
 */
typedef enum Echo { NO_ECHOES, ECHOES } Echo;
/*
 * The [[synerror]] function is like [[runerror]],
 * except that before its format string, it takes an
 * argument of type [[Sourceloc]], which tracks the
 * source-code location being read at the time of the
 * error. The location can be printed as part of the
 * error message. The [[Sourceloc]] values are taken
 * care of by the parsing infrastructure described in \
 * crefcparse.chap, which is the place from which
 * [[synerror]] is called. [*]\intlabelsynerror\intlabel
 * Sourceloc
 * <shared type definitions>=
 */
typedef struct Sourceloc *Sourceloc;
/*
 * <shared type definitions>=
 */
typedef enum ErrorFormat { WITH_LOCATIONS, WITHOUT_LOCATIONS } ErrorFormat;
/*
 * I define type abbreviations for [[ParserState]] and
 * [[ParsingContext]].
 * <shared type definitions>=
 */
typedef struct ParserState *ParserState;
typedef struct ParsingContext *ParsingContext;
/*
 * Each form of component is parsed by its own shift
 * function. Why ``shift''? Think of the [[ParserState]]
 * as the state of a machine that puts components on the
 * left and the input on the right. A shift function
 * removes initial inputs and appends to components;
 * this action ``shifts'' information from right to
 * left. Shifting plays a role in several varieties of
 * parsing technology.
 * 
 * A shift function normally updates the inputs and
 * components in the parser state. A shift function also
 * returns one of these results:
 * <shared type definitions>=
 */
typedef enum ParserResult {
  PARSED,            /* some input was parsed without any errors */
  INPUT_EXHAUSTED,   /* there aren't enough inputs */
  INPUT_LEFTOVER,    /* there are too many inputs */
  BAD_INPUT,         /* an input wasn't what it should have been */
  STOP_PARSING       /* all the inputs have been parsed; it's time to stop */
} ParserResult;
/*
 * When a shift function runs out of input or sees input
 * left over, it returns [[INPUT_EXHAUSTED]] or
 * [[INPUT_LEFTOVER]]. Returning one of these error
 * results is better than simply calling [[synerror]],
 * because the calling function knows what row it's
 * trying to parse and so can issue a better error
 * message. But for other error conditions, shift
 * functions can call [[synerror]] directly.
 */

/*
 * The C type of a shift function is [[ShiftFun]].
 * <shared type definitions>=
 */
typedef ParserResult (*ShiftFun)(ParserState);
/*
 * <shared type definitions>=
 */
typedef struct ParserRow *ParserTable;
/*
 * ━━━━━━━━━━━━━━━━━━━━━━━━━�
�━━━━━━━━━━━━━━━━━━━━━━━━━━�
                                                                              ��
 * \advance\nwdefspaceby 0.05
 * 
 *  ┌────────────────────────�
         ��──────────────────────┐
 *  │|height 12pt width 0pt Here are integer codes  │
 *  │for all the syntactic forms that are suggested │
 *  │to be implemented as syntactic sugar.          │
 *  └────────────────────────�
         ��──────────────────────┘
 * <shared type definitions>=
 */
enum Sugar {
  CAND, COR,    /* short-circuit Boolean operators */

  WHILESTAR, DO_WHILE, FOR,     /* bonus loop forms */

  WHEN, UNLESS,       /* single-sided conditionals */

  RECORD,             /* record-type definition */

  COND                /* McCarthy's conditional from Lisp */

};
/*
 * <shared type definitions>=
 */
typedef struct Par *Par;
typedef enum { ATOM, LIST } Paralt; 
/*
 * <shared type definitions>=
 */
typedef struct Linestream *Linestream;
/*
 * <shared type definitions>=
 */
typedef struct Parlist *Parlist; /* list of Par */
/*
 * This simple structure reflects the concrete syntax of
 * Impcore, micro-Scheme, and the other bridge
 * languages. It's simple because I've stolen the simple
 * concrete syntax that John McCarthy developed for \
 * lisp. Simple syntax is represented by a simple data
 * structure.
 */

/*
 * Interface to Parstream
 * 
 * A [[Parstream]] is an abstract type. \intlabel
 * Parstream
 * <shared type definitions>=
 */
typedef struct Parstream *Parstream;
/*
 * Buffering characters
 * 
 * A classic abstraction: the resizeable buffer.
 * Function [[bprint]] writes to a buffer.
 * <shared type definitions>=
 */
typedef struct Printbuf *Printbuf;
/*
 * <shared type definitions>=
 */
/*
 * The type [[va_list_box]] is almost, but not quite, a
 * standard C type for holding a variable number of
 * arguments. A function that can accept a variable
 * number of arguments is called variadic, and according
 * to the C standard, the arguments of a variadic
 * function are stored in an object of type [[va_list]],
 * which is defined in the standard library in header
 * file stdarg.h. (If you are not accustomed to variadic
 * functions and [[stdarg.h]], you may wish to consult
 * Sections 7.2 and 7.3 of \citeNNkernighan:c:2.)
 * So what is [[va_list_box]]? It's a workaround for a
 * bug that afflicts some versions of the GNU C compiler
 * on 64-bit hardware. These compilers fail when values
 * of type [[va_list]] are passed as arguments. [Library
 * functions such as [[vfprintf]] itself are
 * grandfathered; only users cannot write functions that
 * take [[va_list]] arguments. Feh.] \codeindex
 * va-list-box@va_list_box A workaround for this problem
 * is to place the [[va_list]] in a structure and pass a
 * pointer to the structure. That structure is called
 * [[va_list_box]], and it is defined here:
 * <definition of [[va_list_box]]>=
 */
typedef struct va_list_box {
  va_list ap;
} va_list_box;
typedef void Printer(Printbuf output, va_list_box *args);
/*
 * The heavy lifting is done by function
 * [[test_result]], which returns a value of type
 * [[TestResult]]. \iilabelTestResult
 * <shared type definitions>=
 */
typedef enum TestResult { TEST_PASSED, TEST_FAILED } TestResult;

/*
 * <structure definitions for \uschemeplus>=
 */
struct Lambda { Namelist formals; Exp body; }; 
struct Value {
    Valuealt alt;
    union {
        bool boolv;
        int num;
        Name sym;
        struct { Value *car; Value *cdr; } pair;
        struct { Lambda lambda; Env env; } closure;
        struct { int tag; Primitive *function; } primitive;
        Value *forward;
        const char *invalid;
    } u;
};

/*
 * <structure definitions for \uschemeplus>=
 */
struct Def {
    Defalt alt;
    union {
        struct { Name name; Exp exp; } val;
        Exp exp;
        struct { Name name; Lambda lambda; } define;
        Deflist defs;
    } u;
};

struct XDef {
    XDefalt alt; union { Def def; Name use; UnitTest test; } u;
};

struct UnitTest {
    UnitTestalt alt;
    union {
        struct { Exp check; Exp expect; } check_expect;
        Exp check_assert;
        Exp check_error;
    } u;
};

struct Exp {
    Expalt alt;
    union {
        Value literal;
        Name var;
        struct { Name name; Exp exp; } set;
        struct { Exp cond; Exp truex; Exp falsex; } ifx;
        struct { Exp cond; Exp body; } whilex;
        Explist begin;
        struct { Letkeyword let; Namelist xs; Explist es; Exp body; } letx;
        Lambda lambdax;
        struct { Exp fn; Explist actuals; } apply;
        Exp returnx;
        Exp throw;
        struct { Exp body; Exp handler; } try_catch;
        Env callenv;
        Env letxenv;
    } u;
};

/*
 * <structure definitions for \uschemeplus>=
 */
struct Parlist {
   Par hd;
   struct Parlist *tl;
};

struct Namelist {
   Name hd;
   struct Namelist *tl;
};

struct UnitTestlist {
   UnitTest hd;
   struct UnitTestlist *tl;
};

struct Explist {
   Exp hd;
   struct Explist *tl;
};

struct Deflist {
   Def    /*OMIT*/ hd;
   struct Deflist *tl;
};

struct Valuelist {
   Value hd;
   struct Valuelist *tl;
};

struct Registerlist {
   Register hd;
   struct Registerlist *tl;
};

struct UnitTestlistlist {
   UnitTestlist hd;
   struct UnitTestlistlist *tl;
};

/*
 * <structure definitions for \uschemeplus>=
 */
struct Frame {
    struct Exp context;     // mutated in place during evaluation
    Exp syntax;             // when not NULL, kept pristine for error messages
};
/*
 * To make [[visitenv]] to work, we have to expose the
 * representation of environments. (In Chapter [->],
 * this representation is private.)
 * <structure definitions for \uschemeplus>=
 */
struct Env {
    Name name;
    Value *loc;
    Env tl;
};
/*
 * Parsing micro-Scheme code
 * 
 * Parsing tables and reduce functions
 * 
 * Here are all the components that go into
 * micro-Scheme's abstract syntax. They include all the
 * components used to parse Impcore, plus a [[Value]]
 * component that is used when parsing a quoted
 * S-expression.
 * <structure definitions for \uscheme>=
 */
struct Component {
    Exp exp;
    Explist exps;
    Name name;
    Namelist names;
    Value value;
    /*
     * Here's how the parser might be extended [*]
     * <fields of \uscheme\ [[Component]] added in exercises>=
     */
    /* if implementing COND, add a question-answer field here */
    /*
     * \lisp's original conditional
     * 
     * <fields of \uscheme\ [[Component]] added in exercises>=
     */
    // for COND:
    struct qa_pairs { Explist questions; Explist answers; } qa_pairs;
};
/*
 * Parser state and shift functions
 * 
 * [*] A table-driven parser converts an input
 * [[Parlist]] into components. There are at most
 * [[MAXCOMPS]] components. (The value of [[MAXCOMPS]]
 * must be at least the number of children that can
 * appear in any node of any abstract-syntax tree.
 * To support \exrefpageimpcore.ex.localvars, which has
 * four components in the [[define]] form, I set
 * [[MAXCOMPS]] to 4.) Inputs and components both go
 * into a data structure. And if no programmer ever made
 * a mistake, inputs and components would be enough. But
 * because programmers do make mistakes, the data
 * structure includes additional context, which can be
 * added to an error message. The context I use includes
 * the syntax we are trying to parse, the location where
 * it came from, and if there's a keyword or function
 * name involved, what it is. \implabelSourceloc\
 * intlabelParserState\intlabelParsingContext
 * <shared structure definitions>=
 */
#define MAXCOMPS 4 /* max # of components in any syntactic form */
struct ParserState {
    int nparsed;           /* number of components parsed so far */
    struct Component components[MAXCOMPS];  /* those components */
    Parlist input;         /* the part of the input not yet parsed */

    struct ParsingContext {   /* context of this parse */
        Par par;       /* the original thing we are parsing */
        struct Sourceloc {
            int line;                /* current line number */
            const char *sourcename;  /* where the line came from */
        } *source;
        Name name;     /* a keyword, or name of a function being defined */
    } context;
};
/*
 * The important invariant of this data structure is
 * that components[i] is meaningful if and only if 0 <=i
 * < nparsed.
 */

/*
 * Representing and parsing tables and rows
 * 
 * As shown in \cref
 * cparse.fig.exptable,cparse.fig.xdeftable on \cpageref
 * cparse.fig.exptable,cparse.fig.xdeftable, a row needs
 * a keyword, a code, and a sequence of components. The
 * sequence of components is represented as an array of
 * shift functions ending in [[stop]].
 * <shared structure definitions>=
 */
struct ParserRow {
    const char *keyword;
    int code;
    ShiftFun *shifts;  /* points to array of shift functions */
};
/*
 * <shared structure definitions>=
 */
struct Par { Paralt alt; union { Name atom; Parlist list; } u; }; 
/*
 * Implementation of Linestream
 * 
 * A [[Linestream]] owns the memory used to store each
 * line. That memory is pointed to by [[buf]], and its
 * size is stored in [[bufsize]]. \implabelLinestream
 * If no line has been read, [[buf]] is [[NULL]] and
 * [[bufsize]] is zero.
 * <shared structure definitions>=
 */
struct Linestream {
    char *buf;               /* holds the last line read */
    int bufsize;                /* size of buf */

    struct Sourceloc source; /* where the last line came from */
    FILE *fin;               /* non-NULL if filelines */
    const char *s;           /* non-NULL if stringlines */
};
/*
 * The rest of the [[Linestream]] structure stores
 * mutable state characterizing the source from which
 * lines come:
 * 
 *   • The [[source]] field tracks the location of the
 *  line currently in [[buf]].
 *   • The [[fin]] field, if the stream is built from a
 *  file, contains the pointer to that file's handle.
 *  Otherwise [[fin]] is [[NULL]].
 *   • The [[s]] field, if the stream is built from a
 *  string, points to the characters of that string
 *  that have not yet been converted to lines.
 *  Otherwise [[s]] is [[NULL]].
 * 
 */


/*
 * Checking for cycles in cons
 * 
 * I've left in this early-stage debugging code, which
 * looks for a cycle after every [[cons]].
 * <function prototypes for \uschemeplus>=
 */
void cyclecheck(Value *l);
/*
 * <function prototypes for \uschemeplus>=
 */
Lambda mkLambda(Namelist formals, Exp body);
Value mkNil(void);
Value mkBoolv(bool boolv);
Value mkNum(int num);
Value mkSym(Name sym);
Value mkPair(Value *car, Value *cdr);
Value mkClosure(Lambda lambda, Env env);
Value mkPrimitive(int tag, Primitive *function);
Value mkForward(Value *forward);
Value mkInvalid(const char *invalid);
/*
 * <function prototypes for \uschemeplus>=
 */
Def mkVal(Name name, Exp exp);
Def mkExp(Exp exp);
Def mkDefine(Name name, Lambda lambda);
Def mkDefs(Deflist defs);
struct Def mkValStruct(Name name, Exp exp);
struct Def mkExpStruct(Exp exp);
struct Def mkDefineStruct(Name name, Lambda lambda);
struct Def mkDefsStruct(Deflist defs);
XDef mkDef(Def def);
XDef mkUse(Name use);
XDef mkTest(UnitTest test);
struct XDef mkDefStruct(Def def);
struct XDef mkUseStruct(Name use);
struct XDef mkTestStruct(UnitTest test);
UnitTest mkCheckExpect(Exp check, Exp expect);
UnitTest mkCheckAssert(Exp check_assert);
UnitTest mkCheckError(Exp check_error);
struct UnitTest mkCheckExpectStruct(Exp check, Exp expect);
struct UnitTest mkCheckAssertStruct(Exp check_assert);
struct UnitTest mkCheckErrorStruct(Exp check_error);
Exp mkLiteral(Value literal);
Exp mkVar(Name var);
Exp mkSet(Name name, Exp exp);
Exp mkIfx(Exp cond, Exp truex, Exp falsex);
Exp mkWhilex(Exp cond, Exp body);
Exp mkBegin(Explist begin);
Exp mkLetx(Letkeyword let, Namelist xs, Explist es, Exp body);
Exp mkLambdax(Lambda lambdax);
Exp mkApply(Exp fn, Explist actuals);
Exp mkBreakx(void);
Exp mkContinuex(void);
Exp mkReturnx(Exp returnx);
Exp mkThrow(Exp throw);
Exp mkTryCatch(Exp body, Exp handler);
Exp mkHole(void);
Exp mkWhileRunningBody(void);
Exp mkCallenv(Env callenv);
Exp mkLetxenv(Env letxenv);
struct Exp mkLiteralStruct(Value literal);
struct Exp mkVarStruct(Name var);
struct Exp mkSetStruct(Name name, Exp exp);
struct Exp mkIfxStruct(Exp cond, Exp truex, Exp falsex);
struct Exp mkWhilexStruct(Exp cond, Exp body);
struct Exp mkBeginStruct(Explist begin);
struct Exp mkLetxStruct(Letkeyword let, Namelist xs, Explist es, Exp body);
struct Exp mkLambdaxStruct(Lambda lambdax);
struct Exp mkApplyStruct(Exp fn, Explist actuals);
struct Exp mkBreakxStruct(void);
struct Exp mkContinuexStruct(void);
struct Exp mkReturnxStruct(Exp returnx);
struct Exp mkThrowStruct(Exp throw);
struct Exp mkTryCatchStruct(Exp body, Exp handler);
struct Exp mkHoleStruct(void);
struct Exp mkWhileRunningBodyStruct(void);
struct Exp mkCallenvStruct(Env callenv);
struct Exp mkLetxenvStruct(Env letxenv);
/*
 * <function prototypes for \uschemeplus>=
 */
int     lengthPL(Parlist ps);
Par     nthPL   (Parlist ps, unsigned n);
Parlist mkPL    (Par p, Parlist ps);
Parlist popPL   (Parlist ps);
Printer printparlist;

int      lengthNL(Namelist ns);
Name     nthNL   (Namelist ns, unsigned n);
Namelist mkNL    (Name n, Namelist ns);
Namelist popNL   (Namelist ns);
Printer  printnamelist;

int          lengthUL(UnitTestlist us);
UnitTest     nthUL   (UnitTestlist us, unsigned n);
UnitTestlist mkUL    (UnitTest u, UnitTestlist us);
UnitTestlist popUL   (UnitTestlist us);
Printer      printunittestlist;

int     lengthEL(Explist es);
Exp     nthEL   (Explist es, unsigned n);
Explist mkEL    (Exp e, Explist es);
Explist popEL   (Explist es);
Printer printexplist;

int     lengthDL(Deflist ds);
Def    /*OMIT*/ nthDL   (Deflist ds, unsigned n);
Deflist mkDL    (Def    /*OMIT*/ d, Deflist ds);
Deflist popDL   (Deflist ds);
Printer printdeflist;

int       lengthVL(Valuelist vs);
Value     nthVL   (Valuelist vs, unsigned n);
Valuelist mkVL    (Value v, Valuelist vs);
Valuelist popVL   (Valuelist vs);
Printer   printvaluelist;

int          lengthRL(Registerlist rs);
Register     nthRL   (Registerlist rs, unsigned n);
Registerlist mkRL    (Register r, Registerlist rs);
Registerlist popRL   (Registerlist rs);
Printer      printregisterlist;

int              lengthULL(UnitTestlistlist uss);
UnitTestlist     nthULL   (UnitTestlistlist uss, unsigned n);
UnitTestlistlist mkULL    (UnitTestlist us, UnitTestlistlist uss);
UnitTestlistlist popULL   (UnitTestlistlist uss);
Printer          printunittestlistlist;

/*
 * Code for writing interpreters in C
 * 
 * [*][*] \invisiblelocaltableofcontents[*]
 * 
 * \crefimpcore.chap presents only those parts of the
 * Impcore interpreter that are most relevant to the
 * study of programming languages. If that code is the
 * tip of the iceberg, there's a good deal beneath the
 * surface. Much of it is interesting, some is not.
 * The parts that are generic to writing interpreters,
 * not specific to Impcore, can be found here and in \
 * crefcparse.chap.
 * 
 * This appendix presents most of the implementations of
 * the interfaces shown in \crefimpcore.chap. It also
 * presents interfaces and implementations used to read
 * lines and parenthesized phrases from input.
 * Everything presented here is used not only to help
 * implement Impcore, but also to help implement
 * micro-Scheme and \uschemeplus in \cref
 * scheme.chap,schemes.chap,gc.chap. And almost
 * everything used to implement Impcore is presented
 * here—with two exceptions.
 * 
 *   • The parsing code used to convert input to
 *  abstract syntax uses a form of shift-reduce
 *  parsing. While the technology is old and is well
 *  understood, when compared to other techniques
 *  I use, it requires elaborate code and complicated
 *  data structures. This complexity is justified
 *  because it makes it easy for you to extend any of
 *  the parsers, but because the code is complex,
 *  it is best presented on its own. The parsing
 *  infrastructure is shown in \crefcparse.chap,
 *  along with its application to the Impcore parser.
 *   • There are a few parts of the Impcore interpreter,
 *  like the functions that print abstract syntax, or
 *  the implementation of function environments,
 *  which are not reused in any other interpreter.
 *  These parts are relegated to \crefimpcorea.chap.
 * 
 * All the infrastructure presented here is reusable.
 * If you choose to reuse it to build your own
 * interpreters, your interpreters will be simple and
 * easy to modify, but not fast.
 * 
 * The code in this appendix is organized to parallel
 * the presentation in \crefimpcore.chap. A detailed
 * overview, which connects concepts, types, functions,
 * interfaces, and implementations, is shown in \
 * crefpagecinterps.code-by-concept. A higher-level
 * overview, which shows what information is presented
 * in each chapter or appendix, is shown in \crefpage
 * cinterps.code-by-chapter.
 * 
 * ━━━━━━━━━━━━━━━━━━━━━━━━━�
�━━━━━━━━━━━━━━━━━━━━━━━━━━�
                                                                              ��
 * --- Types & Functions --- Interface ---
 * Implementation
 * ━━━━━━━━━━━━━━━━━━━━━━━━━�
�━━━━━━━━━━━━━━━━━━━━━━━━━━�
                                                                              ��
 * \changecrefs\smallcref
 * 
 * @\extracolsep\
 * fill >\tolerance=
 * 1600p1.3in >\
 * tolerance=
 * 1600p1.5in >\
 * tolerance=
 * 1600p1.0in >\
 * tolerance=        [[Exp]], [[Def]]    \Crefpimpcore.Exp.int,impcore.Def.int
                                                                             \na
 * 1600p0.9in @\
 * extracolsep0pt \
 * theaderAbstract
 * syntax, names,
 * values,
 * functions, and
 * environments
 * Abstract syntax
 * Abstract syntax   [[XDef]],           \Crefpimpcore.XDef.int,
                       impcore.UnitTest.int                                  \na
 *                [[UnitTest]]
 * Names             [[Name]]            \CrefpName.int
                                                                  \CrefpName.imp
 * Value             [[Value]]           \Crefpimpcore.Value.int
                                                                             \na
 * Function          [[Fun]],            \Crefpimpcore.type.Fun,
                       impcore.type.Userfun                                  \na
 *                [[Userfun]]
 * Environment       [[Valenv]]          \Crefpimpcore.Valenv.int
                                                        \Crefpimpcore.Valenv.imp
 * Environment       [[Funenv]]          \Crefpimpcore.Funenv.int
                                                        \Crefpimpcore.Funenv.imp
 * \theader
 * Evaluation        [[eval]]            \Crefpimpcore.eval.int
                                                          \Crefpimpcore.eval.imp
 * Evaluator
 * Evaluator         [[evaldef]]         \Crefpimpcore.evaldef.int
                                                       \Crefpimpcore.evaldef.imp
 * Evaluator         [[readevalprint]]   \Crefpimpcore.readevalprint.int
                                                 \Crefpimpcore.readevalprint.imp
 * Interaction       [[Echo]]            \CrefpEcho.int
                                                                             \na
 * \theaderStreams   [[XDefstream]],
 * and lists         [[filexdefs]],      \Crefp
                                                                          \Crefp
 * Extended          [[stringxdefs]],    XDefstream.int,filexdefs.int,
   stringxdefs.int,getxdef.int,xdefstream.int      XDefstream.imp,filexdefs.imp,
                                      stringxdefs.imp,getxdef.imp,xdefstream.imp
 * definitions       [[getxdef]]
 *                [[xdefstream]]
 * Parenthesized     [[Par]],
 * phrases           [[Parstream]],      \CrefpPar.int,Parstream.int,getpar.int
                                                  \CrefpParstream.imp,getpar.imp
 *                [[getpar]]
 * Lines             [[Linestream]],     \CrefpLinestream.int,getline_.int
                                               \CrefpLinestream.imp,getline_.imp
 *                [[getline_]]
 * Lists of [[Exp]]
 * s, [[Value]]s,    (not shown)         \Crefpimpcore.lists.int
                                                       (generated automatically)
 * and others
 * \theaderPrinting
 * and error         [[print]],          \Crefpprint.int,fprint.int
                                                      \Crefpprint.imp,fprint.imp
 * signaling         [[fprint]]
 * Printers
 * Error-signaling   [[synerror]],       \Crefpsynerror.int,runerror.int
                                                 \Crefpsynerror.imp,runerror.imp
 * printers          [[runerror]]
 * Error helpers     [[checkargc]],      \Crefpcheckargc.int,duplicatename.int
                                           \Crefpcheckargc.imp,duplicatename.imp
 *                [[duplicatename]]
 * Printer extension [[installprinter]], \Crefpinstallprinter.int,Printer.int
                                                        \Crefpinstallprinter.imp
 *                [[Printer]]
 * Source locations  [[Sourceloc]]       \CrefpSourceloc.int
                                                                             \na
 * Error formats     [[ErrorFormat]]     \CrefpErrorFormat.int
                                                                             \na
 * Error modes       [[ErrorMode]],      \CrefpErrorMode.int,set_error_mode.int
                                                        \Crefpset_error_mode.imp
 *                [[set_error_mode]]
 * 
 * Key ideas, their interfaces, and their
 * implementations (excludes parsing)
 * 
 * \promissoryWhat about [[main]]? And unit testing?
 * 
 * [*]
 * ━━━━━━━━━━━━━━━━━━━━━━━━━�
�━━━━━━━━━━━━━━━━━━━━━━━━━━�
                                                                              ��
 * \changecrefs\revertcrefSectionSections \revertcref
 * AppendixAppendicesappendix
 * 
 * ━━━━━━━━━━━━━━━━━━━━━━━━━�
�━━━━━━━━━━━━━━━━━━━━━━━━━━�
                                                                              ��
 * --- Where --- What
 * ━━━━━━━━━━━━━━━━━━━━━━━━━�
�━━━━━━━━━━━━━━━━━━━━━━━━━━�
                                                                              ��
 * 
 *   \theader\Cref \moduleall.h         Representations
 *   impcore.chap:                      of [[Exp]],
 *  central ideas and                      [[Def]],
 *   fundamental data                      [[XDef]],
 *      structures                      [[Value]], and
 *                                      lists
 *                 \moduleenv.c         Operations on
 *              53                      value
 *                                      environments
 *                 \moduleeval.c        Evaluation:
 *             369                      [[eval]],
 *                                      [[evaldef]],
 *                                      [[readevalprint]]
 *                 \moduleimpcore.c     The [[main]]
 *              68                      function
 *                                      (launches the
 *                                      interpreter)
 *                 \modulename.c        Conversion
 *              45                      between names and
 *                                      strings, used in
 *                                      many interpreters
 *   \theader\Cref \moduleerror.c       Error functions,
 *  cinterps.chap:                      formats, modes
 *  (mostly) reusable
 *   code for writing
 *  interpreters in C
 *              92
 *                 \modulelex.c         Get [[Par]] from
 *                                      string,
 *             176                      [[Linestream]]
 *                                      using [[getpar]],
 *                                      [[getparlist]]
 *              18 \moduleoverflow.c    Detect stack
 *                                      overflow
 *              67 \moduleprint.c       The extensible
 *                                      printer
 *                 \modulelinestream.c  Build
 *                                      [[Linestream]]s
 *              86                      from files or
 *                                      strings;
 *                                      [[getline_]]
 *              31 \moduletests.c       Report test
 *                                      results
 *                 \modulexdefstream.c  Functions
 *              33                      [[xdefstream]]
 *                                      and [[getxdef]]
 *   \theader\Cref \moduleparse.c       Impcore-specific
 *  cparse.chap: code                      code and parsing
 *  for parsing, both                      tables, turn
 *    reusable and                      [[Par]] into
 *     specific to                      [[Exp]] or
 *     Impcore 111                      [[XDef]]
 *                 \module              Reusable
 *                 tableparsing.c       infrastructure:
 *             347                      [[tableparse]],
 *                                      [[rowparse]],
 *                                      common shift
 *                                      functions
 *   \theader\Cref
 *  app:impcore: code
 * that is peripheral
 *   to the ideas and
 *  is specific to
 *         Impcore
 *                 \moduleenv.c         Operations on
 *              50                      function
 *                                      environments
 *                 \moduleprintfuns.c   Printing
 *                                      functions for
 *             103                      [[Value]],
 *                                      [[Exp]],
 *                                      [[XDef]], many
 *                                      others
 *                 \moduleimptests.c    Run unit tests
 *              67                      using Impcore's
 *                                      dual environments
 * 
 * The implementation of Impcore, as organized into
 * chapters, appendices, and files [*] [*]
 * 
 * ━━━━━━━━━━━━━━━━━━━━━━━━━�
�━━━━━━━━━━━━━━━━━━━━━━━━━━�
                                                                              ��
 * 
 * Streams
 * 
 * The evaluator works by repeatedly calling [[getxdef]]
 * on a stream of [[XDef]]s. Behind the scenes, there's
 * a lot going on:
 * 
 *   • Each [[XDef]] is produced from a parenthesized
 *  phrase, like \monobox(val n 0) or \monobox(define
 *  id (x) x). A parenthesized phrase, which in the
 *  code is called [[Par]], is simply a fragment of
 *  the input in which parentheses are balanced;
 *  converting a parenthesized phrase to an
 *  expression or an extended definition is the job
 *  of the parser presented in \crefcparse.chap.
 *  Producing parenthesized phrases, however, is done
 *  here; function [[parstream]] produces a stream of
 *  [[Par]]s, called [[Parstream]], and [[getpar]]
 *  takes a [[Parstream]] and produces a [[Par]].
 *   • A [[Par]] is found on one or more input lines.
 *  (And an input line may contain more than one
 *  [[Par]].) A [[Parstream]] is produced from a
 *  [[Linestream]], and a [[Linestream]] may be
 *  produced either from a string or from an input
 *  file.
 * 
 * Each stream follows the same pattern: there are one
 * or more functions to create streams, and there's a
 * function to get a thing from a stream. Their
 * implementations are also similar. All the streams and
 * their implementations are presented in this section.
 * I present streams of lines first, then parenthesized
 * phrases, and finally extended definitions. That way,
 * as you read each implementation, you'll be familiar
 * with what it depends on.
 * 
 * Streams of lines
 * 
 * A [[Linestream]] encapsulates a seqeuence of input
 * lines.
 * 
 * Interface to Linestream
 * 
 * To use a [[Linestream]], call [[getline_]]. [The
 * function is called [[getline_]] with a trailing
 * underscore so as not to conflict with [[getline]], a
 * POSIX standard function. I~was using [[getline]] for
 * 20~years before the POSIX function was standardized,
 * and I'm too stubborn to change.] The [[getline_]]
 * function prints a prompt, reads the next line of
 * input from the source, and returns a pointer to the
 * line. You needn't worry about how long the line is;
 * [[getline_]] allocates enough memory to hold it.
 * Because [[getline_]] reuses the same memory to hold
 * successive lines, it is an unchecked run-time error
 * to retain a pointer returned by [[getline_]] after a
 * subsequent call to [[getline_]]. A client that needs
 * to save input characters must copy the result of
 * [[getline_]] before calling [[getline_]] again.\
 * intlabelLinestream\intlabelgetline_
 */

/*
 * A [[Stack]] is a mutable datatype. A [[Stack]] is
 * created by [[emptystack]], and it is mutated by
 * [[pushcontext]], [[popframe]], and [[clearstack]]
 * (which pops all the remaining frames). [*]
 * <function prototypes for \uschemeplus>=
 */
Stack  emptystack  (void);
Exp    pushcontext (struct Exp e, Stack s);
void   popframe    (Stack s);
void   clearstack  (Stack s);
/*
 * Function [[pushcontext]] pushes a frame in which
 * [[syntax]] is [[NULL]].
 */

/*
 * Function [[topframe]] returns a pointer to the frame
 * on the top of a stack, or if the stack is empty, it
 * returns [[NULL]]. It is OK to mutate a stack [[s]] by
 * writing through [[topframe(s)]].
 * <function prototypes for \uschemeplus>=
 */
Frame *topframe (Stack s);  // NULL if empty
/*
 * A special function is used to push a [[CALLENV]] or
 * [[LETXENV]] context. As described in \secref
 * schemes.tail-calls, this function can optimize tail
 * calls.
 * <function prototypes for \uschemeplus>=
 */
void   pushenv_opt (Env env, Expalt context, Stack s);  // may optimize
/*
 * As always with tricolor marking, we begin with the
 * roots, making the objects they point to gray. We then
 * turn gray objects black until there aren't any more
 * gray objects. In concrete terms, we first forward all
 * the pointers that are in roots, then forward pointers
 * in gray objects until there are no more.
 * <function prototypes for \uschemeplus>=
 */
void stack_trace_init(int *countp);  // how many steps to show
void stack_trace_current_expression(Exp e,   Env rho, Stack s);
void stack_trace_current_value     (Value v, Env rho, Stack s);
/*
 * Function [[getoption]] returns the value of an
 * option, or if the option is not set, it returns
 * [[defaultval]].
 * <function prototypes for \uschemeplus>=
 */
Value getoption(Name name, Env env, Value defaultval);
/*
 * Diagnostic code for \chaprefgc
 * 
 * \chaprefgc, which follows this chapter, builds on
 * this interpreter to implement automatic memory
 * management. The [[validate]] function is used to
 * debug garbage collectors; provided the argument [[v]]
 * represents a valid value, [[validate(v)]] returns 
 * [[v]].
 * <function prototypes for \uschemeplus>=
 */
Value validate(Value v);
/*
 * Updating lists of expressions within contexts
 * 
 * [*] Function application, the \xlet family, and \
 * xbegin all require evaluating expressions in
 * sequence. Looking at the \rulename
 * Small-Step-Apply-Next-Arg rule on page [->],
 * how should we implement the transition from a context
 * like \mathbox\capply(v_f, v_1, ..., v_i-1, \hole,
 * e_i+1, ..., e_n) to a context like \mathbox\capply
 * (v_f, v_1, ..., v_i-1, v, \hole, e_i+2, ..., e_n)?
 * We can represent the originating context \mathbox
 * v_1, ..., v_i-1, \hole, e_i+1, ..., e_n as a value of
 * type [[Explist]], where expressions e_i+1...e_n come
 * from the original syntax, and values v_1, ..., v_i-1
 * are represented as \xliteral expressions. Then, to
 * make the state transition, we can overwrite the hole
 * with v_i, and we can overwrite e_i+1 with a hole.
 * This operation is so common that I have defined a
 * function for it: function [[transition_explist]]
 * overwrites the hole with [[v]], writes a new hole one
 * position to the right, and returns the expression
 * that is overwritten by the new hole. That expression
 * is stored in static memory, so subsequent calls to
 * [[transition_memory]] overwrite previous results.
 * <function prototypes for \uschemeplus>=
 */
Exp transition_explist(Explist es, Value v); // pointer to static memory
/*
 * If the hole is in the rightmost position,
 * [[transition_explist]] overwrites the hole with [[v]]
 * and then returns [[NULL]].
 */

/*
 * What about initializing a context by putting a hole
 * in the first position? Function
 * [[head_replaced_with_hole]] works much like
 * [[transition_explist]]: it puts a hole in the initial
 * position and returns a pointer to the expression that
 * was there. If the list is empty, so there is no
 * initial position, it returns null.
 * <function prototypes for \uschemeplus>=
 */
Exp head_replaced_with_hole(Explist es);
                                        // shares memory with transition_explist
/*
 * A function like [[transition_explist]] really helps
 * implement \xapply, \xlet, and \xletrec, but we have
 * to be careful: if we are overwriting an [[Explist]],
 * we can't use the original [[Explist]] from the
 * syntax—we have to use a copy. The time to make the
 * copy is when we first push the context that contains
 * the [[Explist]]. For example, when I push a context
 * like \xapply(\hole, \ldotsne), I copy the list of
 * expressions using function [[copyEL]]:
 * 
 *  \monopushcontext(mkApplyStruct(mkHole(), copyEL(\
 *  ldotsne)), evalstack).
 * 
 * Function [[copyEL]] copies a list of expressions, and
 * when I'm finished with the copy, [[freeEL]] recovers
 * the memory.
 * <function prototypes for \uschemeplus>=
 */
Explist copyEL(Explist es);
void    freeEL(Explist es);
/*
 * We'll call [[freeEL]] when popping a context that
 * contains a copied [[Explist]].
 */

/*
 * When an [[Explist]] appears in an \xapply, \xlet, or
 * \xletrec context, each element goes through three
 * states:
 * 
 *  1. Initially it points to fresh memory that contains
 *  a copy of syntax from the original expression.
 * 
 *  \stdbreak
 *  2. At some point the syntax is copied into static
 *  memory and the element's own memory is
 *  overwritten to contain a hole.
 * 
 *  \stdbreak
 *  3. Finally the element's own memory is overwritten
 *  with the value that results from evaluating the
 *  original expression.
 * 
 *  \stdbreak
 * 
 * \stdbreak Once every element has reached the final
 * state, the [[Explist]] contains only literals, and we
 * can convert it to a list of values:
 * <function prototypes for \uschemeplus>=
 */
Valuelist asLiterals(Explist es);
Value     asLiteral (Exp e);
/*
 * Function [[asLiteral]] implements the same
 * conversion, but for a single [[Exp]]. And because
 * function [[asLiterals]] has to allocate, I provide
 * [[freeVL]], which frees the memory allocated by
 * [[asLiterals]].
 */

/*
 * <function prototypes for \uschemeplus>=
 */
void freeVL(Valuelist vs);
/*
 * Armed with these tools, we're ready to interpret
 * forms that evaluate expressions in sequence.
 */

/*
 * Printing the stack
 * 
 * Here are the functions used to print frames and
 * stacks. Function [[printnoenv]] prints the current
 * environment as a C pointer, rather than as a list of
 * (name, value) pairs.
 * <function prototypes for \uschemeplus>=
 */
void printstack   (FILE *, va_list_box*);
void printoneframe(FILE *, va_list_box*);
void printframe   (FILE *, Frame *fr);
void printnoenv   (FILE *, va_list_box*);
/*
 * Register roots are added and removed in last-in,
 * first-out order. For this purpose \apprefgcsa
 * provides functions [[pushreg]] and [[popreg]]. If the
 * pointer passed to [[popreg]] is not equal to the
 * pointer passed to the matching [[pushreg]], it is a
 * checked run-time error.
 * <function prototypes for \uschemeplus>=
 */
#ifndef DEBUG_GC_REGISTERS    /*OMIT*/
void pushreg(Value *reg);
void popreg (Value *reg);
#endif  /*OMIT*/
/*
 * We may also need to push or pop all the registers on
 * a list of values.
 */

/*
 * <function prototypes for \uschemeplus>=
 */
void pushregs(Valuelist regs);
void popregs (Valuelist regs);
/*
 * After a block of memory has been acquired (via
 * [[malloc]] or [[calloc]]) to hold heap objects, but
 * before any object has been delivered to the
 * interpreter via [[allocloc]], code should call
 * [[gc_debug_post_acquire]].
 * <function prototypes for \uschemeplus>=
 */
void gc_debug_post_acquire(Value *mem, unsigned nvalues); 
/*
 * This function must be used carefully; the copying
 * collector can announce the acquisition of an entire
 * block at once, but because the mark-and-sweep
 * collector wraps each [[Value]] in an [[Mvalue]], it
 * must call [[gc_debug_post_acquire]] on one object at
 * a time.
 */

/*
 * When a block of memory that belongs to the collector
 * is no longer needed and is about to be released, code
 * should call [[gc_debug_pre_release]] just before
 * calling [[free]]. As with acquisition, the
 * mark-and-sweep collector must release one object at a
 * time.
 * <function prototypes for \uschemeplus>=
 */
void gc_debug_pre_release(Value *mem, unsigned nvalues); 
/*
 * Just before the allocator delivers a heap object to
 * the interpreter, it should call
 * [[gc_debug_pre_allocate]].
 * <function prototypes for \uschemeplus>=
 */
void gc_debug_pre_allocate(Value *mem); 
/*
 * After the garbage collector decides an object is
 * unreachable, it should call function
 * [[gc_debug_post_reclaim]]. This function should be
 * called after the collector has finished writing to
 * any part of the object. The function will mark the
 * object [[INVALID]], and then until the object is
 * allocated again, Valgrind will complain about writes
 * to it. Even if you don't have Valgrind, the function
 * is still useful; for example, if a value like [['(a b
 * c)]] mysteriously turns into [['(a b . <invalid>)]],
 * you've discovered a premature reclamation.
 * <function prototypes for \uschemeplus>=
 */
void gc_debug_post_reclaim(Value *mem); 
/*
 * For the convenience of the copying collector, which
 * reclaims an entire semispace at one time, it can call
 * [[gc_debug_post_reclaim_block]]. Be careful if you're
 * reclaiming a semispace because the heap grew—you'll
 * need to pass the old, smaller size, not the new,
 * larger size.
 * <function prototypes for \uschemeplus>=
 */
void gc_debug_post_reclaim_block(Value *mem, unsigned nvalues); 
/*
 * Whenever the interpreter uses a value that is
 * obtained by dereferencing a pointer into the heap, it
 * should wrap that value in [[validate]]. If the
 * [[alt]] field of [[v]] is [[INVALID]], [[validate
 * (v)]] halts the program with an error message.
 * Otherwise it returns [[v]]. The interpreter in \
 * chaprefschemes calls [[validate]], and your own code
 * should also call [[validate]] whenever it reads a
 * value that you believe should be good.
 * <function prototypes for \uschemeplus>=
 */
Value validate(Value v);
/*
 * If the tools above aren't enough, you can use
 * functions [[gcprint]] and [[gcprintf]] to write any
 * information you like to standard error. Function
 * [[gcprint]] works like [[print]], and [[gcprintf]]
 * works like [[printf]], but they work only when the
 * environment variable GCVERBOSE is set. If GCVERBOSE
 * is not set, [[gcprint]] and [[gcprintf]] do nothing.
 * <function prototypes for \uschemeplus>=
 */
void gcprint (const char *fmt, ...);  /* print GC debugging info */
void gcprintf(const char *fmt, ...);
/*
 * Finally, the debug code needs to be initialized—but
 * [[initallocate]] in \apprefgcsa does the
 * initialization.
 * <function prototypes for \uschemeplus>=
 */
void gc_debug_init(void);
/*
 * <function prototypes for \uscheme>=
 */
int gammadesired(int defaultval, int minimum);
/*
 * <function prototypes for \uscheme>=
 */
Value *find(Name name, Env env);
/*
 * The function [[bindalloc]] binds a name to a freshly
 * allocated location, and it puts a value in that
 * location. Formally, when called with store sigma,
 * bindalloc(x, v, rho) chooses an \aloc\notindom sigma,
 * updates the store to be sigma{\aloc|->v}, and returns
 * the extended environment rho{x |->\aloc}. \scmflabel
 * bindalloc,bindalloclist
 * <function prototypes for \uscheme>=
 */
Env bindalloc    (Name name,   Value v,      Env env);
Env bindalloclist(Namelist xs, Valuelist vs, Env env);
/*
 * Calling bindalloclist(<\ldotsnx>, <\ldotsnv>, rho)
 * does the same job for a list of values, returning rho
 * {x_1 |->\aloc_1, ..., x_n |->\aloc_n}, where \ldotsn\
 * aloc are fresh locations, which [[bindalloclist]]
 * initializes to values \ldotsnv.
 */

/*
 * Allocation
 * 
 * The fresh locations created by [[bindalloc]] and
 * [[bindalloclist]] come from [[allocate]]. Calling
 * [[allocate(v)]] finds a location \aloc\notindom sigma
 * , stores [[v]] in \aloc (thereby updating sigma), and
 * returns \aloc.[*]
 * <function prototypes for \uscheme>=
 */
Value *allocate(Value v);
/*
 * <function prototypes for \uscheme>=
 */
void initallocate(Env *globals);
/*
 * Allocation is described in great detail in Chapter 
 * [->].
 */

/*
 * Values
 * 
 * [*] The representation of values appears in chunk 
 * [->] in Section [->] above. The value interface also
 * exports predefined values [[truev]] and [[falsev]],
 * which represent [[#t]] and [[#f]].
 * <function prototypes for \uscheme>=
 */
Value truev, falsev;
/*
 * Before executing any code that refers to [[truev]] or
 * [[falsev]], clients must call [[initvalue]].
 */

/*
 * <function prototypes for \uscheme>=
 */
void initvalue(void);
/*
 * Function [[istrue]] takes a value and returns
 * [[true]] if the value should be regarded as true
 * (i.e., is not [[#f]]) and [[false]] otherwise.
 * <function prototypes for \uscheme>=
 */
bool istrue(Value v);
/*
 * Function [[unspecified]] returns an unspecified
 * value.
 * <function prototypes for \uscheme>=
 */
Value unspecified(void);
/*
 * If you get the micro-Scheme interpreter to crash,
 * your micro-Scheme code is probably looking at a value
 * returned by [[unspecified]]. That's an unchecked
 * run-time error.
 */

/*
 * For example, \monoboxeval(e, rho), when evaluated
 * with store sigma, finds a v and a sigma' such that \
 * evale ==>\evalr[']v, updates the store to be sigma',
 * and returns v.
 * <function prototypes for \uscheme>=
 */
Value eval   (Exp e, Env rho);
Env   evaldef(Def d, Env rho, Echo echo);
/*
 * Similarly, \monoboxevaldef(e, rho, echo), when
 * evaluated with store sigma, finds a rho' and a sigma'
 * such that \evaldefe -->\evaldefr', updates the store
 * to be sigma', and returns rho'. If [[echo]] is
 * [[ECHOING]], [[evaldef]] also prints the name or
 * value of whatever expression is evaluated or added
 * to rho.
 */

/*
 * [*] To handle a sequence of definitions, we use
 * [[readevalprint]]. In principle, [[readevalprint]]
 * ought to look a lot like [[evaldef]]. In particular,
 * [[readevalprint]] ought to take an environment and
 * return an environment. But when an error occurs,
 * [[readevalprint]] doesn't actually return; instead it
 * calls [[synerror]] or [[runerror]]. And if an error
 * occurs, we don't want to lose the definitions that
 * precede it. So instead of returning a new
 * environment, [[readevalprint]] writes the new
 * environment through an environment pointer [[envp]],
 * which is passed as a parameter.
 * <function prototypes for \uscheme>=
 */
void readevalprint(XDefstream xdefs, Env *envp, Echo echo);
/*
 * Primitives
 * 
 * Compared to Impcore, micro-Scheme has many
 * primitives. The function [[addprimitives]] mutates an
 * existing environment by adding bindings to all the
 * primitive operations.
 * <function prototypes for \uscheme>=
 */
void addprimitives(Env *envp);
/*
 * Here are some of the printing functions used.
 * <function prototypes for \uscheme>=
 */
void printenv    (Printbuf, va_list_box*);
void printvalue  (Printbuf, va_list_box*);
void printexp    (Printbuf, va_list_box*);
void printdef    (Printbuf, va_list_box*);
void printlambda (Printbuf, va_list_box*);
/*
 * <function prototypes for \uscheme>=
 */
void process_tests(UnitTestlist tests, Env rho);
/*
 * We implement them with the function [[binary]], which
 * delegates to [[cons]] and [[equalatoms]]. [*]
 * <function prototypes for \uscheme>=
 */
Value cons(Value v, Value w);
Value equalatoms(Value v, Value w);
/*
 * <function prototypes for \uscheme ((elided))>=
 */
Exp desugarLetStar(Namelist xs, Explist es, Exp body);
Exp desugarLet    (Namelist xs, Explist es, Exp body);
/*
 * \crefpage
 * scheme.ex.let-sugar-ok,scheme.ex.letstar-sugar-ok ask
 * you to prove these desugarings are correct.
 */

/*
 * New parsing functions: S-expressions and bindings
 * 
 * Each new shift function is supported by a new parsing
 * function.
 * <function prototypes for \uscheme>=
 */
Value parsesx(Par p, Sourceloc source);
struct Component parseletbindings(ParsingContext context, Parlist input);
/*
 * <function prototypes for \uscheme>=
 */
int number_of_good_tests(UnitTestlist tests, Env rho);
/*
 * And except for the environment, [[test_result]] is
 * just like the Impcore version.
 * <function prototypes for \uscheme>=
 */
TestResult test_result(UnitTest t, Env rho);
/*
 * Function [[equalpairs]] tests for equality of atoms
 * and pairs. It resembles function [[equalatoms]] (\
 * chunkrefscheme.chunk.equalatoms), which implements
 * the primitive [[=]], with two differences:
 * 
 *   • Its semantics are those of [[equal?]], not [[=]].
 *   • Instead of returning a micro-Scheme Boolean
 *  represented as a C [[Value]], it returns a
 *  Boolean represented as a C [[bool]].
 * 
 * [*]
 * <function prototypes for \uscheme>=
 */
bool equalpairs(Value v, Value w);
/*
 * Support for an exercise: Concatenating names
 * 
 * Here is an auxiliary function that will be useful if
 * you do \schemexpagerecord-sugar. It concatenates
 * names.
 * <function prototypes for \uscheme>=
 */
Name namecat(Name n1, Name n2);
/*
 * The rest of the interface is just the allocator. The
 * managed-heap function [[allocloc]] provides an
 * uninitialized location; chunk [->] shows how to use
 * [[allocloc]] to implement [[allocate]].
 * <function prototypes for \uscheme>=
 */
Value *allocloc(void);
/*
 * [*] The allocator and [[roots]] structure are related
 * by this precondition: Clients may call [[allocloc]]
 * only when all objects that could lead to live values
 * appear in [[roots]]. The copying collector's
 * implementation of [[allocloc]] also requires that,
 * when called, all pointers to allocated values must be
 * reachable from [[roots]], so that they can be updated
 * when the values move.
 */

/*
 * The interpreter has one more obligation: before
 * calling [[allocloc]], it must call [[initallocate]],
 * passing a pointer to the environment that holds the
 * global variables.
 * <function prototypes for \uscheme>=
 */
void initallocate(Env *globals);
/*
 * <function prototypes for \uscheme>=
 */
Exp desugarAnd(Explist args);
/*
 * <function prototypes for \uscheme>=
 */
Namelist freevars(Exp e, Namelist bound, Namelist free);
/*
 * <function prototypes for \uscheme>=
 */
Exp desugarOr(Explist args);
/*
 * <function prototypes for \uscheme>=
 */
Exp desugarCond(Explist questions, Explist answers);
/*
 * <function prototypes for \uscheme>=
 */
Deflist desugarRecord(Name recname, Namelist fieldnames);
/*
 * Pointer comparison is built into C, but I provide two
 * other operations on names.
 * <shared function prototypes>=
 */
Name strtoname(const char *s);
const char *nametostr(Name x);
/*
 * These functions satisfy the following algebraic laws:
 * 
 *  [[strcmp(s, nametostr(strtoname(s))) == 0]]
 *  [[strcmp(s, t) == 0]] if and only if [[strtoname
 *  (s) == strtoname(t)]]
 * 
 * Informally, the first law says if you build a name
 * from a string, [[nametostr]] returns a copy of your
 * original string. The second law says you can compare
 * names using pointer equality.
 */

/*
 * <shared function prototypes>=
 */
XDef getxdef(XDefstream xdefs);
/*
 * To create a stream of definitions, we need a source
 * of lines. That source can be a string compiled into
 * the program, or an external file. So that error
 * messages can refer to the source, we need to give its
 * name. And if the source is a file, we need to say
 * whether to prompt for input. (Reading from an
 * internal string never prompts.) \intlabelfilexdefs\
 * intlabelstringxdefs [*]
 */

/*
 * <shared function prototypes>=
 */
XDefstream stringxdefs(const char *stringname, const char *input);
XDefstream filexdefs  (const char *filename, FILE *input, Prompts prompts);
/*
 * Prompts are either absent or standard; the interface
 * provides no way to change prompts.\intlabelPrompts
 */

/*
 * Interface to infrastructure: Printing
 * 
 * [*] After every definition, the interpreter prints a
 * name or a value. And if an error occurs or a unit
 * test fails, the interpreter may also print an
 * expression or a definition. For printing,
 * the C standard library provides [[printf]], but
 * [[printf]] and its siblings are not well suited to
 * print messages that include renderings of expressions
 * or definitions. To address this problem,
 * this interface defines functions [[print]] and
 * [[fprint]], which support direct printing of [[Name]]
 * s, [[Exp]]s, and so on.\intlabelprint\intlabelfprint
 * <shared function prototypes>=
 */
void print (const char *fmt, ...);  // print to standard output
void fprint(FILE *output, const char *fmt, ...);  // print to given file
/*
 * The implementations of [[print]] and [[fprint]] are
 * extensible; adding a new conversion specification is
 * as simple as calling [[installprinter]]:\intlabel
 * installprinter
 * <shared function prototypes>=
 */
void installprinter(unsigned char c, Printer *take_and_print);
/*
 * The conversion specifications listed above are
 * installed when the interpreter launches, by code
 * chunk [[<<install conversion specifications for
 * [[print]] and [[fprint]]>>]]. The details, including
 * the definition of [[Printer]], are in \cref
 * Printer.int,impcorea.printfuns.
 */

/*
 * The simpler of the two error-signaling functions is
 * [[runerror]]. In its normal mode of operation
 * [[runerror]] prints to standard error and then
 * [[longjmp]]s to [[errorjmp]]. [*]\intlabelrunerror
 * <shared function prototypes>=
 */
void runerror(const char *fmt, ...);
extern jmp_buf errorjmp;        // longjmp here on error
/*
 * During unit testing, [[runerror]] operates in testing
 * mode, and it behaves a little differently. The
 * details are in \crefpagecinterps.runerror.
 */

/*
 * <shared function prototypes>=
 */
void synerror(Sourceloc src, const char *fmt, ...);
/*
 * The possibility of printing source-code locations
 * complicates the interface. When the interpreter is
 * reading code interactively, printing source-code
 * locations is silly—if there's a syntax error, it's in
 * what you just typed. But if the interpreter is
 * reading code from a file, it's a different story—it's
 * useful to have the file's name and the number of the
 * line containing the bad syntax. But the error module
 * doesn't know where the interpreter is reading code
 * from—only the [[main]] function in \chunkref
 * impcore.chunk.main knows that. So the error module
 * has to be told how syntax errors should be formatted:
 * with locations or without. \intlabelErrorFormat
 */

/*
 * <shared function prototypes>=
 */
void set_toplevel_error_format(ErrorFormat format);
/*
 * Interface to infrastructure: Helper functions for
 * common errors
 * 
 * [*] To help the interpreter detect errors, I define a
 * couple of functions that are used in evaluating
 * function calls and function definitions. Function
 * [[checkargc]] is used to check the number of
 * arguments to both user-defined and primitive
 * functions. The first argument is an abstract-syntax
 * tree representing the application being checked; if
 * [[expected]] != [[actual]], [[checkargc]] calls
 * [[runerror]], passing a message that contains [[e]].
 * \intlabelcheckargc
 * <shared function prototypes>=
 */
void checkargc(Exp e, int expected, int actual);
/*
 * The [[duplicatename]] function finds a duplicate name
 * on a [[Namelist]] if such a name exists. It is used
 * to check that formal parameters to user-defined
 * functions all have different names. If the name list
 * [[names]] contains a duplicate occurrence of any
 * name, the function returns such a name; otherwise it
 * returns [[NULL]]. \intlabelduplicatename
 * <shared function prototypes>=
 */
Name duplicatename(Namelist names);
/*
 * Planning an extensible parser
 * 
 * A parser is a function that is given a [[Par]] and
 * builds an abstract-syntax tree, which it then
 * returns. Each of the first three bridge languages
 * (Impcore, micro-Scheme, and \uschemeplus) has two
 * major syntactic categories, which means two types of
 * abstract-syntax trees, which means two parsers.
 * <shared function prototypes>=
 */
Exp  parseexp (Par p, Sourceloc source);
XDef parsexdef(Par p, Sourceloc source);
/*
 * Each parser also takes a pointer to a source-code
 * location, which it uses if it has to report an error.
 */

/*
 * Parsing begins with a look at the input, which is
 * either an [[ATOM]] or a [[LIST]] of [[Par]]s. And the
 * interpretation of the input depends on whether we are
 * parsing an [[Exp]] or an [[XDef]].
 * 
 *   • If the input is an [[ATOM]], we are parsing an
 *  expression (in Impcore, a [[VAR]] or [[LITERAL]]
 *  expression), and the job of making it into an
 *  [[Exp]] is given to function [[exp_of_atom]],
 *  which is language-dependent.
 * <shared function prototypes>=
 */
Exp exp_of_atom(Name atom);
/*
 * The standard reduce functions are [[reduce_to_exp]]
 * and [[reduce_to_xdef]]. The first argument codes for
 * what kind of node the components should be
 * reduced to; the second argument points to an array
 * that holds the components.
 * <shared function prototypes>=
 */
Exp  reduce_to_exp (int alt, struct Component *components);
XDef reduce_to_xdef(int alt, struct Component *components);
/*
 * <shared function prototypes>=
 */
struct ParserState mkParserState(Par p, Sourceloc source);
/*
 * Here are the four basic shift functions.
 * <shared function prototypes>=
 */
ParserResult sExp     (ParserState state);  /* shift 1 input into Exp */
ParserResult sExps    (ParserState state);  /* shift all inputs into Explist */
ParserResult sName    (ParserState state);  /* shift 1 input into Name */
ParserResult sNamelist(ParserState state);  /* shift 1 input into Namelist */
/*
 * The names are abbreviated because I represent a
 * syntactic form's components as an array of shift
 * functions. This dirty trick is inspired by the
 * functional-programming techniques described in \
 * chaprefscheme. But we don't need those techniques
 * just yet. For now, let's just implement shift
 * functions.
 */

/*
 * <shared function prototypes>=
 */
void halfshift(ParserState state); /* advance input, check for room in output */
/*
 * <shared function prototypes>=
 */
Explist parseexplist(Parlist p, Sourceloc source);
/*
 * <shared function prototypes>=
 */
Name parsename(Par p, ParsingContext context);
/*
 * <shared function prototypes>=
 */
ParserResult stop(ParserState state);
/*
 * <shared function prototypes>=
 */
ParserResult setcontextname(ParserState state);
/*
 * <shared function prototypes>=
 */
ParserResult sLocals(ParserState state);  // shift locals if (locals x y z ...)
/*
 * <shared function prototypes>=
 */
void rowparse(struct ParserRow *table, ParserState s);
void usage_error(int alt, ParserResult r, ParsingContext context);
/*
 * <shared function prototypes>=
 */
struct ParserRow *tableparse(ParserState state, ParserTable t);
/*
 * <shared function prototypes>=
 */
ParserResult use_exp_parser(ParserState state);
/*
 * <shared function prototypes>=
 */
int code_of_name(Name n);
/*
 * In Impcore, there are no expressions that bind names,
 * so expressions need not be checked; only [[define]]
 * needs to be checked.
 * <shared function prototypes>=
 */
void check_exp_duplicates(Sourceloc source, Exp e);
void check_def_duplicates(Sourceloc source, Def d);
/*
 * <shared function prototypes>=
 */
Par mkAtom(Name atom);
Par mkList(Parlist list);
struct Par mkAtomStruct(Name atom);
struct Par mkListStruct(Parlist list);
/*
 * <shared function prototypes>=
 */
char *getline_(Linestream r, const char *prompt);
/*
 * To create a [[Linestream]], you need a string or a
 * file. And when creating a [[Linestream]], you name
 * the source; that name is used in error messages. [*] 
 * [*]
 * <shared function prototypes>=
 */
Linestream stringlines(const char *stringname, const char *s);
Linestream filelines  (const char *filename,   FILE *fin);
/*
 * If an [[s]] passed to [[stringlines]] is nonempty, it
 * is a checked run-time error for it to end in any
 * character except newline. After a call to
 * [[stringlines]], client code must ensure that
 * pointers into [[s]] remain valid until the last call
 * to [[getline_]]. If [[getline_]] is called after the
 * memory pointed to by [[s]] is no longer valid, it is
 * an unchecked run-time error.
 */

/*
 * To create a [[Parstream]], you specify not only the
 * lines from which [[Par]]s will be read, but also the
 * prompts to be used (\cpagerefPrompts.int). To get a
 * [[Par]] from a stream, call [[getpar]]. And for error
 * messages, code can ask a [[Parstream]] for its
 * current source location. \intlabelparstream\intlabel
 * getpar
 * <shared function prototypes>=
 */
Parstream parstream(Linestream lines, Prompts prompts);
Par       getpar   (Parstream r);
Sourceloc parsource(Parstream pars);
/*
 * The final part of the interface to a [[Parstream]] is
 * the global variable [[read_tick_as_quote]]. If
 * [[read_tick_as_quote]] is true, [[getpar]] turns an
 * input like [['(1 2 3)]] into the parenthesized phrase
 * [[(quote (1 2 3))]]. When set, this variable makes
 * the tick mark behave the way micro-Scheme wants it to
 * behave.
 * <shared function prototypes>=
 */
extern bool read_tick_as_quote;
/*
 * A buffer is created with [[printbuf]] and destroyed
 * with [[freebuf]].
 * <shared function prototypes>=
 */
Printbuf printbuf(void);
void freebuf(Printbuf *);
/*
 * We append to a buffer with [[bufput]] or [[bufputs]],
 * and we empty the buffer with [[bufreset]].
 * <shared function prototypes>=
 */
void bufput(Printbuf, char);
void bufputs(Printbuf, const char*);
void bufreset(Printbuf);
/*
 * We can do two things with the contents of a buffer:
 * copy them in to a freshly allocated block of memory,
 * or write them to an open file handle.
 * <shared function prototypes>=
 */
char *bufcopy(Printbuf);
void fwritebuf(Printbuf buf, FILE *output);
/*
 * The extensible buffer printer
 * 
 * To recapitulate \crefsec:print-interface, the
 * standard C functions [[printf]] and [[fprintf]] are
 * great, but they don't know how to print things like
 * values and expressions. And when you can't put a
 * value or an expression in a format string, the code
 * needed to print an error message becomes awkward and
 * unreadable. My solution is to define new, custom
 * print functions that know how to print values and
 * expressions:
 * <shared function prototypes>=
 */
void print (const char *fmt, ...);                /* print to standard output */
void fprint(FILE *output, const char *fmt, ...);     /* print to given file */
void bprint(Printbuf output, const char *fmt, ...);  /* print to given buffer */
/*
 * I use [[bprint]] to write error messages—if an error
 * message is written during the evaluation of a
 * [[check-expect]] or [[check-error]], the message can
 * be captured and can either be used to explain what
 * went wrong (if an error occurs unexpectedly during a
 * [[check-expect]]) or can be silently discarded (if an
 * error occurs as expected during a [[check-error]]).
 */

/*
 * To extend a printer, you announce a new format
 * specifier with [[installprinter]], and you provide a
 * function used to print a value so specified.
 * <shared function prototypes>=
 */
void installprinter(unsigned char specifier, Printer *take_and_print);
/*
 * The function provided has type [[Printer]]. Its
 * specification is that it takes one value out of the
 * list [[args]], then prints the value to the
 * given buffer. [*]\intlabelPrinter
 */

/*
 * The next brick is my function [[vbprint]] and its
 * associated table [[printertab]]. Function [[vbprint]]
 * stands in the same relation to [[bprint]] as standard
 * function [[vfprintf]] stands to [[fprintf]]:
 * <shared function prototypes>=
 */
void vbprint(Printbuf output, const char *fmt, va_list_box *box);
/*
 * The [[printertab]] table, which is private to the
 * printing module, associates a [[Printer]] function to
 * each possible conversion specifier. This style of
 * programming exploits first-class functions in C,
 * drawing on some of the ideas presented as part of
 * micro-Scheme in \crefscheme.chap. Function
 * [[installprinter]] simply updates [[printertab]].
 */

/*
 * Printing functions
 * 
 * [*] The most interesting printing functions are
 * language-dependent; they are found in \cref
 * impcorea.chap,schemea.chap. But functions that print
 * percent signs, strings, decimal integers, characters,
 * and names are shared among all languages, and they
 * are found here.
 * <shared function prototypes>=
 */
Printer printpercent, printstring, printdecimal, printchar, printname;
/*
 * The print function for parenthesized phrases is
 * surprisingly simple: it just calls [[bprint]]
 * recursively:
 * <shared function prototypes>=
 */
Printer printpar;
/*
 * In testing mode, [[runerror]] buffers an error
 * message and [[longjmp]]s to [[testjmp]]. \intlabel
 * ErrorMode\intlabelset_error_mode
 * <shared function prototypes>=
 */
typedef enum ErrorMode { NORMAL, TESTING } ErrorMode;
void set_error_mode(ErrorMode mode);
extern jmp_buf testjmp;    /* if error occurs during a test, longjmp here */
Printbuf errorbuf;         /* if error occurs during a test, message is here */
/*
 * The error mode is initially [[NORMAL]], but it can be
 * changed using [[set_error_mode]]. When the error mode
 * is [[TESTING]], it is an unchecked run-time error to
 * call [[synerror]], and it is an unchecked run-time
 * error to call [[runerror]] except while a [[setjmp]]
 * involving [[testjmp]] is active on the C call stack.
 */

/*
 * The implementation uses C trickery with [[volatile]]
 * variables: the address of a [[volatile]] local
 * variable [[c]] is used as a proxy for the stack
 * pointer. (Because I spent years writing compilers,
 * I understand a little of how these things work.) The
 * first call to [[checkoverflow]] captures the stack
 * pointer and stores as a ``low-water mark.'' Each
 * later call checks the current stack pointer against
 * that low-water mark. If the distance exceeds
 * [[limit]], [[checkoverflow]] calls [[runerror]].
 * Otherwise it returns the distance.
 * <shared function prototypes>=
 */
extern int  checkoverflow(int limit);
extern void reset_overflow_check(void);
/*
 * Arithmetic-overflow detection
 * 
 * Unlike standard C arithmetic, the arithmetic in this
 * book detects arithmetic overflow: an operation on
 * 32-bit signed integers whose result cannot also be
 * represented as a 32-bit signed integer. Such
 * arithmetic is defined by the C standard as
 * ``undefined behavior,'' so our code needs to
 * detect it before it might happen. Function
 * [[checkarith]] does arithmetic using 64-bit integers,
 * and if the result does not fit in the specified
 * number of bits, it triggers a checked run-time error.
 * <shared function prototypes>=
 */
extern void checkarith(char operation, int32_t n, int32_t m, int precision);
/*
 * Here's how we print Unicode characters.
 * <shared function prototypes>=
 */
void fprint_utf8(FILE *output, unsigned code_point);
void print_utf8 (unsigned u);
/*
 * The auxiliary function [[report_test_results]] prints
 * a report of the results. The reporting code is shared
 * among all interpreters written in C; its
 * implementation appears in \crefpage
 * cinterps.report_test_results.
 * <shared function prototypes>=
 */
void report_test_results(int npassed, int ntests);
/*
 * Functions [[printdecimal]], [[printname]],
 * [[printstring]], and [[printpercent]] are defined in
 * \crefpagecinterps.printfuns. Functions that print
 * lists are generated automatically. The remaining
 * functions, which print Impcore's abstract syntax and
 * values, are defined here.
 * <shared function prototypes>=
 */
Printer printexp, printdef, printvalue, printfun;
/*
 * Implementations of the primitives
 * 
 * [*] Each primitive is associated with a unique tag,
 * which identifies the primitive and with a function,
 * which implements the primitive. The tags make it easy
 * for one function to implement more than one
 * primitive, which makes it easy for similar primitives
 * to share code. The primitives are implemented by
 * these functions:
 * 
 *  [[arith]]  Arithmetic primitives, which expect
 *             integers as arguments
 *  [[binary]] Non-arithmetic primitives that expect
 *             two arguments
 *  [[unary]]  Primitives that expect one argument
 * 
 * <shared function prototypes>=
 */
Primitive arith, binary, unary;
/*
 * Shift functions are as in Impcore, but with two
 * additions: to parse quoted S-expressions, shift
 * function [[sSexp]] has been added, and to parse
 * bindings in [[LETX]] forms, [[sBindings]] has been
 * added.
 * <shared function prototypes>=
 */
ParserResult sSexp    (ParserState state);
ParserResult sBindings(ParserState state);

/*
 * Finally, global variable [[high_stack_mark]] tracks
 * the maximum number of frames held on the stack during
 * an evaluation.
 * <global variables for \uschemeplus>=
 */
extern int high_stack_mark;
/*
 * Instrumentation
 * 
 * An option is a \uschemeplus variable that might be
 * set to influence the behavior of the interpreter.
 * 
 *   • Option [[ --- optimize-tail-calls]], if set to
 *  [[#f]], prevents the interpreter from optimizing
 *  tail calls.
 *   • Option [[ --- show-high-stack-mark]], if set to a
 *  non-[[#f]] value, prints the maximum size of the
 *  stack after each definition is evaluated.
 *   • Option [[ --- trace-stack]], if set to a
 *  nonnegative number n, shows the abstract-machine
 *  state for n steps. If [[ --- trace-stack]] is
 *  negative, all steps are shown. It is an unchecked
 *  run-time error to change [[ --- trace-stack]]
 *  from a number to a non-number.
 * 
 * Options [[ --- optimize-tail-calls]] and [[ ---
 * show-high-stack-mark]] are used to set these global
 * variables:
 * <global variables for \uschemeplus>=
 */
extern int optimize_tail_calls;
extern int show_high_stack_mark;

/*
 * To enable the codes to appear as cases in [[switch]]
 * statements, I define them using C macros:
 * <macro definitions used in parsing>=
 */
#define ANEXP(ALT)  (  0+(ALT))
#define ADEF(ALT)   (100+(ALT))
#define ATEST(ALT)  (200+(ALT))
#define ANXDEF(ALT) (300+(ALT))
#define ALET(ALT)   (400+(ALT))
#define SUGAR(CODE) (500+(CODE))
#define LATER       1000
#define EXERCISE    1001
/*
 * The names are abbreviated because I represent a
 * syntactic form's components as an array of shift
 * functions. This dirty trick is inspired by the
 * functional-programming techniques described in \
 * chaprefscheme. But we don't need those techniques
 * just yet. For now, let's just implement shift
 * functions.
 * <declarations of global variables used in lexical analysis and parsing>=
 */
extern struct ParserRow exptable[];
extern struct ParserRow xdeftable[];
/*
 * When a parser sees input with the wrong number of
 * components, as in \monobox(if p (set x 5)) or \
 * monobox(set x y z), it calls [[usage_error]] with a
 * code, a [[ParserResult]], and a context. The code is
 * looked up in [[usage_table]], which contains a sample
 * string showing what sort of syntax was expected.
 * <declarations of global variables used in lexical analysis and parsing>=
 */
extern struct Usage {
    int code;
                         /* codes for form in reduce_to_exp or reduce_to_xdef */
    const char *expected;  /* shows the expected usage of the identified form */
} usage_table[];
/*
 * The root type and its variables are visible to all
 * C code.
 * <{\Tt all.h} for \uschemeplus>=
 */
/*
 * <structure definitions used in garbage collection>=
 */
struct Roots {
    struct {
        Env *user;              // global variables from the user's program 
        struct {
            UnitTestlistlist pending_tests; // unit tests waiting to be run
        } internal;             // the interpreter's internal variables
    } globals;                  // all the global variables
    Stack stack;
                           // the uscheme+ stack, with all parameters and locals
    Registerlist registers;    // pointers to 'machine registers'
};
/*
 * At initialization we create a [[gc_pool]], which
 * stands for all objects allocated using [[allocloc]].
 * The flag [[gc_uses_mark_bits]], if set, tells
 * Valgrind that when memory is first allocated, its
 * contents are zero. We also initialize the
 * [[gcverbose]] flag.
 * <global variables used in garbage collection>=
 */
extern int gc_uses_mark_bits;
/*
 * This is the data structure that is shared between the
 * interpreter and the garbage collector. The
 * interpreter makes sure that before any call to
 * [[allocate]], [[roots]] is up to date and contains
 * pointers to any locations that could affect the rest
 * of the computation. The garbage collector inspects
 * the roots and also updates pointers to any objects
 * that it moves.
 * <global variables used in garbage collection>=
 */
extern struct Roots roots;

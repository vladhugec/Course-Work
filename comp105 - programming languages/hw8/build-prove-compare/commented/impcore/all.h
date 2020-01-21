/*
 * To make it possible to reuse the general-purpose
 * interfaces in later interpreters, I also distinguish
 * between shared and unshared definitions; a definition
 * is ``shared'' if it is used in another interpreter
 * later in the book.
 * <{\Tt all.h} for \impcore>=
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
 * <type definitions for \impcore>=
 */
typedef struct UnitTestlist *UnitTestlist; // list of UnitTest
/*
 * A [[UnitTestlist]] is list of pointers of type
 * [[UnitTest]]. I use this naming convention in all my
 * C code. List types are manifest, and their
 * definitions are in the lists interface in chunk [->].
 * I also define a type for lists of [[Exp]]s.
 */

/*
 * <type definitions for \impcore>=
 */
typedef struct Explist *Explist; // list of Exp
/*
 * Interface to values
 * 
 * The value interface defines the type of value that
 * our expressions evaluate to. Impcore supports only
 * integers. A [[Valuelist]] is a list of [[Value]]s.\
 * iilabelValue
 * <type definitions for \impcore>=
 */
typedef int32_t Value;
typedef struct Valuelist *Valuelist;     // list of Value
/*
 * Interface to functions, both user-defined and
 * primitive
 * 
 * In the Impcore interpreter, the type ``function'' is
 * another discriminated-union type. There are two
 * alternatives: user-defined functions and primitive
 * functions. Just like the operational semantics, which
 * represents a user-defined function as \user(<x_1,
 * ..., x_n>, e), the interpreter represents a
 * user-defined function as a pair containing formals
 * and body. The interpreter represents each primitive
 * by its name. \iilabelFun
 * <type definitions for \impcore>=
 */
typedef struct Funlist *Funlist; // list of Fun
/*
 * Interface to environments: more abstract types
 * 
 * In the operational semantics, the environments rho
 *  and xi hold values, and the environment phi holds
 * functions. To represent these two kinds of
 * environments, C offers these choices:
 * 
 *   • We can define one C type for environments that
 *  hold a [[Value]] and another for environments
 *  that hold a [[Fun]], and we can define two
 *  versions of each function. This choice guarantees
 *  type safety, but requires duplication of code.
 *   • We can define a single C type for environments
 *  that hold a [[void*]] pointer, define a single
 *  version of each function, and use type casting to
 *  convert a [[void*]] to a [[Value*]] or [[Fun*]]
 *  as needed. This choice duplicates no code, but it
 *  is unsafe; if we accidentally put a [[Value*]] in
 *  an environment intended to hold a [[Fun*]], it is
 *  an error that neither the C compiler nor the
 *  run-time system can detect.
 * 
 * In the interests of safety, I duplicate code.
 * Chapter [->] shows how in another implementation
 * language, ML, we can use polymorphism to achieve type
 * safety without duplicating code.\iilabelFunenv\
 * iilabelValenv
 * <type definitions for \impcore>=
 */
typedef struct Valenv *Valenv;
typedef struct Funenv *Funenv;
/*
 * <type definitions for \impcore>=
 */
typedef struct Exp *Exp;
typedef enum { LITERAL, VAR, SET, IFX, WHILEX, BEGIN, APPLY } Expalt;

/*
 * <type definitions for \impcore>=
 */
typedef struct Userfun Userfun; 
typedef struct Def *Def;
typedef enum { VAL, EXP, DEFINE } Defalt; 
/*
 * <type definitions for \impcore>=
 */
typedef struct XDef *XDef;
typedef enum { DEF, USE, TEST } XDefalt; 
typedef struct UnitTest *UnitTest;
typedef enum { CHECK_EXPECT, CHECK_ASSERT, CHECK_ERROR } UnitTestalt;

/*
 * <type definitions for \impcore>=
 */
typedef struct Fun Fun;
typedef enum { USERDEF, PRIMITIVE } Funalt; 
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
 * <shared type definitions>=
 */
typedef struct Par *Par;
typedef enum { ATOM, LIST } Paralt; 

/*
 * Components, reduce functions, and form codes
 * 
 * A parser consumes inputs and puts components into an
 * array. (Inputs are [[Par]]s and components are
 * abstract syntax.) A reduce function takes the
 * components in the array and reduces the them to a
 * single node an even bigger abstract-syntax tree
 * (which may then be stored as a component in another
 * array). ``Reduction'' is done by applying the build
 * function for the node to the components that are
 * reduced. In Impcore, a component is an expression, a
 * list of expressions, a name, or a list of names. [*]
 * <structure definitions for \impcore>=
 */
struct Component {
    Exp exp;
    Explist exps;
    Name name;
    Namelist names;
};
/*
 * If you're a seasoned C programmer, you might think
 * that the ``right'' representation of the component
 * abstraction is a [[union]], not a [[struct]]. But
 * unions are unsafe. By using a struct, I give myself a
 * fighting chance to debug the code. If I make a
 * mistake and pick the wrong component,
 * a memory-checking tool like Valgrind (\crefpage
 * valgrind) will detect the error.
 */

/*
 * <structure definitions for \impcore>=
 */
struct Exp {
    Expalt alt;
    union {
        Value literal;
        Name var;
        struct { Name name; Exp exp; } set;
        struct { Exp cond; Exp truex; Exp falsex; } ifx;
        struct { Exp cond; Exp exp; } whilex;
        Explist begin;
        struct { Name name; Explist actuals; } apply;
    } u;
};

/*
 * <structure definitions for \impcore>=
 */
struct Userfun { Namelist formals; Exp body; }; 
struct Def {
    Defalt alt;
    union {
        struct { Name name; Exp exp; } val;
        Exp exp;
        struct { Name name; Userfun userfun; } define;
    } u;
};

/*
 * <structure definitions for \impcore>=
 */
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

/*
 * <structure definitions for \impcore>=
 */
struct Fun { Funalt alt; union { Userfun userdef; Name primitive; } u; }; 
/*
 * <structure definitions for \impcore>=
 */
struct Parlist {
   Par hd;
   struct Parlist *tl;
};

struct UnitTestlist {
   UnitTest hd;
   struct UnitTestlist *tl;
};

struct Explist {
   Exp hd;
   struct Explist *tl;
};

struct Namelist {
   Name hd;
   struct Namelist *tl;
};

struct Valuelist {
   Value hd;
   struct Valuelist *tl;
};

struct Funlist {
   Fun hd;
   struct Funlist *tl;
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
 * <shared structure definitions>=
 */
struct Par { Paralt alt; union { Name atom; Parlist list; } u; }; 

/*
 * A new environment may be created by passing a list of
 * names and a list of associated values or function
 * definitions. For example, mkValenv(<x_1, ..., x_n>,
 * <v_1, ..., v_n>) returns the environment {x_1 |->v_1,
 * ..., x_n |->v_n}. If the two lists are not the same
 * length, it is a checked run-time error.
 * <function prototypes for \impcore>=
 */
Valenv mkValenv(Namelist vars, Valuelist vals);
Funenv mkFunenv(Namelist vars, Funlist   defs);
/*
 * To retrieve a value or function definition, we use
 * [[fetchval]] or [[fetchfun]]. In the operational
 * semantics, I write the lookup fetchval(x, rho) simply
 * as rho(x). \iiflabelfetchval\iiflabelfetchfun
 * <function prototypes for \impcore>=
 */
Value fetchval(Name name, Valenv env);
Fun   fetchfun(Name name, Funenv env);
/*
 * If the given name does not appear in the environment,
 * it is a checked run-time error. To avoid such errors,
 * we can call [[isvalbound]] or [[isfunbound]]; they
 * return [[1]] if the given name is in the environment,
 * and [[0]] otherwise. Formally, isvalbound(x, rho) is
 * written x in dom rho. \iiflabelisvalbound\iiflabel
 * isfunbound
 */

/*
 * <function prototypes for \impcore>=
 */
bool isvalbound(Name name, Valenv env);
bool isfunbound(Name name, Funenv env);
/*
 * To add new bindings to an environment, use
 * [[bindval]] and [[bindfun]]. Unlike previous
 * operations on environments, [[bindval]] and
 * [[bindfun]] cannot be specified as pure functions.
 * Instead, [[bindval]] and [[bindfun]] mutate their
 * environments, replacing the old bindings with new
 * ones. Calling bindval(x, v, rho) is equivalent to
 * performing the assignment rho := rho{x |->v}. Because
 * rho is a mutable abstraction, the caller can see the
 * modifications to the environment. \iiflabelbindval\
 * iiflabelbindfun
 */

/*
 * <function prototypes for \impcore>=
 */
void bindval(Name name, Value val, Valenv env);
void bindfun(Name name, Fun   fun, Funenv env);
/*
 * These functions can be used to replace existing
 * bindings or to add new ones.
 */

/*
 * Function [[eval]] corresponds to the ==> relation in
 * our operational semantics. For example, eval(e, xi, 
 * phi, rho) finds a v, xi', and rho' such that \evale =
 * =>\eval[']v, assigns rho := rho' and xi := xi', and
 * returns v. The function [[evaldef]] similarly
 * corresponds to the --> relation. \iintlabeleval\
 * iintlabelevaldef
 * <function prototypes for \impcore>=
 */
Value eval   (Exp e, Valenv globals, Funenv functions, Valenv formals);
void  evaldef(Def d, Valenv globals, Funenv functions, Echo echo_level);
/*
 * If the [[echo_level]] parameter to [[evaldef]] is
 * [[ECHOES]], [[evaldef]] prints the values and names
 * of top-level expressions and functions. If 
 * [[echo_level]] is [[NO_ECHOES]], nothing is printed.
 */

/*
 * Function [[readevalprint]] consumes a stream of
 * extended definitions. It evaluates each true
 * definition, remembers each unit test, and calls
 * itself recursively on each [[use]]. When the stream
 * of extended definitions is exhausted,
 * [[readevalprint]] runs the remembered unit tests.\
 * iintlabelreadevalprint
 * <function prototypes for \impcore>=
 */
void readevalprint(XDefstream s, Valenv globals, Funenv functions, Echo
                                                                    echo_level);
/*
 * As with [[evaldef]], the [[echo_level]] parameter
 * controls whether [[readevalprint]] prints the values
 * and names of top-level expressions and functions.\
 * intlabelEcho
 */

/*
 * <function prototypes for \impcore>=
 */
void process_tests(UnitTestlist tests, Valenv globals, Funenv functions);
/*
 * <function prototypes for \impcore>=
 */
int number_of_good_tests(UnitTestlist tests, Valenv globals, Funenv functions);
/*
 * <function prototypes for \impcore>=
 */
TestResult test_result(UnitTest t, Valenv globals, Funenv functions);
/*
 * <function prototypes for \impcore>=
 */
Exp mkLiteral(Value literal);
Exp mkVar(Name var);
Exp mkSet(Name name, Exp exp);
Exp mkIfx(Exp cond, Exp truex, Exp falsex);
Exp mkWhilex(Exp cond, Exp exp);
Exp mkBegin(Explist begin);
Exp mkApply(Name name, Explist actuals);
struct Exp mkLiteralStruct(Value literal);
struct Exp mkVarStruct(Name var);
struct Exp mkSetStruct(Name name, Exp exp);
struct Exp mkIfxStruct(Exp cond, Exp truex, Exp falsex);
struct Exp mkWhilexStruct(Exp cond, Exp exp);
struct Exp mkBeginStruct(Explist begin);
struct Exp mkApplyStruct(Name name, Explist actuals);
/*
 * <function prototypes for \impcore>=
 */
Userfun mkUserfun(Namelist formals, Exp body);
Def mkVal(Name name, Exp exp);
Def mkExp(Exp exp);
Def mkDefine(Name name, Userfun userfun);
struct Def mkValStruct(Name name, Exp exp);
struct Def mkExpStruct(Exp exp);
struct Def mkDefineStruct(Name name, Userfun userfun);
/*
 * <function prototypes for \impcore>=
 */
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
/*
 * <function prototypes for \impcore>=
 */
Fun mkUserdef(Userfun userdef);
Fun mkPrimitive(Name primitive);
/*
 * <function prototypes for \impcore>=
 */
int     lengthPL(Parlist ps);
Par     nthPL   (Parlist ps, unsigned n);
Parlist mkPL    (Par p, Parlist ps);
Parlist popPL   (Parlist ps);
Printer printparlist;

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

int      lengthNL(Namelist ns);
Name     nthNL   (Namelist ns, unsigned n);
Namelist mkNL    (Name n, Namelist ns);
Namelist popNL   (Namelist ns);
Printer  printnamelist;

int       lengthVL(Valuelist vs);
Value     nthVL   (Valuelist vs, unsigned n);
Valuelist mkVL    (Value v, Valuelist vs);
Valuelist popVL   (Valuelist vs);
Printer   printvaluelist;

int     lengthFL(Funlist fs);
Fun     nthFL   (Funlist fs, unsigned n);
Funlist mkFL    (Fun f, Funlist fs);
Funlist popFL   (Funlist fs);
Printer printfunlist;

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
 * <shared function prototypes>=
 */
Par mkAtom(Name atom);
Par mkList(Parlist list);
struct Par mkAtomStruct(Name atom);
struct Par mkListStruct(Parlist list);

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

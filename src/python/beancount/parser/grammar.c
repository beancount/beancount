/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 11 "src/python/beancount/parser/grammar.y" /* yacc.c:339  */


#include <stdio.h>
#include <assert.h>
#include "parser.h"
#include "lexer.h"


/*
 * Call a builder method and detect and handle a Python exception being raised
 * in the handler. Always run the code to clean the references provided by the
 * reduced rule. {05bb0fb60e86}
 */
#define BUILDY(clean, target, method_name, format, ...)                         \
    target = PyObject_CallMethod(builder, method_name, format, __VA_ARGS__);    \
    clean;                                                                      \
    if (target == NULL) {                                                       \
        build_grammar_error_from_exception();                                   \
        YYERROR;                                                                \
    }


/* First line of reported file/line string. This is used as #line. */
int yy_firstline;

#define FILE_LINE_ARGS  yy_filename, ((yyloc).first_line + yy_firstline)


/* Build a grammar error from the exception context. */
void build_grammar_error_from_exception(void)
{
    /* TRACE_ERROR("Grammar Builder Exception"); */

    /* Get the exception context. */
    PyObject* ptype;
    PyObject* pvalue;
    PyObject* ptraceback;
    PyErr_Fetch(&ptype, &pvalue, &ptraceback);
    PyErr_NormalizeException(&ptype, &pvalue, &ptraceback);

    /* Clear the exception. */
    PyErr_Clear();

    if (pvalue != NULL) {
        /* Build and accumulate a new error object. {27d1d459c5cd} */
        PyObject* rv = PyObject_CallMethod(builder, "build_grammar_error", "siOO",
                                           yy_filename, yylineno + yy_firstline,
                                           pvalue, ptype);
        Py_DECREF(ptype);
        Py_DECREF(pvalue);
        Py_DECREF(ptraceback);

        if (rv == NULL) {
            PyErr_SetString(PyExc_RuntimeError,
                            "Internal error: While building exception");
        }
    }
    else {
        PyErr_SetString(PyExc_RuntimeError,
                        "Internal error: No exception");
    }
}



/* Error-handling function. {ca6aab8b9748} */
void yyerror(char const* message)
{
    /* Skip lex errors: they have already been registered the lexer itself. */
    if (strstr(message, "LEX_ERROR") != NULL) {
        return;
    }
    else {
        /* Register a syntax error with the builder. */
        PyObject* rv = PyObject_CallMethod(builder, "build_grammar_error", "sis",
                                           yy_filename, yylineno + yy_firstline,
                                           message);
        if (rv == NULL) {
            PyErr_SetString(PyExc_RuntimeError,
                            "Internal error: Building exception from yyerror()");
        }
        Py_XDECREF(rv);
    }
}

/* Get a printable version of a token name. */
const char* getTokenName(int token);


/* Macros to clean up memory for temporaries in rule reductions. */
#define DECREF1(x1)                        Py_DECREF(x1);
#define DECREF2(x1, x2)                    DECREF1(x1); Py_DECREF(x2);
#define DECREF3(x1, x2, x3)                DECREF2(x1, x2); Py_DECREF(x3);
#define DECREF4(x1, x2, x3, x4)            DECREF3(x1, x2, x3); Py_DECREF(x4);
#define DECREF5(x1, x2, x3, x4, x5)        DECREF4(x1, x2, x3, x4); Py_DECREF(x5);
#define DECREF6(x1, x2, x3, x4, x5, x6)    DECREF5(x1, x2, x3, x4, x5); Py_DECREF(x6);


#line 165 "src/python/beancount/parser/grammar.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* In a future release of Bison, this section will be replaced
   by #include "grammar.h".  */
#ifndef YY_YY_SRC_PYTHON_BEANCOUNT_PARSER_GRAMMAR_H_INCLUDED
# define YY_YY_SRC_PYTHON_BEANCOUNT_PARSER_GRAMMAR_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    LEX_ERROR = 258,
    INDENT = 259,
    EOL = 260,
    COMMENT = 261,
    SKIPPED = 262,
    PIPE = 263,
    ATAT = 264,
    AT = 265,
    LCURLCURL = 266,
    RCURLCURL = 267,
    LCURL = 268,
    RCURL = 269,
    EQUAL = 270,
    COMMA = 271,
    TILDE = 272,
    HASH = 273,
    ASTERISK = 274,
    SLASH = 275,
    PLUS = 276,
    MINUS = 277,
    LPAREN = 278,
    RPAREN = 279,
    FLAG = 280,
    TXN = 281,
    BALANCE = 282,
    OPEN = 283,
    CLOSE = 284,
    COMMODITY = 285,
    PAD = 286,
    EVENT = 287,
    PRICE = 288,
    NOTE = 289,
    DOCUMENT = 290,
    PUSHTAG = 291,
    POPTAG = 292,
    OPTION = 293,
    INCLUDE = 294,
    PLUGIN = 295,
    BOOL = 296,
    DATE = 297,
    ACCOUNT = 298,
    CURRENCY = 299,
    STRING = 300,
    NUMBER = 301,
    TAG = 302,
    LINK = 303,
    KEY = 304,
    NEGATIVE = 305
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 125 "src/python/beancount/parser/grammar.y" /* yacc.c:355  */

    char character;
    const char* string;
    PyObject* pyobj;
    struct {
        PyObject* pyobj1;
        PyObject* pyobj2;
    } pairobj;

#line 266 "src/python/beancount/parser/grammar.c" /* yacc.c:355  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif

/* Location type.  */
#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif



int yyparse (void);

#endif /* !YY_YY_SRC_PYTHON_BEANCOUNT_PARSER_GRAMMAR_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 296 "src/python/beancount/parser/grammar.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
             && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE) + sizeof (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  33
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   197

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  51
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  43
/* YYNRULES -- Number of rules.  */
#define YYNRULES  107
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  190

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   305

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   243,   243,   246,   250,   254,   259,   260,   264,   265,
     266,   267,   268,   274,   278,   283,   288,   293,   298,   303,
     307,   312,   320,   325,   330,   335,   342,   348,   352,   356,
     358,   363,   368,   373,   379,   385,   386,   387,   388,   389,
     390,   391,   392,   396,   402,   407,   412,   418,   423,   429,
     434,   439,   445,   451,   457,   464,   468,   474,   480,   486,
     492,   498,   504,   512,   519,   524,   529,   534,   541,   546,
     552,   558,   563,   568,   574,   578,   582,   588,   594,   600,
     606,   608,   614,   615,   616,   617,   618,   619,   620,   621,
     622,   623,   628,   634,   640,   645,   651,   652,   653,   654,
     655,   656,   657,   660,   664,   669,   687,   694
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "LEX_ERROR", "INDENT", "EOL", "COMMENT",
  "SKIPPED", "PIPE", "ATAT", "AT", "LCURLCURL", "RCURLCURL", "LCURL",
  "RCURL", "EQUAL", "COMMA", "TILDE", "HASH", "ASTERISK", "SLASH", "PLUS",
  "MINUS", "LPAREN", "RPAREN", "FLAG", "TXN", "BALANCE", "OPEN", "CLOSE",
  "COMMODITY", "PAD", "EVENT", "PRICE", "NOTE", "DOCUMENT", "PUSHTAG",
  "POPTAG", "OPTION", "INCLUDE", "PLUGIN", "BOOL", "DATE", "ACCOUNT",
  "CURRENCY", "STRING", "NUMBER", "TAG", "LINK", "KEY", "NEGATIVE",
  "$accept", "empty", "txn", "eol", "empty_line", "number_expr",
  "txn_fields", "transaction", "optflag", "posting", "key_value",
  "key_value_value", "posting_or_kv_list", "key_value_list",
  "currency_list", "pushtag", "poptag", "open", "opt_booking", "close",
  "commodity", "pad", "balance", "amount", "amount_tolerance",
  "maybe_number", "compound_amount", "position", "lot_spec",
  "lot_comp_list", "lot_comp", "price", "event", "note", "filename",
  "document", "entry", "option", "include", "plugin", "directive",
  "declarations", "file", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305
};
# endif

#define YYPACT_NINF -126

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-126)))

#define YYTABLE_NINF -108

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -126,  -126,   110,    12,  -126,    14,  -126,    21,  -126,   -17,
     -10,   -40,    -4,     2,   162,  -126,  -126,  -126,  -126,  -126,
    -126,  -126,  -126,  -126,  -126,  -126,  -126,  -126,  -126,  -126,
    -126,  -126,  -126,  -126,  -126,  -126,   107,   107,    23,   107,
       5,  -126,  -126,  -126,    13,    30,    40,    41,    54,    74,
      76,    75,    82,  -126,  -126,   124,  -126,  -126,   107,  -126,
     107,  -126,    -6,    86,   107,   107,    89,    92,    -6,    93,
      96,  -126,     1,  -126,  -126,  -126,    -6,    -6,    -6,  -126,
      50,   107,  -126,  -126,   -14,  -126,  -126,   107,   107,    87,
     107,   107,  -126,   107,  -126,  -126,  -126,  -126,  -126,  -126,
    -126,    67,    -6,    -6,    -6,    -6,    -6,  -126,  -126,   100,
    -126,  -126,   107,  -126,   160,   160,  -126,  -126,  -126,  -126,
    -126,  -126,  -126,   166,  -126,   141,  -126,  -126,    73,    73,
     160,  -126,  -126,   102,  -126,   160,   160,   160,   160,   160,
     -11,  -126,  -126,  -126,   160,   112,  -126,  -126,  -126,   128,
    -126,  -126,  -126,  -126,  -126,  -126,  -126,    87,   107,  -126,
      59,  -126,  -126,   159,    90,    81,  -126,    -6,    -6,  -126,
    -126,  -126,     7,   147,    -5,  -126,    11,  -126,   107,   107,
      -6,  -126,  -126,    81,  -126,  -126,  -126,   129,  -126,  -126
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,   106,     0,     0,   105,    11,     8,    12,    96,     0,
       0,     0,     0,     0,     0,    97,    82,    98,    99,    84,
      85,    91,    86,    83,    90,    87,    88,    89,   104,   100,
     101,   102,   103,     1,    10,     9,     0,     0,     0,     0,
       0,     5,     4,     3,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     2,     6,     0,    52,    53,     0,    93,
       0,    94,     0,     2,     0,     0,     0,     0,     0,     0,
       0,    21,     0,     7,    92,    95,     0,     0,     0,    13,
       0,     0,    50,    49,     2,     2,     2,     0,     0,     0,
       0,     0,    80,     0,    25,    22,    24,    23,     2,    19,
      18,     0,     0,     0,     0,     0,     0,    62,     2,     0,
      55,    56,     0,    47,    57,    58,     2,     2,    61,     2,
       2,     2,    44,    26,    20,     0,    16,    17,    14,    15,
      60,    51,     2,     0,    48,    59,    78,    77,    79,    81,
       2,    46,    45,    63,    54,     2,    28,    29,    27,     0,
      40,    37,    36,    38,    35,    39,    43,    41,     0,    42,
       0,    34,    33,    68,     0,     2,    69,     0,     0,    30,
      75,    76,    64,    65,     0,    74,     0,    72,     0,     0,
       2,    66,    70,     2,    32,    31,    64,     0,    73,    67
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
    -126,     0,  -126,   -36,  -126,   -44,  -126,  -126,  -126,  -126,
      51,  -126,  -126,   -42,  -126,  -126,  -126,  -126,  -126,  -126,
    -126,  -126,  -126,  -125,  -126,    -3,  -126,  -126,  -126,  -126,
      -8,  -126,  -126,  -126,  -126,  -126,  -126,  -126,  -126,  -126,
    -126,  -126,  -126
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,   113,    53,    56,    15,    89,    72,    16,   149,   141,
     134,   158,   123,   114,    84,    17,    18,    19,   112,    20,
      21,    22,    23,    90,    81,   174,   175,   164,   166,   176,
     177,    24,    25,    26,    93,    27,    28,    29,    30,    31,
      32,     2,     3
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
       1,    57,   109,    59,    61,    38,    54,    55,   146,    94,
      54,    55,    33,   180,   147,    76,    77,    78,    80,    34,
     159,   -71,    74,   -71,    75,   182,    35,   183,    85,    86,
      36,   110,    99,   100,   101,   163,    98,    37,   145,   181,
      79,    39,   178,   179,   115,   108,    95,    40,    96,    97,
      60,   116,   117,    71,   119,   120,    62,   121,   125,   126,
     127,   128,   129,    83,    54,    55,   130,   102,    58,   103,
     104,   105,   106,    63,   135,   136,   132,   137,   138,   139,
      76,    77,    78,    64,   111,    65,   103,   104,   105,   106,
     144,   124,   103,   104,   107,    54,    55,    66,   122,   167,
     168,   157,    76,    77,    78,    79,   103,   104,   105,   106,
    -107,     4,    54,    55,     5,     6,     7,     8,    69,    67,
      68,   173,   161,   170,   162,    70,   171,    79,   169,    73,
      82,   118,    87,    76,    77,    78,   173,    88,    91,   173,
     148,    92,   184,   185,   131,   156,     9,    10,    11,    12,
      13,   145,    14,   150,   151,   152,   153,   154,    79,   155,
     103,   104,   105,   106,   133,   172,   103,   104,   105,   106,
     140,   160,   165,   189,   142,   188,     0,   187,     0,     0,
     186,    41,     0,   186,     0,   143,     0,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52
};

static const yytype_int16 yycheck[] =
{
       0,    37,    16,    39,    40,    45,     5,     6,    19,     8,
       5,     6,     0,    18,    25,    21,    22,    23,    62,     5,
     145,    14,    58,    16,    60,    14,     5,    16,    64,    65,
      47,    45,    76,    77,    78,   160,    72,    47,    49,    44,
      46,    45,   167,   168,    86,    81,    45,    45,    47,    48,
      45,    87,    88,    53,    90,    91,    43,    93,   102,   103,
     104,   105,   106,    63,     5,     6,   108,    17,    45,    19,
      20,    21,    22,    43,   116,   117,   112,   119,   120,   121,
      21,    22,    23,    43,    84,    44,    19,    20,    21,    22,
     132,    24,    19,    20,    44,     5,     6,    43,    98,     9,
      10,   145,    21,    22,    23,    46,    19,    20,    21,    22,
       0,     1,     5,     6,     4,     5,     6,     7,    43,    45,
      44,   165,   158,    42,   160,    43,    45,    46,   164,     5,
      44,    44,    43,    21,    22,    23,   180,    45,    45,   183,
     140,    45,   178,   179,    44,   145,    36,    37,    38,    39,
      40,    49,    42,    41,    42,    43,    44,    45,    46,    47,
      19,    20,    21,    22,     4,   165,    19,    20,    21,    22,
       4,    43,    13,    44,   123,   183,    -1,   180,    -1,    -1,
     180,    19,    -1,   183,    -1,    44,    -1,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    52,    92,    93,     1,     4,     5,     6,     7,    36,
      37,    38,    39,    40,    42,    55,    58,    66,    67,    68,
      70,    71,    72,    73,    82,    83,    84,    86,    87,    88,
      89,    90,    91,     0,     5,     5,    47,    47,    45,    45,
      45,    19,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    53,     5,     6,    54,    54,    45,    54,
      45,    54,    43,    43,    43,    44,    43,    45,    44,    43,
      43,    52,    57,     5,    54,    54,    21,    22,    23,    46,
      56,    75,    44,    52,    65,    54,    54,    43,    45,    56,
      74,    45,    45,    85,     8,    45,    47,    48,    54,    56,
      56,    56,    17,    19,    20,    21,    22,    44,    54,    16,
      45,    52,    69,    52,    64,    64,    54,    54,    44,    54,
      54,    54,    52,    63,    24,    56,    56,    56,    56,    56,
      64,    44,    54,     4,    61,    64,    64,    64,    64,    64,
       4,    60,    61,    44,    64,    49,    19,    25,    52,    59,
      41,    42,    43,    44,    45,    47,    52,    56,    62,    74,
      43,    54,    54,    74,    78,    13,    79,     9,    10,    54,
      42,    45,    52,    56,    76,    77,    80,    81,    74,    74,
      18,    44,    14,    16,    54,    54,    52,    76,    81,    44
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    51,    52,    53,    53,    53,    54,    54,    55,    55,
      55,    55,    55,    56,    56,    56,    56,    56,    56,    56,
      56,    57,    57,    57,    57,    57,    58,    59,    59,    59,
      60,    60,    60,    60,    61,    62,    62,    62,    62,    62,
      62,    62,    62,    62,    63,    63,    63,    64,    64,    65,
      65,    65,    66,    67,    68,    69,    69,    70,    71,    72,
      73,    74,    75,    75,    76,    76,    77,    77,    78,    78,
      79,    80,    80,    80,    81,    81,    81,    82,    83,    84,
      85,    86,    87,    87,    87,    87,    87,    87,    87,    87,
      87,    87,    88,    89,    90,    90,    91,    91,    91,    91,
      91,    91,    91,    92,    92,    92,    92,    93
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     1,     1,     1,     1,     2,     1,     2,
       2,     1,     1,     1,     3,     3,     3,     3,     2,     2,
       3,     1,     2,     2,     2,     2,     5,     1,     1,     1,
       5,     7,     7,     4,     4,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     1,     2,     1,
       1,     3,     3,     3,     7,     1,     1,     5,     5,     6,
       6,     2,     2,     4,     1,     1,     2,     4,     1,     2,
       3,     1,     1,     3,     1,     1,     1,     6,     6,     6,
       1,     6,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     4,     3,     3,     4,     1,     1,     1,     1,
       1,     1,     1,     2,     2,     2,     1,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (0)
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

YY_ATTRIBUTE_UNUSED
static unsigned
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  unsigned res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += YYFPRINTF (yyo, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += YYFPRINTF (yyo, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += YYFPRINTF (yyo, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += YYFPRINTF (yyo, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += YYFPRINTF (yyo, "-%d", end_col);
    }
  return res;
 }

#  define YY_LOCATION_PRINT(File, Loc)          \
  yy_location_print_ (File, &(Loc))

# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value, Location); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  YYUSE (yylocationp);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  YY_LOCATION_PRINT (yyoutput, *yylocationp);
  YYFPRINTF (yyoutput, ": ");
  yy_symbol_value_print (yyoutput, yytype, yyvaluep, yylocationp);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                       , &(yylsp[(yyi + 1) - (yynrhs)])                       );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, YYLTYPE *yylocationp)
{
  YYUSE (yyvaluep);
  YYUSE (yylocationp);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
/* The lookahead symbol.  */
int yychar;


/* The semantic value of the lookahead symbol.  */
/* Default value used for initialization, for pacifying older GCCs
   or non-GCC compilers.  */
YY_INITIAL_VALUE (static YYSTYPE yyval_default;)
YYSTYPE yylval YY_INITIAL_VALUE (= yyval_default);

/* Location data for the lookahead symbol.  */
static YYLTYPE yyloc_default
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;
YYLTYPE yylloc = yyloc_default;

    /* Number of syntax errors so far.  */
    int yynerrs;

    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.
       'yyls': related to locations.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    /* The location stack.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls;
    YYLTYPE *yylsp;

    /* The locations where the error started and ended.  */
    YYLTYPE yyerror_range[3];

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yylsp = yyls = yylsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  yylsp[0] = yylloc;
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yyls1, yysize * sizeof (*yylsp),
                    &yystacksize);

        yyls = yyls1;
        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
        YYSTACK_RELOCATE (yyls_alloc, yyls);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex (&yylval, &yylloc);
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
  *++yylsp = yylloc;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

  /* Default location.  */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 3:
#line 247 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
        (yyval.character) = '*';
    }
#line 1618 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 4:
#line 251 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
        (yyval.character) = (yyvsp[0].character);
    }
#line 1626 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 5:
#line 255 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
        (yyval.character) = '*';
    }
#line 1634 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 13:
#line 275 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                (yyval.pyobj) = (yyvsp[0].pyobj);
            }
#line 1642 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 14:
#line 279 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                (yyval.pyobj) = PyNumber_Add((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
                DECREF2((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 1651 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 15:
#line 284 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                (yyval.pyobj) = PyNumber_Subtract((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
                DECREF2((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 1660 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 16:
#line 289 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                (yyval.pyobj) = PyNumber_Multiply((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
                DECREF2((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 1669 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 17:
#line 294 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                (yyval.pyobj) = PyNumber_TrueDivide((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
                DECREF2((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 1678 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 18:
#line 299 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                (yyval.pyobj) = PyNumber_Negative((yyvsp[0].pyobj));
                DECREF1((yyvsp[0].pyobj));
            }
#line 1687 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 19:
#line 304 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                (yyval.pyobj) = (yyvsp[0].pyobj);
            }
#line 1695 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 20:
#line 308 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                (yyval.pyobj) = (yyvsp[-1].pyobj);
            }
#line 1703 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 21:
#line 313 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
               /* Note: We're passing a bogus value here in order to avoid
                * having to declare a second macro just for this one special
                * case. */
               BUILDY(,
                      (yyval.pyobj), "txn_field_new", "O", Py_None);
           }
#line 1715 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 22:
#line 321 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
               BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                      (yyval.pyobj), "txn_field_STRING", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
           }
#line 1724 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 23:
#line 326 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
               BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                      (yyval.pyobj), "txn_field_LINK", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
           }
#line 1733 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 24:
#line 331 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
               BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                      (yyval.pyobj), "txn_field_TAG", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
           }
#line 1742 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 25:
#line 336 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
               /* Mark PIPE as present for backwards compatibility and raise an error */
               BUILDY(DECREF1((yyvsp[-1].pyobj)),
                      (yyval.pyobj), "txn_field_PIPE", "OO", (yyvsp[-1].pyobj), Py_None);
           }
#line 1752 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 26:
#line 343 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                BUILDY(DECREF3((yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                       (yyval.pyobj), "transaction", "siObOO", FILE_LINE_ARGS, (yyvsp[-4].pyobj), (yyvsp[-3].character), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 1761 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 27:
#line 349 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
            (yyval.character) = '\0';
        }
#line 1769 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 28:
#line 353 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
            (yyval.character) = '*';
        }
#line 1777 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 30:
#line 359 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
            BUILDY(DECREF2((yyvsp[-2].pyobj), (yyvsp[-1].pyobj)),
                   (yyval.pyobj), "posting", "siOOOOb", FILE_LINE_ARGS, (yyvsp[-2].pyobj), (yyvsp[-1].pyobj), Py_None, Py_False, (yyvsp[-3].character));
        }
#line 1786 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 31:
#line 364 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
            BUILDY(DECREF3((yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj)),
                   (yyval.pyobj), "posting", "siOOOOb", FILE_LINE_ARGS, (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj), Py_False, (yyvsp[-5].character));
        }
#line 1795 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 32:
#line 369 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
            BUILDY(DECREF3((yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj)),
                   (yyval.pyobj), "posting", "siOOOOb", FILE_LINE_ARGS, (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj), Py_True, (yyvsp[-5].character));
        }
#line 1804 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 33:
#line 374 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
            BUILDY(DECREF1((yyvsp[-1].pyobj)),
                   (yyval.pyobj), "posting", "siOOOOb", FILE_LINE_ARGS, (yyvsp[-1].pyobj), Py_None, Py_None, Py_False, (yyvsp[-2].character));
        }
#line 1813 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 34:
#line 380 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
              BUILDY(DECREF2((yyvsp[-2].pyobj), (yyvsp[-1].pyobj)),
                     (yyval.pyobj), "key_value", "OO", (yyvsp[-2].pyobj), (yyvsp[-1].pyobj));
          }
#line 1822 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 42:
#line 393 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                    (yyval.pyobj) = (yyvsp[0].pyobj);
                }
#line 1830 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 43:
#line 397 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                    Py_INCREF(Py_None);
                    (yyval.pyobj) = Py_None;
                }
#line 1839 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 44:
#line 403 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                       Py_INCREF(Py_None);
                       (yyval.pyobj) = Py_None;
                   }
#line 1848 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 45:
#line 408 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                       BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                              (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                   }
#line 1857 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 46:
#line 413 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                       BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                              (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                   }
#line 1866 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 47:
#line 419 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                   Py_INCREF(Py_None);
                   (yyval.pyobj) = Py_None;
               }
#line 1875 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 48:
#line 424 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                   BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                          (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
               }
#line 1884 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 49:
#line 430 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                  Py_INCREF(Py_None);
                  (yyval.pyobj) = Py_None;
              }
#line 1893 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 50:
#line 435 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                  BUILDY(DECREF1((yyvsp[0].pyobj)),
                         (yyval.pyobj), "handle_list", "OO", Py_None, (yyvsp[0].pyobj));
              }
#line 1902 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 51:
#line 440 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                  BUILDY(DECREF2((yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                         (yyval.pyobj), "handle_list", "OO", (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
              }
#line 1911 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 52:
#line 446 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
             BUILDY(DECREF1((yyvsp[-1].pyobj)),
                    (yyval.pyobj), "pushtag", "O", (yyvsp[-1].pyobj));
         }
#line 1920 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 53:
#line 452 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
           BUILDY(DECREF1((yyvsp[-1].pyobj)),
                  (yyval.pyobj), "poptag", "O", (yyvsp[-1].pyobj));
       }
#line 1929 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 54:
#line 458 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
         BUILDY(DECREF5((yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                (yyval.pyobj), "open", "siOOOOO", FILE_LINE_ARGS, (yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
         ;
     }
#line 1939 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 55:
#line 465 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                (yyval.pyobj) = (yyvsp[0].pyobj);
            }
#line 1947 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 56:
#line 469 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                Py_INCREF(Py_None);
                (yyval.pyobj) = Py_None;
            }
#line 1956 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 57:
#line 475 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
          BUILDY(DECREF3((yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                 (yyval.pyobj), "close", "siOOO", FILE_LINE_ARGS, (yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      }
#line 1965 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 58:
#line 481 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
              BUILDY(DECREF3((yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                     (yyval.pyobj), "commodity", "siOOO", FILE_LINE_ARGS, (yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
          }
#line 1974 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 59:
#line 487 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
        BUILDY(DECREF4((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "pad", "siOOOO", FILE_LINE_ARGS, (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
    }
#line 1983 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 60:
#line 493 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
            BUILDY(DECREF5((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[0].pyobj), (yyvsp[-2].pairobj).pyobj1, (yyvsp[-2].pairobj).pyobj2),
                   (yyval.pyobj), "balance", "siOOOOO", FILE_LINE_ARGS, (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pairobj).pyobj1, (yyvsp[-2].pairobj).pyobj2, (yyvsp[0].pyobj));
        }
#line 1992 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 61:
#line 499 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
           BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                  (yyval.pyobj), "amount", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
       }
#line 2001 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 62:
#line 505 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                     BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                            (yyval.pairobj).pyobj1, "amount", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                     (yyval.pairobj).pyobj2 = Py_None;
                     Py_INCREF(Py_None);
                     ;
                 }
#line 2013 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 63:
#line 513 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                     BUILDY(DECREF2((yyvsp[-3].pyobj), (yyvsp[0].pyobj)),
                            (yyval.pairobj).pyobj1, "amount", "OO", (yyvsp[-3].pyobj), (yyvsp[0].pyobj));
                     (yyval.pairobj).pyobj2 = (yyvsp[-1].pyobj);
                 }
#line 2023 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 64:
#line 520 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                 Py_INCREF(Py_None);
                 (yyval.pyobj) = Py_None;
             }
#line 2032 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 65:
#line 525 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                 (yyval.pyobj) = (yyvsp[0].pyobj);
             }
#line 2040 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 66:
#line 530 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                    BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                           (yyval.pyobj), "compound_amount", "OOO", (yyvsp[-1].pyobj), Py_None, (yyvsp[0].pyobj));
                }
#line 2049 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 67:
#line 535 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                    BUILDY(DECREF3((yyvsp[-3].pyobj), (yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                           (yyval.pyobj), "compound_amount", "OOO", (yyvsp[-3].pyobj), (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                    ;
                }
#line 2059 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 68:
#line 542 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
             BUILDY(DECREF1((yyvsp[0].pyobj)),
                    (yyval.pyobj), "position", "siOO", FILE_LINE_ARGS, (yyvsp[0].pyobj), Py_None);
         }
#line 2068 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 69:
#line 547 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
             BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                    (yyval.pyobj), "position", "siOO", FILE_LINE_ARGS, (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
         }
#line 2077 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 70:
#line 553 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
             BUILDY(DECREF1((yyvsp[-1].pyobj)),
                    (yyval.pyobj), "lot_spec", "O", (yyvsp[-1].pyobj));
         }
#line 2086 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 71:
#line 559 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                  Py_INCREF(Py_None);
                  (yyval.pyobj) = Py_None;
              }
#line 2095 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 72:
#line 564 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                  BUILDY(DECREF1((yyvsp[0].pyobj)),
                         (yyval.pyobj), "handle_list", "OO", Py_None, (yyvsp[0].pyobj));
              }
#line 2104 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 73:
#line 569 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                  BUILDY(DECREF2((yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                         (yyval.pyobj), "handle_list", "OO", (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
              }
#line 2113 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 74:
#line 575 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
             (yyval.pyobj) = (yyvsp[0].pyobj);
         }
#line 2121 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 75:
#line 579 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
             (yyval.pyobj) = (yyvsp[0].pyobj);
         }
#line 2129 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 76:
#line 583 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
             (yyval.pyobj) = (yyvsp[0].pyobj);
         }
#line 2137 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 77:
#line 589 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
          BUILDY(DECREF4((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                 (yyval.pyobj), "price", "siOOOO", FILE_LINE_ARGS, (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      }
#line 2146 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 78:
#line 595 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
          BUILDY(DECREF4((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                 (yyval.pyobj), "event", "siOOOO", FILE_LINE_ARGS, (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      }
#line 2155 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 79:
#line 601 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
          BUILDY(DECREF4((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                 (yyval.pyobj), "note", "siOOOO", FILE_LINE_ARGS, (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      }
#line 2164 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 81:
#line 609 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
             BUILDY(DECREF4((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                    (yyval.pyobj), "document", "siOOOO", FILE_LINE_ARGS, (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
         }
#line 2173 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 91:
#line 624 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
          (yyval.pyobj) = (yyvsp[0].pyobj);
      }
#line 2181 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 92:
#line 629 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
           BUILDY(DECREF2((yyvsp[-2].pyobj), (yyvsp[-1].pyobj)),
                  (yyval.pyobj), "option", "siOO", FILE_LINE_ARGS, (yyvsp[-2].pyobj), (yyvsp[-1].pyobj));
       }
#line 2190 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 93:
#line 635 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
           BUILDY(DECREF1((yyvsp[-1].pyobj)),
                  (yyval.pyobj), "include", "siO", FILE_LINE_ARGS, (yyvsp[-1].pyobj));
       }
#line 2199 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 94:
#line 641 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
           BUILDY(DECREF1((yyvsp[-1].pyobj)),
                  (yyval.pyobj), "plugin", "siOO", FILE_LINE_ARGS, (yyvsp[-1].pyobj), Py_None);
       }
#line 2208 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 95:
#line 646 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
           BUILDY(DECREF2((yyvsp[-2].pyobj), (yyvsp[-1].pyobj)),
                  (yyval.pyobj), "plugin", "siOO", FILE_LINE_ARGS, (yyvsp[-2].pyobj), (yyvsp[-1].pyobj));
       }
#line 2217 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 103:
#line 661 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                 (yyval.pyobj) = (yyvsp[-1].pyobj);
             }
#line 2225 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 104:
#line 665 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                 BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                        (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
             }
#line 2234 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 105:
#line 670 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                 /*
                  * Ignore the error and continue reducing ({3d95e55b654e}).
                  * Note that with the matching rule above, "error" will
                  * successfully reduce on each line that cannot reduce.
                  * Non-erroneous postings after an error occurs will reduce but
                  * not be included because a transaction's list of postings
                  * does not include an "error" rule.
                  *
                  * Note: Adding EOL after the "error" rule above works to
                  * reduce the number of calls to this rule resulting from the
                  * appearance of a LEX_ERROR token but makes the parser errors
                  * skip the next valid directive, so we just have to make sure
                  * repeated runs of this rule's handling code are idempotent.
                  */
                 (yyval.pyobj) = (yyvsp[-1].pyobj);
             }
#line 2256 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 106:
#line 688 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
                  Py_INCREF(Py_None);
                  (yyval.pyobj) = Py_None;
             }
#line 2265 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;

  case 107:
#line 695 "src/python/beancount/parser/grammar.y" /* yacc.c:1661  */
    {
         BUILDY(,
                (yyval.pyobj), "store_result", "O", (yyvsp[0].pyobj));
     }
#line 2274 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
    break;


#line 2278 "src/python/beancount/parser/grammar.c" /* yacc.c:1661  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }

  yyerror_range[1] = yylloc;

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, &yylloc);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  yyerror_range[1] = yylsp[1-yylen];
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;

      yyerror_range[1] = *yylsp;
      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp, yylsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  /* Using YYLLOC is tempting, but would change the location of
     the lookahead.  YYLOC is available though.  */
  YYLLOC_DEFAULT (yyloc, yyerror_range, 2);
  *++yylsp = yyloc;

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, &yylloc);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp, yylsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 703 "src/python/beancount/parser/grammar.y" /* yacc.c:1906  */


/* A function that will convert a token name to a string, used in debugging. */
const char* getTokenName(int token)
{
    switch ( token ) {
        case LEX_ERROR : return "LEX_ERROR";
        case INDENT    : return "INDENT";
        case EOL       : return "EOL";
        case COMMENT   : return "COMMENT";
        case SKIPPED   : return "SKIPPED";
        case PIPE      : return "PIPE";
        case ATAT      : return "ATAT";
        case AT        : return "AT";
        case LCURL     : return "LCURL";
        case RCURL     : return "RCURL";
        case EQUAL     : return "EQUAL";
        case COMMA     : return "COMMA";
        case TILDE     : return "TILDE";
        case HASH      : return "HASH";
        case PLUS      : return "PLUS";
        case MINUS     : return "MINUS";
        case ASTERISK  : return "ASTERISK";
        case SLASH     : return "SLASH";
        case LPAREN    : return "LPAREN";
        case RPAREN    : return "RPAREN";
        case FLAG      : return "FLAG";
        case TXN       : return "TXN";
        case BALANCE   : return "BALANCE";
        case OPEN      : return "OPEN";
        case CLOSE     : return "CLOSE";
        case PAD       : return "PAD";
        case EVENT     : return "EVENT";
        case PRICE     : return "PRICE";
        case NOTE      : return "NOTE";
        case DOCUMENT  : return "DOCUMENT";
        case PUSHTAG   : return "PUSHTAG";
        case POPTAG    : return "POPTAG";
        case OPTION    : return "OPTION";
        case DATE      : return "DATE";
        case ACCOUNT   : return "ACCOUNT";
        case CURRENCY  : return "CURRENCY";
        case STRING    : return "STRING";
        case NUMBER    : return "NUMBER";
        case TAG       : return "TAG";
        case LINK      : return "LINK";
        case KEY       : return "KEY";
    }
    return 0;
}

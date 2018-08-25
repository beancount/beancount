/* A Bison parser, made by GNU Bison 3.0.5.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018 Free Software Foundation, Inc.

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
#define YYBISON_VERSION "3.0.5"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 11 "beancount/parser/grammar.y" 


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
    TRACE_ERROR("Grammar Builder Exception");

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
        PyObject* rv = PyObject_CallMethod(builder, "build_grammar_error", "siOOO",
                                           yy_filename, yylineno + yy_firstline,
                                           pvalue, ptype, ptraceback);
        Py_DECREF(ptype);
        Py_DECREF(pvalue);
        Py_DECREF(ptraceback);

        if (rv == NULL) {
            /* Note: Leave the internal error trickling up its detail. */
            /* PyErr_SetString(PyExc_RuntimeError, */
            /*                 "Internal error: While building exception"); */
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


#line 166 "beancount/parser/grammar.c" 

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
#ifndef YY_YY_BEANCOUNT_PARSER_GRAMMAR_H_INCLUDED
# define YY_YY_BEANCOUNT_PARSER_GRAMMAR_H_INCLUDED
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
    COLON = 276,
    PLUS = 277,
    MINUS = 278,
    LPAREN = 279,
    RPAREN = 280,
    FLAG = 281,
    TXN = 282,
    BALANCE = 283,
    OPEN = 284,
    CLOSE = 285,
    COMMODITY = 286,
    PAD = 287,
    EVENT = 288,
    PRICE = 289,
    NOTE = 290,
    DOCUMENT = 291,
    QUERY = 292,
    CUSTOM = 293,
    PUSHTAG = 294,
    POPTAG = 295,
    PUSHMETA = 296,
    POPMETA = 297,
    OPTION = 298,
    INCLUDE = 299,
    PLUGIN = 300,
    BOOL = 301,
    DATE = 302,
    ACCOUNT = 303,
    CURRENCY = 304,
    STRING = 305,
    NUMBER = 306,
    TAG = 307,
    LINK = 308,
    KEY = 309,
    NEGATIVE = 310
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 126 "beancount/parser/grammar.y" 

    char character;
    const char* string;
    PyObject* pyobj;
    struct {
        PyObject* pyobj1;
        PyObject* pyobj2;
    } pairobj;

#line 272 "beancount/parser/grammar.c" 
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

#endif /* !YY_YY_BEANCOUNT_PARSER_GRAMMAR_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 302 "beancount/parser/grammar.c" 

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
#define YYFINAL  39
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   261

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  56
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  53
/* YYNRULES -- Number of rules.  */
#define YYNRULES  133
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  239

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   310

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
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   259,   259,   262,   266,   270,   274,   279,   280,   284,
     285,   286,   287,   293,   297,   302,   307,   312,   317,   322,
     326,   331,   336,   341,   348,   356,   361,   367,   373,   377,
     381,   385,   387,   392,   397,   402,   407,   413,   419,   424,
     425,   426,   427,   428,   429,   430,   431,   435,   441,   446,
     450,   455,   460,   466,   471,   477,   482,   487,   493,   499,
     505,   514,   520,   527,   531,   537,   543,   549,   555,   561,
     567,   575,   582,   587,   592,   597,   602,   607,   612,   619,
     625,   630,   635,   641,   646,   651,   657,   661,   665,   669,
     676,   682,   688,   694,   700,   702,   709,   714,   719,   724,
     729,   734,   744,   749,   755,   762,   763,   764,   765,   766,
     767,   768,   769,   770,   771,   772,   773,   778,   784,   790,
     795,   801,   802,   803,   804,   805,   806,   807,   808,   811,
     815,   820,   838,   845
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "LEX_ERROR", "INDENT", "EOL", "COMMENT",
  "SKIPPED", "PIPE", "ATAT", "AT", "LCURLCURL", "RCURLCURL", "LCURL",
  "RCURL", "EQUAL", "COMMA", "TILDE", "HASH", "ASTERISK", "SLASH", "COLON",
  "PLUS", "MINUS", "LPAREN", "RPAREN", "FLAG", "TXN", "BALANCE", "OPEN",
  "CLOSE", "COMMODITY", "PAD", "EVENT", "PRICE", "NOTE", "DOCUMENT",
  "QUERY", "CUSTOM", "PUSHTAG", "POPTAG", "PUSHMETA", "POPMETA", "OPTION",
  "INCLUDE", "PLUGIN", "BOOL", "DATE", "ACCOUNT", "CURRENCY", "STRING",
  "NUMBER", "TAG", "LINK", "KEY", "NEGATIVE", "$accept", "empty", "txn",
  "eol", "empty_line", "number_expr", "txn_strings", "tags_links",
  "transaction", "optflag", "price_annotation", "posting", "key_value",
  "key_value_line", "key_value_value", "posting_or_kv_list",
  "key_value_list", "currency_list", "pushtag", "poptag", "pushmeta",
  "popmeta", "open", "opt_booking", "close", "commodity", "pad", "balance",
  "amount", "amount_tolerance", "maybe_number", "maybe_currency",
  "compound_amount", "incomplete_amount", "cost_spec", "cost_comp_list",
  "cost_comp", "price", "event", "query", "note", "filename", "document",
  "custom_value", "custom_value_list", "custom", "entry", "option",
  "include", "plugin", "directive", "declarations", "file", YY_NULLPTR
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
     305,   306,   307,   308,   309,   310
};
# endif

#define YYPACT_NINF -118

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-118)))

#define YYTABLE_NINF -134

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -118,  -118,    91,    24,  -118,  -118,  -118,  -118,  -118,   -21,
     -14,    16,    19,    25,    29,    33,   223,  -118,  -118,  -118,
    -118,  -118,  -118,  -118,  -118,  -118,  -118,  -118,  -118,  -118,
    -118,  -118,  -118,  -118,  -118,  -118,  -118,  -118,  -118,  -118,
      42,    42,    34,    42,    53,    38,    42,    14,  -118,  -118,
    -118,  -118,    41,    51,    52,    45,    75,    74,    79,    81,
      93,    87,   103,  -118,  -118,   151,  -118,  -118,   142,  -118,
      42,    42,  -118,    42,  -118,   183,   110,    42,    42,   112,
     120,   183,   123,   126,   131,  -118,  -118,     7,  -118,   183,
     183,   183,  -118,  -118,  -118,  -118,  -118,  -118,  -118,  -118,
     191,  -118,  -118,  -118,  -118,  -118,   135,    42,  -118,  -118,
       6,  -118,  -118,    42,    42,   191,    42,    42,  -118,  -118,
      42,  -118,   121,  -118,  -118,  -118,    -2,  -118,  -118,    89,
     183,   183,   183,   183,  -118,   183,  -118,  -118,   113,  -118,
    -118,    42,  -118,   178,   178,  -118,  -118,  -118,  -118,    -2,
    -118,  -118,  -118,  -118,  -118,  -118,   191,  -118,  -118,  -118,
    -118,  -118,  -118,  -118,  -118,    46,    46,   198,   178,  -118,
    -118,    16,  -118,   178,   178,   178,   178,  -118,   178,   178,
    -118,   179,  -118,   178,    42,   178,    -5,  -118,  -118,  -118,
     180,  -118,  -118,  -118,   139,     1,   148,  -118,  -118,    21,
    -118,  -118,   216,   149,     5,  -118,  -118,  -118,   127,   127,
    -118,   222,  -118,  -118,  -118,    -9,    10,    18,  -118,    -4,
    -118,     9,   183,   183,  -118,  -118,   183,  -118,  -118,   127,
    -118,    42,  -118,    42,   152,  -118,  -118,  -118,  -118
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,   132,     0,     0,   131,    11,     9,    10,    12,     0,
       0,     0,     0,     0,     0,     0,     0,   121,   105,   122,
     123,   124,   125,   107,   108,   114,   109,   106,   113,   112,
     115,   111,   110,   116,   130,   126,   127,   128,   129,     1,
       0,     0,     0,     0,     0,     0,     0,     0,     6,     5,
       4,     3,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     2,     7,     0,    58,    59,     2,    60,
       0,     0,   118,     0,   119,     0,     2,     0,     0,     0,
       0,     0,     0,     0,     0,     2,    21,     2,     8,     0,
       0,     0,    44,    41,    40,    42,    39,    13,    43,    47,
      45,    37,    46,    61,   117,   120,     0,     0,    56,    55,
       2,     2,     2,     0,     0,     0,     0,     0,    94,     2,
       0,   102,     0,    23,    22,    24,     0,    19,    18,     0,
       0,     0,     0,     0,    69,     0,    70,     2,     0,    63,
      64,     0,    53,    65,    66,     2,     2,     2,     2,     0,
       2,    98,    97,   101,    96,     2,   100,    99,   103,    26,
      25,     2,    20,    16,    17,    14,    15,     0,    68,    57,
       2,     0,    54,    67,    91,    90,    93,     2,    92,   104,
      48,    27,    71,    62,     0,    95,     2,    52,    51,    38,
       0,    30,    29,    31,    24,     0,     0,    49,    50,     2,
      72,    36,    73,     2,     2,    75,    74,    79,     2,     2,
      82,     0,    89,    87,    88,    83,     2,     0,    86,     0,
      84,     0,     2,     2,    33,    77,     2,    76,    81,     2,
      80,     0,    32,     0,     0,    85,    35,    34,    78
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -118,     0,  -118,   -36,  -118,   -29,  -118,  -117,  -118,  -118,
     -11,  -118,   204,    43,  -118,  -118,   -30,  -118,  -118,  -118,
    -118,  -118,  -118,  -118,  -118,  -118,  -118,  -118,   -64,  -118,
     -87,     3,  -118,    26,  -118,    -7,     4,  -118,  -118,  -118,
    -118,  -118,  -118,  -118,  -118,  -118,  -118,  -118,  -118,  -118,
    -118,  -118,  -118
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,   142,    63,    66,    17,   202,    87,   126,    18,   196,
     231,   187,   184,   172,   101,   181,   143,   110,    19,    20,
      21,    22,    23,   141,    24,    25,    26,    27,   102,   107,
     203,   207,   218,   232,   211,   219,   220,    28,    29,    30,
      31,   119,    32,   158,   122,    33,    34,    35,    36,    37,
      38,     2,     3
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
       1,   190,   149,    64,    65,    67,   198,    69,   228,   -72,
      72,    74,   229,   191,   192,   123,   208,   116,   209,    64,
      65,   193,   138,   230,    39,   229,    64,    65,   -73,   130,
     131,    40,   132,   133,   103,   104,   226,   105,    41,   100,
     -72,   111,   112,    89,    90,    91,   106,    64,    65,    42,
     159,   160,   115,   159,   160,    68,   139,   124,   157,   205,
     127,   128,   129,    86,    73,   130,   131,   227,    99,   195,
      42,   137,    97,    44,    70,    45,   109,   145,   146,    46,
     147,   148,   144,    47,   150,   121,   155,   125,    71,    75,
     161,  -133,     4,   156,    78,     5,     6,     7,     8,    76,
      77,   163,   164,   165,   166,   170,   167,   168,   130,   131,
     140,   132,   133,   177,   162,   173,   174,   175,   176,   125,
     178,   217,   217,    79,    80,   179,    64,    65,    81,    82,
       9,    10,    11,    12,    13,    14,    15,    84,    16,   234,
     183,    83,   217,    89,    90,    91,   212,   185,   189,    89,
      90,    91,   135,    85,   130,   131,    88,   132,   133,   108,
     113,   180,   169,   201,    89,    90,    91,   151,   152,   153,
     114,   154,    97,   117,   213,   224,   118,   214,    97,   216,
     216,   120,   171,   186,   136,   197,   194,   -28,    92,    93,
      94,    95,    96,    97,    98,   236,   199,   237,   205,   200,
     216,   238,   221,   206,   210,    89,    90,    91,   215,   215,
     130,   131,   233,   132,   133,    43,   206,   130,   131,   225,
     132,   133,   200,   200,   188,   204,   200,    64,    65,   200,
       0,   222,   223,   235,    97,   130,   131,     0,   132,   133,
     134,    48,    49,     0,     0,     0,     0,   182,     0,    50,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62
};

static const yytype_int16 yycheck[] =
{
       0,     6,   119,     5,     6,    41,     5,    43,    12,    18,
      46,    47,    16,    18,    19,     8,    11,    81,    13,     5,
       6,    26,    16,    14,     0,    16,     5,     6,    18,    19,
      20,    52,    22,    23,    70,    71,    18,    73,    52,    68,
      49,    77,    78,    22,    23,    24,    75,     5,     6,    54,
      52,    53,    81,    52,    53,    21,    50,    50,   122,    49,
      89,    90,    91,    63,    50,    19,    20,    49,    68,   186,
      54,   107,    51,    54,    21,    50,    76,   113,   114,    50,
     116,   117,   112,    50,   120,    85,   122,    87,    50,    48,
     126,     0,     1,   122,    49,     4,     5,     6,     7,    48,
      48,   130,   131,   132,   133,   141,   135,   137,    19,    20,
     110,    22,    23,   149,    25,   145,   146,   147,   148,   119,
     150,   208,   209,    48,    50,   155,     5,     6,    49,    48,
      39,    40,    41,    42,    43,    44,    45,    50,    47,   226,
     170,    48,   229,    22,    23,    24,    19,   177,   184,    22,
      23,    24,    17,    50,    19,    20,     5,    22,    23,    49,
      48,   161,    49,   199,    22,    23,    24,    46,    47,    48,
      50,    50,    51,    50,    47,   211,    50,    50,    51,   208,
     209,    50,     4,     4,    49,     5,   186,    48,    46,    47,
      48,    49,    50,    51,    52,   231,    48,   233,    49,   199,
     229,    49,   209,   203,   204,    22,    23,    24,   208,   209,
      19,    20,   223,    22,    23,    11,   216,    19,    20,   216,
      22,    23,   222,   223,   181,   199,   226,     5,     6,   229,
      -1,     9,    10,   229,    51,    19,    20,    -1,    22,    23,
      49,    18,    19,    -1,    -1,    -1,    -1,    49,    -1,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    57,   107,   108,     1,     4,     5,     6,     7,    39,
      40,    41,    42,    43,    44,    45,    47,    60,    64,    74,
      75,    76,    77,    78,    80,    81,    82,    83,    93,    94,
      95,    96,    98,   101,   102,   103,   104,   105,   106,     0,
      52,    52,    54,    68,    54,    50,    50,    50,    18,    19,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    58,     5,     6,    59,    59,    21,    59,
      21,    50,    59,    50,    59,    48,    48,    48,    49,    48,
      50,    49,    48,    48,    50,    50,    57,    62,     5,    22,
      23,    24,    46,    47,    48,    49,    50,    51,    52,    57,
      61,    70,    84,    59,    59,    59,    61,    85,    49,    57,
      73,    59,    59,    48,    50,    61,    84,    50,    50,    97,
      50,    57,   100,     8,    50,    57,    63,    61,    61,    61,
      19,    20,    22,    23,    49,    17,    49,    59,    16,    50,
      57,    79,    57,    72,    72,    59,    59,    59,    59,    63,
      59,    46,    47,    48,    50,    59,    61,    84,    99,    52,
      53,    59,    25,    61,    61,    61,    61,    61,    72,    49,
      59,     4,    69,    72,    72,    72,    72,    59,    72,    72,
      57,    71,    49,    72,    68,    72,     4,    67,    69,    59,
       6,    18,    19,    26,    57,    63,    65,     5,     5,    48,
      57,    59,    61,    86,    89,    49,    57,    87,    11,    13,
      57,    90,    19,    47,    50,    57,    61,    86,    88,    91,
      92,    91,     9,    10,    59,    87,    18,    49,    12,    16,
      14,    66,    89,    66,    86,    92,    59,    59,    49
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    56,    57,    58,    58,    58,    58,    59,    59,    60,
      60,    60,    60,    61,    61,    61,    61,    61,    61,    61,
      61,    62,    62,    62,    63,    63,    63,    64,    65,    65,
      65,    65,    66,    67,    67,    67,    67,    68,    69,    70,
      70,    70,    70,    70,    70,    70,    70,    70,    71,    71,
      71,    71,    71,    72,    72,    73,    73,    73,    74,    75,
      76,    77,    78,    79,    79,    80,    81,    82,    83,    84,
      85,    85,    86,    86,    87,    87,    88,    88,    88,    89,
      90,    90,    90,    91,    91,    91,    92,    92,    92,    92,
      93,    94,    95,    96,    97,    98,    99,    99,    99,    99,
      99,    99,   100,   100,   101,   102,   102,   102,   102,   102,
     102,   102,   102,   102,   102,   102,   102,   103,   104,   105,
     105,   106,   106,   106,   106,   106,   106,   106,   106,   107,
     107,   107,   107,   108
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     1,     1,     1,     1,     1,     2,     1,
       1,     1,     1,     1,     3,     3,     3,     3,     2,     2,
       3,     1,     2,     2,     1,     2,     2,     6,     1,     1,
       1,     1,     1,     6,     8,     8,     4,     3,     3,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     4,
       4,     2,     2,     1,     2,     1,     1,     3,     3,     3,
       3,     4,     7,     1,     1,     5,     5,     6,     6,     2,
       2,     4,     1,     1,     1,     1,     2,     2,     4,     2,
       3,     3,     1,     1,     1,     3,     1,     1,     1,     1,
       6,     6,     6,     6,     1,     7,     1,     1,     1,     1,
       1,     1,     1,     2,     6,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     4,     3,     3,
       4,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       2,     2,     1,     1
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
    default: /* Avoid compiler warnings. */
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

  /* Default location. */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  yyerror_range[1] = yyloc;
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 3:
#line 263 "beancount/parser/grammar.y" 
    {
        (yyval.character) = '*';
    }
#line 1669 "beancount/parser/grammar.c" 
    break;

  case 4:
#line 267 "beancount/parser/grammar.y" 
    {
        (yyval.character) = (yyvsp[0].character);
    }
#line 1677 "beancount/parser/grammar.c" 
    break;

  case 5:
#line 271 "beancount/parser/grammar.y" 
    {
        (yyval.character) = '*';
    }
#line 1685 "beancount/parser/grammar.c" 
    break;

  case 6:
#line 275 "beancount/parser/grammar.y" 
    {
        (yyval.character) = '#';
    }
#line 1693 "beancount/parser/grammar.c" 
    break;

  case 13:
#line 294 "beancount/parser/grammar.y" 
    {
                (yyval.pyobj) = (yyvsp[0].pyobj);
            }
#line 1701 "beancount/parser/grammar.c" 
    break;

  case 14:
#line 298 "beancount/parser/grammar.y" 
    {
                (yyval.pyobj) = PyNumber_Add((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
                DECREF2((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 1710 "beancount/parser/grammar.c" 
    break;

  case 15:
#line 303 "beancount/parser/grammar.y" 
    {
                (yyval.pyobj) = PyNumber_Subtract((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
                DECREF2((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 1719 "beancount/parser/grammar.c" 
    break;

  case 16:
#line 308 "beancount/parser/grammar.y" 
    {
                (yyval.pyobj) = PyNumber_Multiply((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
                DECREF2((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 1728 "beancount/parser/grammar.c" 
    break;

  case 17:
#line 313 "beancount/parser/grammar.y" 
    {
                (yyval.pyobj) = PyNumber_TrueDivide((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
                DECREF2((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 1737 "beancount/parser/grammar.c" 
    break;

  case 18:
#line 318 "beancount/parser/grammar.y" 
    {
                (yyval.pyobj) = PyNumber_Negative((yyvsp[0].pyobj));
                DECREF1((yyvsp[0].pyobj));
            }
#line 1746 "beancount/parser/grammar.c" 
    break;

  case 19:
#line 323 "beancount/parser/grammar.y" 
    {
                (yyval.pyobj) = (yyvsp[0].pyobj);
            }
#line 1754 "beancount/parser/grammar.c" 
    break;

  case 20:
#line 327 "beancount/parser/grammar.y" 
    {
                (yyval.pyobj) = (yyvsp[-1].pyobj);
            }
#line 1762 "beancount/parser/grammar.c" 
    break;

  case 21:
#line 332 "beancount/parser/grammar.y" 
    {
                Py_INCREF(Py_None);
                (yyval.pyobj) = Py_None;
            }
#line 1771 "beancount/parser/grammar.c" 
    break;

  case 22:
#line 337 "beancount/parser/grammar.y" 
    {
                BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                       (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
            }
#line 1780 "beancount/parser/grammar.c" 
    break;

  case 23:
#line 342 "beancount/parser/grammar.y" 
    {
                BUILDY(,
                       (yyval.pyobj), "pipe_deprecated_error", "si", FILE_LINE_ARGS);
                (yyval.pyobj) = (yyvsp[-1].pyobj);
            }
#line 1790 "beancount/parser/grammar.c" 
    break;

  case 24:
#line 349 "beancount/parser/grammar.y" 
    {
               /* Note: We're passing a bogus value here in order to avoid
                * having to declare a second macro just for this one special
                * case. */
               BUILDY(,
                      (yyval.pyobj), "tag_link_new", "O", Py_None);
           }
#line 1802 "beancount/parser/grammar.c" 
    break;

  case 25:
#line 357 "beancount/parser/grammar.y" 
    {
               BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                      (yyval.pyobj), "tag_link_LINK", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
           }
#line 1811 "beancount/parser/grammar.c" 
    break;

  case 26:
#line 362 "beancount/parser/grammar.y" 
    {
               BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                      (yyval.pyobj), "tag_link_TAG", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
           }
#line 1820 "beancount/parser/grammar.c" 
    break;

  case 27:
#line 368 "beancount/parser/grammar.y" 
    {
                BUILDY(DECREF4((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                       (yyval.pyobj), "transaction", "siObOOO", FILE_LINE_ARGS, (yyvsp[-5].pyobj), (yyvsp[-4].character), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 1829 "beancount/parser/grammar.c" 
    break;

  case 28:
#line 374 "beancount/parser/grammar.y" 
    {
            (yyval.character) = '\0';
        }
#line 1837 "beancount/parser/grammar.c" 
    break;

  case 29:
#line 378 "beancount/parser/grammar.y" 
    {
            (yyval.character) = '*';
        }
#line 1845 "beancount/parser/grammar.c" 
    break;

  case 30:
#line 382 "beancount/parser/grammar.y" 
    {
            (yyval.character) = '#';
        }
#line 1853 "beancount/parser/grammar.c" 
    break;

  case 32:
#line 388 "beancount/parser/grammar.y" 
    {
                     (yyval.pyobj) = (yyvsp[0].pyobj);
                 }
#line 1861 "beancount/parser/grammar.c" 
    break;

  case 33:
#line 393 "beancount/parser/grammar.y" 
    {
            BUILDY(DECREF3((yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[-1].pyobj)),
                   (yyval.pyobj), "posting", "siOOOOOb", FILE_LINE_ARGS, (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[-1].pyobj), Py_None, Py_False, (yyvsp[-4].character));
        }
#line 1870 "beancount/parser/grammar.c" 
    break;

  case 34:
#line 398 "beancount/parser/grammar.y" 
    {
            BUILDY(DECREF4((yyvsp[-5].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj)),
                   (yyval.pyobj), "posting", "siOOOOOb", FILE_LINE_ARGS, (yyvsp[-5].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj), Py_False, (yyvsp[-6].character));
        }
#line 1879 "beancount/parser/grammar.c" 
    break;

  case 35:
#line 403 "beancount/parser/grammar.y" 
    {
            BUILDY(DECREF4((yyvsp[-5].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj)),
                   (yyval.pyobj), "posting", "siOOOOOb", FILE_LINE_ARGS, (yyvsp[-5].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj), Py_True, (yyvsp[-6].character));
        }
#line 1888 "beancount/parser/grammar.c" 
    break;

  case 36:
#line 408 "beancount/parser/grammar.y" 
    {
            BUILDY(DECREF1((yyvsp[-1].pyobj)),
                   (yyval.pyobj), "posting", "siOOOOOb", FILE_LINE_ARGS, (yyvsp[-1].pyobj), missing_obj, Py_None, Py_None, Py_False, (yyvsp[-2].character));
        }
#line 1897 "beancount/parser/grammar.c" 
    break;

  case 37:
#line 414 "beancount/parser/grammar.y" 
    {
              BUILDY(DECREF2((yyvsp[-1].string), (yyvsp[0].pyobj)),
                     (yyval.pyobj), "key_value", "OO", (yyvsp[-1].string), (yyvsp[0].pyobj));
          }
#line 1906 "beancount/parser/grammar.c" 
    break;

  case 38:
#line 420 "beancount/parser/grammar.y" 
    {
                   (yyval.pyobj) = (yyvsp[-1].pyobj);
               }
#line 1914 "beancount/parser/grammar.c" 
    break;

  case 46:
#line 432 "beancount/parser/grammar.y" 
    {
                    (yyval.pyobj) = (yyvsp[0].pyobj);
                }
#line 1922 "beancount/parser/grammar.c" 
    break;

  case 47:
#line 436 "beancount/parser/grammar.y" 
    {
                    Py_INCREF(Py_None);
                    (yyval.pyobj) = Py_None;
                }
#line 1931 "beancount/parser/grammar.c" 
    break;

  case 48:
#line 442 "beancount/parser/grammar.y" 
    {
                       Py_INCREF(Py_None);
                       (yyval.pyobj) = Py_None;
                   }
#line 1940 "beancount/parser/grammar.c" 
    break;

  case 49:
#line 447 "beancount/parser/grammar.y" 
    {
                       (yyval.pyobj) = (yyvsp[-3].pyobj);
                   }
#line 1948 "beancount/parser/grammar.c" 
    break;

  case 50:
#line 451 "beancount/parser/grammar.y" 
    {
                       BUILDY(DECREF2((yyvsp[-3].pyobj), (yyvsp[-1].pyobj)),
                              (yyval.pyobj), "handle_list", "OO", (yyvsp[-3].pyobj), (yyvsp[-1].pyobj));
                   }
#line 1957 "beancount/parser/grammar.c" 
    break;

  case 51:
#line 456 "beancount/parser/grammar.y" 
    {
                       BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                              (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                   }
#line 1966 "beancount/parser/grammar.c" 
    break;

  case 52:
#line 461 "beancount/parser/grammar.y" 
    {
                       BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                              (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                   }
#line 1975 "beancount/parser/grammar.c" 
    break;

  case 53:
#line 467 "beancount/parser/grammar.y" 
    {
                   Py_INCREF(Py_None);
                   (yyval.pyobj) = Py_None;
               }
#line 1984 "beancount/parser/grammar.c" 
    break;

  case 54:
#line 472 "beancount/parser/grammar.y" 
    {
                   BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                          (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
               }
#line 1993 "beancount/parser/grammar.c" 
    break;

  case 55:
#line 478 "beancount/parser/grammar.y" 
    {
                  Py_INCREF(Py_None);
                  (yyval.pyobj) = Py_None;
              }
#line 2002 "beancount/parser/grammar.c" 
    break;

  case 56:
#line 483 "beancount/parser/grammar.y" 
    {
                  BUILDY(DECREF1((yyvsp[0].pyobj)),
                         (yyval.pyobj), "handle_list", "OO", Py_None, (yyvsp[0].pyobj));
              }
#line 2011 "beancount/parser/grammar.c" 
    break;

  case 57:
#line 488 "beancount/parser/grammar.y" 
    {
                  BUILDY(DECREF2((yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                         (yyval.pyobj), "handle_list", "OO", (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
              }
#line 2020 "beancount/parser/grammar.c" 
    break;

  case 58:
#line 494 "beancount/parser/grammar.y" 
    {
             BUILDY(DECREF1((yyvsp[-1].pyobj)),
                    (yyval.pyobj), "pushtag", "O", (yyvsp[-1].pyobj));
         }
#line 2029 "beancount/parser/grammar.c" 
    break;

  case 59:
#line 500 "beancount/parser/grammar.y" 
    {
           BUILDY(DECREF1((yyvsp[-1].pyobj)),
                  (yyval.pyobj), "poptag", "O", (yyvsp[-1].pyobj));
       }
#line 2038 "beancount/parser/grammar.c" 
    break;

  case 60:
#line 506 "beancount/parser/grammar.y" 
    {
             /* Note: key_value is a tuple, Py_BuildValue() won't wrap it up
              * within a tuple, so expand in the method (it receives two
              * objects). See https://docs.python.org/3.4/c-api/arg.html. */
             BUILDY(DECREF1((yyvsp[-1].pyobj)),
                    (yyval.pyobj), "pushmeta", "O", (yyvsp[-1].pyobj));
         }
#line 2050 "beancount/parser/grammar.c" 
    break;

  case 61:
#line 515 "beancount/parser/grammar.y" 
    {
            BUILDY(DECREF1((yyvsp[-2].pyobj)),
                   (yyval.pyobj), "popmeta", "O", (yyvsp[-2].pyobj));
        }
#line 2059 "beancount/parser/grammar.c" 
    break;

  case 62:
#line 521 "beancount/parser/grammar.y" 
    {
         BUILDY(DECREF5((yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                (yyval.pyobj), "open", "siOOOOO", FILE_LINE_ARGS, (yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
         ;
     }
#line 2069 "beancount/parser/grammar.c" 
    break;

  case 63:
#line 528 "beancount/parser/grammar.y" 
    {
                (yyval.pyobj) = (yyvsp[0].pyobj);
            }
#line 2077 "beancount/parser/grammar.c" 
    break;

  case 64:
#line 532 "beancount/parser/grammar.y" 
    {
                Py_INCREF(Py_None);
                (yyval.pyobj) = Py_None;
            }
#line 2086 "beancount/parser/grammar.c" 
    break;

  case 65:
#line 538 "beancount/parser/grammar.y" 
    {
          BUILDY(DECREF3((yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                 (yyval.pyobj), "close", "siOOO", FILE_LINE_ARGS, (yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      }
#line 2095 "beancount/parser/grammar.c" 
    break;

  case 66:
#line 544 "beancount/parser/grammar.y" 
    {
              BUILDY(DECREF3((yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                     (yyval.pyobj), "commodity", "siOOO", FILE_LINE_ARGS, (yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
          }
#line 2104 "beancount/parser/grammar.c" 
    break;

  case 67:
#line 550 "beancount/parser/grammar.y" 
    {
        BUILDY(DECREF4((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "pad", "siOOOO", FILE_LINE_ARGS, (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
    }
#line 2113 "beancount/parser/grammar.c" 
    break;

  case 68:
#line 556 "beancount/parser/grammar.y" 
    {
            BUILDY(DECREF5((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[0].pyobj), (yyvsp[-2].pairobj).pyobj1, (yyvsp[-2].pairobj).pyobj2),
                   (yyval.pyobj), "balance", "siOOOOO", FILE_LINE_ARGS, (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pairobj).pyobj1, (yyvsp[-2].pairobj).pyobj2, (yyvsp[0].pyobj));
        }
#line 2122 "beancount/parser/grammar.c" 
    break;

  case 69:
#line 562 "beancount/parser/grammar.y" 
    {
           BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                  (yyval.pyobj), "amount", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
       }
#line 2131 "beancount/parser/grammar.c" 
    break;

  case 70:
#line 568 "beancount/parser/grammar.y" 
    {
                     BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                            (yyval.pairobj).pyobj1, "amount", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                     (yyval.pairobj).pyobj2 = Py_None;
                     Py_INCREF(Py_None);
                     ;
                 }
#line 2143 "beancount/parser/grammar.c" 
    break;

  case 71:
#line 576 "beancount/parser/grammar.y" 
    {
                     BUILDY(DECREF2((yyvsp[-3].pyobj), (yyvsp[0].pyobj)),
                            (yyval.pairobj).pyobj1, "amount", "OO", (yyvsp[-3].pyobj), (yyvsp[0].pyobj));
                     (yyval.pairobj).pyobj2 = (yyvsp[-1].pyobj);
                 }
#line 2153 "beancount/parser/grammar.c" 
    break;

  case 72:
#line 583 "beancount/parser/grammar.y" 
    {
                 Py_INCREF(missing_obj);
                 (yyval.pyobj) = missing_obj;
             }
#line 2162 "beancount/parser/grammar.c" 
    break;

  case 73:
#line 588 "beancount/parser/grammar.y" 
    {
                 (yyval.pyobj) = (yyvsp[0].pyobj);
             }
#line 2170 "beancount/parser/grammar.c" 
    break;

  case 74:
#line 593 "beancount/parser/grammar.y" 
    {
                 Py_INCREF(missing_obj);
                 (yyval.pyobj) = missing_obj;
             }
#line 2179 "beancount/parser/grammar.c" 
    break;

  case 75:
#line 598 "beancount/parser/grammar.y" 
    {
                 (yyval.pyobj) = (yyvsp[0].pyobj);
             }
#line 2187 "beancount/parser/grammar.c" 
    break;

  case 76:
#line 603 "beancount/parser/grammar.y" 
    {
                    BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                           (yyval.pyobj), "compound_amount", "OOO", (yyvsp[-1].pyobj), Py_None, (yyvsp[0].pyobj));
                }
#line 2196 "beancount/parser/grammar.c" 
    break;

  case 77:
#line 608 "beancount/parser/grammar.y" 
    {
                    BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                           (yyval.pyobj), "compound_amount", "OOO", (yyvsp[-1].pyobj), Py_None, (yyvsp[0].pyobj));
                }
#line 2205 "beancount/parser/grammar.c" 
    break;

  case 78:
#line 613 "beancount/parser/grammar.y" 
    {
                    BUILDY(DECREF3((yyvsp[-3].pyobj), (yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                           (yyval.pyobj), "compound_amount", "OOO", (yyvsp[-3].pyobj), (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                    ;
                }
#line 2215 "beancount/parser/grammar.c" 
    break;

  case 79:
#line 620 "beancount/parser/grammar.y" 
    {
                      BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                             (yyval.pyobj), "amount", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                 }
#line 2224 "beancount/parser/grammar.c" 
    break;

  case 80:
#line 626 "beancount/parser/grammar.y" 
    {
              BUILDY(DECREF1((yyvsp[-1].pyobj)),
                     (yyval.pyobj), "cost_spec", "OO", (yyvsp[-1].pyobj), Py_False);
          }
#line 2233 "beancount/parser/grammar.c" 
    break;

  case 81:
#line 631 "beancount/parser/grammar.y" 
    {
              BUILDY(DECREF1((yyvsp[-1].pyobj)),
                     (yyval.pyobj), "cost_spec", "OO", (yyvsp[-1].pyobj), Py_True);
          }
#line 2242 "beancount/parser/grammar.c" 
    break;

  case 82:
#line 636 "beancount/parser/grammar.y" 
    {
              Py_INCREF(Py_None);
              (yyval.pyobj) = Py_None;
          }
#line 2251 "beancount/parser/grammar.c" 
    break;

  case 83:
#line 642 "beancount/parser/grammar.y" 
    {
                   /* We indicate that there was a cost if there */
                   (yyval.pyobj) = PyList_New(0);
               }
#line 2260 "beancount/parser/grammar.c" 
    break;

  case 84:
#line 647 "beancount/parser/grammar.y" 
    {
                   BUILDY(DECREF1((yyvsp[0].pyobj)),
                          (yyval.pyobj), "handle_list", "OO", Py_None, (yyvsp[0].pyobj));
               }
#line 2269 "beancount/parser/grammar.c" 
    break;

  case 85:
#line 652 "beancount/parser/grammar.y" 
    {
                   BUILDY(DECREF2((yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                          (yyval.pyobj), "handle_list", "OO", (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
               }
#line 2278 "beancount/parser/grammar.c" 
    break;

  case 86:
#line 658 "beancount/parser/grammar.y" 
    {
              (yyval.pyobj) = (yyvsp[0].pyobj);
          }
#line 2286 "beancount/parser/grammar.c" 
    break;

  case 87:
#line 662 "beancount/parser/grammar.y" 
    {
              (yyval.pyobj) = (yyvsp[0].pyobj);
          }
#line 2294 "beancount/parser/grammar.c" 
    break;

  case 88:
#line 666 "beancount/parser/grammar.y" 
    {
              (yyval.pyobj) = (yyvsp[0].pyobj);
          }
#line 2302 "beancount/parser/grammar.c" 
    break;

  case 89:
#line 670 "beancount/parser/grammar.y" 
    {
              BUILDY(,
                     (yyval.pyobj), "cost_merge", "O", Py_None);
          }
#line 2311 "beancount/parser/grammar.c" 
    break;

  case 90:
#line 677 "beancount/parser/grammar.y" 
    {
          BUILDY(DECREF4((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                 (yyval.pyobj), "price", "siOOOO", FILE_LINE_ARGS, (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      }
#line 2320 "beancount/parser/grammar.c" 
    break;

  case 91:
#line 683 "beancount/parser/grammar.y" 
    {
          BUILDY(DECREF4((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                 (yyval.pyobj), "event", "siOOOO", FILE_LINE_ARGS, (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      }
#line 2329 "beancount/parser/grammar.c" 
    break;

  case 92:
#line 689 "beancount/parser/grammar.y" 
    {
             BUILDY(DECREF4((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                    (yyval.pyobj), "query", "siOOOO", FILE_LINE_ARGS, (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
         }
#line 2338 "beancount/parser/grammar.c" 
    break;

  case 93:
#line 695 "beancount/parser/grammar.y" 
    {
          BUILDY(DECREF4((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                 (yyval.pyobj), "note", "siOOOO", FILE_LINE_ARGS, (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      }
#line 2347 "beancount/parser/grammar.c" 
    break;

  case 95:
#line 703 "beancount/parser/grammar.y" 
    {
             BUILDY(DECREF5((yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                    (yyval.pyobj), "document", "siOOOOO", FILE_LINE_ARGS, (yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
         }
#line 2356 "beancount/parser/grammar.c" 
    break;

  case 96:
#line 710 "beancount/parser/grammar.y" 
    {
                 BUILDY(DECREF1((yyvsp[0].pyobj)),
                        (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
             }
#line 2365 "beancount/parser/grammar.c" 
    break;

  case 97:
#line 715 "beancount/parser/grammar.y" 
    {
                 BUILDY(DECREF1((yyvsp[0].pyobj)),
                        (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
             }
#line 2374 "beancount/parser/grammar.c" 
    break;

  case 98:
#line 720 "beancount/parser/grammar.y" 
    {
                 BUILDY(DECREF1((yyvsp[0].pyobj)),
                        (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
             }
#line 2383 "beancount/parser/grammar.c" 
    break;

  case 99:
#line 725 "beancount/parser/grammar.y" 
    {
                 BUILDY(DECREF1((yyvsp[0].pyobj)),
                        (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
             }
#line 2392 "beancount/parser/grammar.c" 
    break;

  case 100:
#line 730 "beancount/parser/grammar.y" 
    {
                 BUILDY(DECREF1((yyvsp[0].pyobj)),
                        (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
             }
#line 2401 "beancount/parser/grammar.c" 
    break;

  case 101:
#line 735 "beancount/parser/grammar.y" 
    {
                 /* Obtain beancount.core.account.TYPE */
                 PyObject* module = PyImport_ImportModule("beancount.core.account");
                 PyObject* dtype = PyObject_GetAttrString(module, "TYPE");
                 Py_DECREF(module);
                 BUILDY(DECREF2((yyvsp[0].pyobj), dtype),
                        (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), dtype);
             }
#line 2414 "beancount/parser/grammar.c" 
    break;

  case 102:
#line 745 "beancount/parser/grammar.y" 
    {
                      Py_INCREF(Py_None);
                      (yyval.pyobj) = Py_None;
                  }
#line 2423 "beancount/parser/grammar.c" 
    break;

  case 103:
#line 750 "beancount/parser/grammar.y" 
    {
                      BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                             (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                  }
#line 2432 "beancount/parser/grammar.c" 
    break;

  case 104:
#line 756 "beancount/parser/grammar.y" 
    {
           BUILDY(DECREF4((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                  (yyval.pyobj), "custom", "siOOOO", FILE_LINE_ARGS, (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
       }
#line 2441 "beancount/parser/grammar.c" 
    break;

  case 116:
#line 774 "beancount/parser/grammar.y" 
    {
          (yyval.pyobj) = (yyvsp[0].pyobj);
      }
#line 2449 "beancount/parser/grammar.c" 
    break;

  case 117:
#line 779 "beancount/parser/grammar.y" 
    {
           BUILDY(DECREF2((yyvsp[-2].pyobj), (yyvsp[-1].pyobj)),
                  (yyval.pyobj), "option", "siOO", FILE_LINE_ARGS, (yyvsp[-2].pyobj), (yyvsp[-1].pyobj));
       }
#line 2458 "beancount/parser/grammar.c" 
    break;

  case 118:
#line 785 "beancount/parser/grammar.y" 
    {
           BUILDY(DECREF1((yyvsp[-1].pyobj)),
                  (yyval.pyobj), "include", "siO", FILE_LINE_ARGS, (yyvsp[-1].pyobj));
       }
#line 2467 "beancount/parser/grammar.c" 
    break;

  case 119:
#line 791 "beancount/parser/grammar.y" 
    {
           BUILDY(DECREF1((yyvsp[-1].pyobj)),
                  (yyval.pyobj), "plugin", "siOO", FILE_LINE_ARGS, (yyvsp[-1].pyobj), Py_None);
       }
#line 2476 "beancount/parser/grammar.c" 
    break;

  case 120:
#line 796 "beancount/parser/grammar.y" 
    {
           BUILDY(DECREF2((yyvsp[-2].pyobj), (yyvsp[-1].pyobj)),
                  (yyval.pyobj), "plugin", "siOO", FILE_LINE_ARGS, (yyvsp[-2].pyobj), (yyvsp[-1].pyobj));
       }
#line 2485 "beancount/parser/grammar.c" 
    break;

  case 129:
#line 812 "beancount/parser/grammar.y" 
    {
                 (yyval.pyobj) = (yyvsp[-1].pyobj);
             }
#line 2493 "beancount/parser/grammar.c" 
    break;

  case 130:
#line 816 "beancount/parser/grammar.y" 
    {
                 BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                        (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
             }
#line 2502 "beancount/parser/grammar.c" 
    break;

  case 131:
#line 821 "beancount/parser/grammar.y" 
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
#line 2524 "beancount/parser/grammar.c" 
    break;

  case 132:
#line 839 "beancount/parser/grammar.y" 
    {
                  Py_INCREF(Py_None);
                  (yyval.pyobj) = Py_None;
             }
#line 2533 "beancount/parser/grammar.c" 
    break;

  case 133:
#line 846 "beancount/parser/grammar.y" 
    {
         BUILDY(DECREF1((yyvsp[0].pyobj)),
                (yyval.pyobj), "store_result", "O", (yyvsp[0].pyobj));
     }
#line 2542 "beancount/parser/grammar.c" 
    break;


#line 2546 "beancount/parser/grammar.c" 
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
#line 854 "beancount/parser/grammar.y" 


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
        case COLON     : return "COLON";
        case LPAREN    : return "LPAREN";
        case RPAREN    : return "RPAREN";
        case FLAG      : return "FLAG";
        case TXN       : return "TXN";
        case BALANCE   : return "BALANCE";
        case OPEN      : return "OPEN";
        case CLOSE     : return "CLOSE";
        case PAD       : return "PAD";
        case EVENT     : return "EVENT";
        case QUERY     : return "QUERY";
        case CUSTOM    : return "CUSTOM";
        case PRICE     : return "PRICE";
        case NOTE      : return "NOTE";
        case DOCUMENT  : return "DOCUMENT";
        case PUSHTAG   : return "PUSHTAG";
        case POPTAG    : return "POPTAG";
        case PUSHMETA  : return "PUSHMETA";
        case POPMETA   : return "POPMETA";
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
    return "INVALID_TOKEN";
}

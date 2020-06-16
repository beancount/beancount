/* A Bison parser, made by GNU Bison 3.4.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2019 Free Software Foundation,
   Inc.

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

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.4.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 2

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 52 "beancount/parser/grammar.y"


#include "macros.h"
#include "grammar.h"
#include "lexer.h"

extern YY_DECL;

/*
 * Call a builder method and detect and handle a Python exception being raised
 * in the handler. Always run the code to clean the references provided by the
 * reduced rule. {05bb0fb60e86}
 */
#define CALL(CLEAN, target, method_name, format, ...)                   \
    target = PyObject_CallMethod(builder, method_name, "Oi" format,     \
                                 (yyloc).file_name, (yyloc).first_line, \
                                 ## __VA_ARGS__);                       \
    CLEAN;                                                              \
    if (target == NULL) {                                               \
        build_grammar_error_from_exception(builder, yyloc);             \
        YYERROR;                                                        \
    }

/* Build a grammar error from the exception context. */
void build_grammar_error_from_exception(PyObject* builder, YYLTYPE yyloc)
{
    PyObject* traceback = NULL;
    PyObject* value = NULL;
    PyObject* type = NULL;
    PyObject* rv = NULL;

    /* Get the exception context. */
    PyErr_Fetch(&type, &value, &traceback);
    PyErr_NormalizeException(&type, &value, &traceback);

    if (value) {
        /* traceback can be NULL. This is the case for exceptions
         * raised in C code for example. Replace NULL with Py_None to
         * keep PyObject_CallMethod() happy. */
        traceback = traceback ? traceback : Py_None;

        /* Build and accumulate a new error object. {27d1d459c5cd} */
        rv = PyObject_CallMethod(builder, "build_grammar_error", "OiOOO",
                                 yyloc.file_name, yyloc.first_line,
                                 value, type, traceback);
    } else {
        PyErr_SetString(PyExc_RuntimeError, "No exception");
    }

    Py_XDECREF(rv);
    Py_XDECREF(type);
    Py_XDECREF(value);
    Py_XDECREF(traceback);
}

/* Error-handling function. {ca6aab8b9748} */
void yyerror(YYLTYPE *locp, yyscan_t scanner, PyObject* builder, char const* message)
{
    PyObject* rv = NULL;

    /* Skip lex errors: they have already been registered the lexer itself. */
    if (strstr(message, "LEX_ERROR"))
        return;

    /* Register a syntax error with the builder. */
    rv = PyObject_CallMethod(builder, "build_grammar_error", "Ois",
                             locp[0].file_name, locp[0].first_line,
                             message);

    Py_XDECREF(rv);
}

#define DECREF(...) _CC_FUNC(Py_DECREF, __VA_ARGS__)


#line 146 "beancount/parser/grammar.c"

# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* Use api.header.include to #include this header
   instead of duplicating it here.  */
#ifndef YY_YY_BEANCOUNT_PARSER_GRAMMAR_H_INCLUDED
# define YY_YY_BEANCOUNT_PARSER_GRAMMAR_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int yydebug;
#endif
/* "%code requires" blocks.  */
#line 12 "beancount/parser/grammar.y"


#include <stdio.h>
#include <assert.h>
#include "parser.h"

/* Extend default location type with file name information. */
typedef struct YYLTYPE {
    int first_line;
    int first_column;
    int last_line;
    int last_column;
    PyObject* file_name;
} YYLTYPE;

#define YYLTYPE_IS_DECLARED 1

/* Extend defult location action to copy file name over. */
#define YYLLOC_DEFAULT(current, rhs, N)                                 \
    do {                                                                \
        if (N) {                                                        \
            (current).first_line   = YYRHSLOC(rhs, 1).first_line;       \
            (current).first_column = YYRHSLOC(rhs, 1).first_column;     \
            (current).last_line    = YYRHSLOC(rhs, N).last_line;        \
            (current).last_column  = YYRHSLOC(rhs, N).last_column;      \
            (current).file_name    = YYRHSLOC(rhs, N).file_name;        \
        } else {                                                        \
            (current).first_line   = (current).last_line =              \
                YYRHSLOC(rhs, 0).last_line;                             \
            (current).first_column = (current).last_column =            \
                YYRHSLOC(rhs, 0).last_column;                           \
            (current).file_name    = YYRHSLOC(rhs, 0).file_name;        \
        }                                                               \
    } while (0)

/* Get a printable version of a token name. */
const char* token_to_string(int token);


#line 220 "beancount/parser/grammar.c"

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
    NONE = 301,
    BOOL = 302,
    DATE = 303,
    ACCOUNT = 304,
    CURRENCY = 305,
    STRING = 306,
    NUMBER = 307,
    TAG = 308,
    LINK = 309,
    KEY = 310,
    NEGATIVE = 311
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 142 "beancount/parser/grammar.y"

    char character;
    const char* string;
    PyObject* pyobj;
    struct {
        PyObject* pyobj1;
        PyObject* pyobj2;
    } pairobj;

#line 298 "beancount/parser/grammar.c"

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



int yyparse (yyscan_t scanner, PyObject* builder);

#endif /* !YY_YY_BEANCOUNT_PARSER_GRAMMAR_H_INCLUDED  */



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
typedef unsigned short yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short yytype_int16;
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
#  define YYSIZE_T unsigned
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

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
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


#define YY_ASSERT(E) ((void) (0 && (E)))

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
#define YYLAST   303

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  57
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  55
/* YYNRULES -- Number of rules.  */
#define YYNRULES  136
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  242

#define YYUNDEFTOK  2
#define YYMAXUTOK   311

/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                                \
  ((unsigned) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
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
      55,    56
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   278,   278,   281,   285,   289,   293,   298,   299,   303,
     304,   305,   306,   312,   316,   321,   326,   331,   336,   341,
     345,   350,   355,   360,   367,   375,   380,   386,   392,   396,
     400,   404,   406,   411,   417,   423,   428,   433,   438,   444,
     450,   455,   456,   457,   458,   459,   460,   461,   462,   463,
     467,   473,   478,   482,   487,   492,   498,   503,   509,   514,
     519,   525,   531,   537,   546,   552,   559,   563,   569,   575,
     581,   587,   593,   599,   607,   614,   619,   624,   629,   634,
     639,   644,   651,   657,   662,   667,   673,   678,   683,   689,
     693,   697,   701,   708,   714,   720,   726,   732,   734,   741,
     746,   751,   756,   761,   766,   776,   781,   787,   794,   795,
     796,   797,   798,   799,   800,   801,   802,   803,   804,   805,
     810,   816,   822,   827,   833,   834,   835,   836,   837,   838,
     839,   840,   843,   847,   852,   870,   877
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
  "INCLUDE", "PLUGIN", "NONE", "BOOL", "DATE", "ACCOUNT", "CURRENCY",
  "STRING", "NUMBER", "TAG", "LINK", "KEY", "NEGATIVE", "$accept", "empty",
  "txn", "eol", "empty_line", "number_expr", "txn_strings", "tags_links",
  "transaction", "optflag", "price_annotation", "account", "currency",
  "posting", "key_value", "key_value_line", "key_value_value",
  "posting_or_kv_list", "key_value_list", "currency_list", "pushtag",
  "poptag", "pushmeta", "popmeta", "open", "opt_booking", "close",
  "commodity", "pad", "balance", "amount", "amount_tolerance",
  "maybe_number", "maybe_currency", "compound_amount", "incomplete_amount",
  "cost_spec", "cost_comp_list", "cost_comp", "price", "event", "query",
  "note", "filename", "document", "custom_value", "custom_value_list",
  "custom", "entry", "option", "include", "plugin", "directive",
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
     305,   306,   307,   308,   309,   310,   311
};
# endif

#define YYPACT_NINF -121

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-121)))

#define YYTABLE_NINF -137

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -121,  -121,   216,    22,  -121,  -121,  -121,  -121,  -121,   -24,
      -7,    26,    35,    15,    47,    54,   265,  -121,  -121,  -121,
    -121,  -121,  -121,  -121,  -121,  -121,  -121,  -121,  -121,  -121,
    -121,  -121,  -121,  -121,  -121,  -121,  -121,  -121,  -121,  -121,
      55,    55,   100,    55,   105,    65,    55,     6,  -121,  -121,
    -121,  -121,    81,    81,    81,    79,    81,    82,    79,    81,
      81,    88,    94,  -121,  -121,   154,  -121,  -121,   229,  -121,
      55,    55,  -121,    55,  -121,  -121,   101,    79,    55,  -121,
      55,    81,   111,   101,   115,   116,   117,  -121,  -121,     7,
    -121,   101,   101,   101,  -121,  -121,  -121,  -121,  -121,  -121,
    -121,   129,  -121,  -121,  -121,  -121,  -121,  -121,  -121,    25,
      55,  -121,  -121,    23,  -121,  -121,    55,    55,   129,    55,
      55,  -121,  -121,    55,  -121,   222,  -121,  -121,  -121,    11,
    -121,  -121,   138,   101,   101,   101,   101,  -121,   101,  -121,
    -121,    79,  -121,  -121,    55,  -121,   169,   169,  -121,  -121,
    -121,  -121,    11,  -121,  -121,  -121,  -121,  -121,   129,  -121,
    -121,  -121,  -121,  -121,  -121,  -121,  -121,  -121,    52,    52,
     129,   169,  -121,  -121,    26,  -121,   169,   169,   169,   169,
    -121,   169,   169,  -121,   172,  -121,   169,    55,   169,    -5,
    -121,  -121,  -121,   173,  -121,  -121,  -121,   133,    -2,    81,
    -121,  -121,    32,  -121,  -121,   161,    79,    95,  -121,  -121,
    -121,    80,    80,  -121,    14,  -121,  -121,  -121,    12,   124,
      17,  -121,    84,  -121,   103,   101,   101,  -121,  -121,   101,
    -121,  -121,    80,  -121,    55,  -121,    55,    79,  -121,  -121,
    -121,  -121
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,   135,     0,     0,   134,    11,     9,    10,    12,     0,
       0,     0,     0,     0,     0,     0,     0,   124,   108,   125,
     126,   127,   128,   110,   111,   117,   112,   109,   116,   115,
     118,   114,   113,   119,   133,   129,   130,   131,   132,     1,
       0,     0,     0,     0,     0,     0,     0,     0,     6,     5,
       4,     3,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     2,     7,     0,    61,    62,     2,    63,
       0,     0,   121,     0,   122,    33,     0,     2,     0,    34,
       0,     0,     0,     0,     0,     0,     0,     2,    21,     2,
       8,     0,     0,     0,    47,    46,    43,    41,    13,    45,
      50,    48,    42,    44,    39,    49,    64,   120,   123,     0,
       0,    58,    59,     2,     2,     2,     0,     0,     0,     0,
       0,    97,     2,     0,   105,     0,    23,    22,    24,     0,
      19,    18,     0,     0,     0,     0,     0,    72,     0,    73,
       2,     0,    66,    67,     0,    56,    68,    69,     2,     2,
       2,     2,     0,     2,   101,   100,    99,     2,   103,   104,
     102,   106,    26,    25,     2,    20,    16,    17,    14,    15,
       0,    71,    60,     2,     0,    57,    70,    94,    93,    96,
       2,    95,   107,    51,    27,    74,    65,     0,    98,     2,
      55,    54,    40,     0,    30,    29,    31,    24,     0,     0,
      52,    53,     2,    75,    38,    76,     2,     2,    77,    78,
      82,     2,     2,    85,     0,    92,    90,    91,    86,     2,
       0,    89,     0,    87,     0,     2,     2,    35,    80,     2,
      79,    84,     2,    83,     0,    32,     0,     0,    88,    37,
      36,    81
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -121,     0,  -121,   -37,  -121,     2,  -121,  -120,  -121,  -121,
     -41,   -28,   -50,  -121,   175,     4,  -121,  -121,   -39,  -121,
    -121,  -121,  -121,  -121,  -121,  -121,  -121,  -121,  -121,  -121,
     -76,  -121,   -57,   -29,  -121,   -11,  -121,   -20,   -38,  -121,
    -121,  -121,  -121,  -121,  -121,  -121,  -121,  -121,  -121,  -121,
    -121,  -121,  -121,  -121,  -121
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,   145,    63,    66,    17,   205,    89,   129,    18,   199,
     234,    76,   137,   190,   187,   175,   104,   184,   146,   113,
      19,    20,    21,    22,    23,   144,    24,    25,    26,    27,
     105,   110,   206,   210,   221,   235,   214,   222,   223,    28,
      29,    30,    31,   122,    32,   161,   125,    33,    34,    35,
      36,    37,    38,     2,     3
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
       1,   193,   152,   201,    67,    80,    69,   119,    83,    72,
      74,    64,    65,   194,   195,   126,    64,    65,   103,    64,
      65,   196,    39,   225,   226,    77,    78,   112,    81,    40,
     -75,    84,    85,   106,   107,   229,   108,    64,    65,   141,
     102,   114,   138,   115,   133,   134,    41,   135,   136,   160,
      42,   162,   163,   116,    91,    92,    93,    73,   127,   139,
      64,    65,   -75,    88,   162,   163,    45,    79,   100,   198,
     101,   133,   134,   140,   142,    79,   147,   111,   109,   148,
     149,    42,   150,   151,    98,   118,   153,   124,   157,   128,
      44,   172,   164,   130,   131,   132,   231,   159,    46,   215,
     232,   171,    91,    92,    93,    47,   211,   173,   212,   176,
     177,   178,   179,   143,   181,   180,    71,   233,   182,   232,
     185,    68,   128,    91,    92,    93,    70,   158,   216,    79,
      75,   217,    98,    82,   186,   166,   167,   168,   169,    86,
     170,   188,   -76,   133,   134,    87,   135,   136,   133,   134,
     192,   135,   136,    98,   220,   220,   209,   133,   134,    90,
     135,   136,   117,   165,   183,   204,   120,   121,   123,   209,
     230,   202,   237,   174,    79,   220,   189,   227,   200,    79,
     133,   134,   -28,   135,   136,   236,    43,   241,   191,   197,
     228,   207,   224,     0,   238,     0,     0,   239,     0,   240,
       0,     0,   203,     0,     0,     0,   208,   213,     0,     0,
       0,   218,   218,   219,   219,     0,  -136,     4,     0,   208,
       5,     6,     7,     8,     0,   203,   203,    64,    65,   203,
       0,     0,   203,     0,   219,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    91,    92,    93,     0,     0,     0,
       0,    91,    92,    93,     0,     9,    10,    11,    12,    13,
      14,    15,     0,     0,    16,     0,     0,     0,     0,   154,
     155,    75,     0,   156,    98,    94,    95,    96,    75,    79,
      97,    98,    99,    48,    49,     0,     0,     0,     0,     0,
       0,    50,    51,    52,    53,    54,    55,    56,    57,    58,
      59,    60,    61,    62
};

static const yytype_int16 yycheck[] =
{
       0,     6,   122,     5,    41,    55,    43,    83,    58,    46,
      47,     5,     6,    18,    19,     8,     5,     6,    68,     5,
       6,    26,     0,     9,    10,    53,    54,    77,    56,    53,
      18,    59,    60,    70,    71,    18,    73,     5,     6,    16,
      68,    78,    17,    80,    19,    20,    53,    22,    23,   125,
      55,    53,    54,    81,    22,    23,    24,    51,    51,   109,
       5,     6,    50,    63,    53,    54,    51,    50,    68,   189,
      68,    19,    20,   110,    51,    50,   115,    77,    76,   116,
     117,    55,   119,   120,    52,    83,   123,    87,   125,    89,
      55,   141,   129,    91,    92,    93,    12,   125,    51,    19,
      16,   140,    22,    23,    24,    51,    11,   144,    13,   148,
     149,   150,   151,   113,   153,   152,    51,    14,   157,    16,
     170,    21,   122,    22,    23,    24,    21,   125,    48,    50,
      49,    51,    52,    51,   173,   133,   134,   135,   136,    51,
     138,   180,    18,    19,    20,    51,    22,    23,    19,    20,
     187,    22,    23,    52,   211,   212,   206,    19,    20,     5,
      22,    23,    51,    25,   164,   202,    51,    51,    51,   219,
     220,   199,   229,     4,    50,   232,     4,   214,     5,    50,
      19,    20,    49,    22,    23,   226,    11,   237,   184,   189,
     219,   202,   212,    -1,   232,    -1,    -1,   234,    -1,   236,
      -1,    -1,   202,    -1,    -1,    -1,   206,   207,    -1,    -1,
      -1,   211,   212,   211,   212,    -1,     0,     1,    -1,   219,
       4,     5,     6,     7,    -1,   225,   226,     5,     6,   229,
      -1,    -1,   232,    -1,   232,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    22,    23,    24,    -1,    -1,    -1,
      -1,    22,    23,    24,    -1,    39,    40,    41,    42,    43,
      44,    45,    -1,    -1,    48,    -1,    -1,    -1,    -1,    47,
      48,    49,    -1,    51,    52,    46,    47,    48,    49,    50,
      51,    52,    53,    18,    19,    -1,    -1,    -1,    -1,    -1,
      -1,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    58,   110,   111,     1,     4,     5,     6,     7,    39,
      40,    41,    42,    43,    44,    45,    48,    61,    65,    77,
      78,    79,    80,    81,    83,    84,    85,    86,    96,    97,
      98,    99,   101,   104,   105,   106,   107,   108,   109,     0,
      53,    53,    55,    71,    55,    51,    51,    51,    18,    19,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    59,     5,     6,    60,    60,    21,    60,
      21,    51,    60,    51,    60,    49,    68,    68,    68,    50,
      69,    68,    51,    69,    68,    68,    51,    51,    58,    63,
       5,    22,    23,    24,    46,    47,    48,    51,    52,    53,
      58,    62,    68,    69,    73,    87,    60,    60,    60,    62,
      88,    58,    69,    76,    60,    60,    68,    51,    62,    87,
      51,    51,   100,    51,    58,   103,     8,    51,    58,    64,
      62,    62,    62,    19,    20,    22,    23,    69,    17,    69,
      60,    16,    51,    58,    82,    58,    75,    75,    60,    60,
      60,    60,    64,    60,    47,    48,    51,    60,    62,    68,
      87,   102,    53,    54,    60,    25,    62,    62,    62,    62,
      62,    75,    69,    60,     4,    72,    75,    75,    75,    75,
      60,    75,    75,    58,    74,    69,    75,    71,    75,     4,
      70,    72,    60,     6,    18,    19,    26,    58,    64,    66,
       5,     5,    68,    58,    60,    62,    89,    92,    58,    69,
      90,    11,    13,    58,    93,    19,    48,    51,    58,    62,
      89,    91,    94,    95,    94,     9,    10,    60,    90,    18,
      69,    12,    16,    14,    67,    92,    67,    89,    95,    60,
      60,    69
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    57,    58,    59,    59,    59,    59,    60,    60,    61,
      61,    61,    61,    62,    62,    62,    62,    62,    62,    62,
      62,    63,    63,    63,    64,    64,    64,    65,    66,    66,
      66,    66,    67,    68,    69,    70,    70,    70,    70,    71,
      72,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    74,    74,    74,    74,    74,    75,    75,    76,    76,
      76,    77,    78,    79,    80,    81,    82,    82,    83,    84,
      85,    86,    87,    88,    88,    89,    89,    90,    90,    91,
      91,    91,    92,    93,    93,    93,    94,    94,    94,    95,
      95,    95,    95,    96,    97,    98,    99,   100,   101,   102,
     102,   102,   102,   102,   102,   103,   103,   104,   105,   105,
     105,   105,   105,   105,   105,   105,   105,   105,   105,   105,
     106,   107,   108,   108,   109,   109,   109,   109,   109,   109,
     109,   109,   110,   110,   110,   110,   111
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     1,     1,     1,     1,     1,     2,     1,
       1,     1,     1,     1,     3,     3,     3,     3,     2,     2,
       3,     1,     2,     2,     1,     2,     2,     6,     1,     1,
       1,     1,     1,     1,     1,     6,     8,     8,     4,     3,
       3,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     4,     4,     2,     2,     1,     2,     1,     1,
       3,     3,     3,     3,     4,     7,     1,     1,     5,     5,
       6,     6,     2,     2,     4,     1,     1,     1,     1,     2,
       2,     4,     2,     3,     3,     1,     1,     1,     3,     1,
       1,     1,     1,     6,     6,     6,     6,     1,     7,     1,
       1,     1,     1,     1,     1,     1,     2,     6,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       4,     3,     3,     4,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     2,     2,     1,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
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
        yyerror (&yylloc, scanner, builder, YY_("syntax error: cannot back up")); \
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
static int
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  int res = 0;
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
                  Type, Value, Location, scanner, builder); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, yyscan_t scanner, PyObject* builder)
{
  FILE *yyoutput = yyo;
  YYUSE (yyoutput);
  YYUSE (yylocationp);
  YYUSE (scanner);
  YYUSE (builder);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yytype], *yyvaluep);
# endif
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, yyscan_t scanner, PyObject* builder)
{
  YYFPRINTF (yyo, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  YY_LOCATION_PRINT (yyo, *yylocationp);
  YYFPRINTF (yyo, ": ");
  yy_symbol_value_print (yyo, yytype, yyvaluep, yylocationp, scanner, builder);
  YYFPRINTF (yyo, ")");
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
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp, int yyrule, yyscan_t scanner, PyObject* builder)
{
  unsigned long yylno = yyrline[yyrule];
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
                       &yyvsp[(yyi + 1) - (yynrhs)]
                       , &(yylsp[(yyi + 1) - (yynrhs)])                       , scanner, builder);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule, scanner, builder); \
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
            else
              goto append;

          append:
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

  return (YYSIZE_T) (yystpcpy (yyres, yystr) - yyres);
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
                  if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
                    yysize = yysize1;
                  else
                    return 2;
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
    if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
      yysize = yysize1;
    else
      return 2;
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
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, YYLTYPE *yylocationp, yyscan_t scanner, PyObject* builder)
{
  YYUSE (yyvaluep);
  YYUSE (yylocationp);
  YYUSE (scanner);
  YYUSE (builder);
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
yyparse (yyscan_t scanner, PyObject* builder)
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
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yynewstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  *yyssp = (yytype_int16) yystate;

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    goto yyexhaustedlab;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = (YYSIZE_T) (yyssp - yyss + 1);

# if defined yyoverflow
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
        yyss = yyss1;
        yyvs = yyvs1;
        yyls = yyls1;
      }
# else /* defined YYSTACK_RELOCATE */
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
# undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */

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
      yychar = yylex (&yylval, &yylloc, scanner, builder);
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
| yyreduce -- do a reduction.  |
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
#line 282 "beancount/parser/grammar.y"
    {
        (yyval.character) = '*';
    }
#line 1712 "beancount/parser/grammar.c"
    break;

  case 4:
#line 286 "beancount/parser/grammar.y"
    {
        (yyval.character) = (yyvsp[0].character);
    }
#line 1720 "beancount/parser/grammar.c"
    break;

  case 5:
#line 290 "beancount/parser/grammar.y"
    {
        (yyval.character) = '*';
    }
#line 1728 "beancount/parser/grammar.c"
    break;

  case 6:
#line 294 "beancount/parser/grammar.y"
    {
        (yyval.character) = '#';
    }
#line 1736 "beancount/parser/grammar.c"
    break;

  case 13:
#line 313 "beancount/parser/grammar.y"
    {
                (yyval.pyobj) = (yyvsp[0].pyobj);
            }
#line 1744 "beancount/parser/grammar.c"
    break;

  case 14:
#line 317 "beancount/parser/grammar.y"
    {
                (yyval.pyobj) = PyNumber_Add((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
                DECREF((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 1753 "beancount/parser/grammar.c"
    break;

  case 15:
#line 322 "beancount/parser/grammar.y"
    {
                (yyval.pyobj) = PyNumber_Subtract((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
                DECREF((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 1762 "beancount/parser/grammar.c"
    break;

  case 16:
#line 327 "beancount/parser/grammar.y"
    {
                (yyval.pyobj) = PyNumber_Multiply((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
                DECREF((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 1771 "beancount/parser/grammar.c"
    break;

  case 17:
#line 332 "beancount/parser/grammar.y"
    {
                (yyval.pyobj) = PyNumber_TrueDivide((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
                DECREF((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 1780 "beancount/parser/grammar.c"
    break;

  case 18:
#line 337 "beancount/parser/grammar.y"
    {
                (yyval.pyobj) = PyNumber_Negative((yyvsp[0].pyobj));
                DECREF((yyvsp[0].pyobj));
            }
#line 1789 "beancount/parser/grammar.c"
    break;

  case 19:
#line 342 "beancount/parser/grammar.y"
    {
                (yyval.pyobj) = (yyvsp[0].pyobj);
            }
#line 1797 "beancount/parser/grammar.c"
    break;

  case 20:
#line 346 "beancount/parser/grammar.y"
    {
                (yyval.pyobj) = (yyvsp[-1].pyobj);
            }
#line 1805 "beancount/parser/grammar.c"
    break;

  case 21:
#line 351 "beancount/parser/grammar.y"
    {
                Py_INCREF(Py_None);
                (yyval.pyobj) = Py_None;
            }
#line 1814 "beancount/parser/grammar.c"
    break;

  case 22:
#line 356 "beancount/parser/grammar.y"
    {
                CALL(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                     (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
            }
#line 1823 "beancount/parser/grammar.c"
    break;

  case 23:
#line 361 "beancount/parser/grammar.y"
    {
                CALL(,
                     (yyval.pyobj), "pipe_deprecated_error", "");
                (yyval.pyobj) = (yyvsp[-1].pyobj);
            }
#line 1833 "beancount/parser/grammar.c"
    break;

  case 24:
#line 368 "beancount/parser/grammar.y"
    {
               /* Note: We're passing a bogus value here in order to avoid
                * having to declare a second macro just for this one special
                * case. */
               CALL(,
                    (yyval.pyobj), "tag_link_new", "O", Py_None);
           }
#line 1845 "beancount/parser/grammar.c"
    break;

  case 25:
#line 376 "beancount/parser/grammar.y"
    {
               CALL(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                    (yyval.pyobj), "tag_link_LINK", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
           }
#line 1854 "beancount/parser/grammar.c"
    break;

  case 26:
#line 381 "beancount/parser/grammar.y"
    {
               CALL(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                    (yyval.pyobj), "tag_link_TAG", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
           }
#line 1863 "beancount/parser/grammar.c"
    break;

  case 27:
#line 387 "beancount/parser/grammar.y"
    {
                CALL(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                     (yyval.pyobj), "transaction", "ObOOO", (yyvsp[-5].pyobj), (yyvsp[-4].character), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 1872 "beancount/parser/grammar.c"
    break;

  case 28:
#line 393 "beancount/parser/grammar.y"
    {
            (yyval.character) = '\0';
        }
#line 1880 "beancount/parser/grammar.c"
    break;

  case 29:
#line 397 "beancount/parser/grammar.y"
    {
            (yyval.character) = '*';
        }
#line 1888 "beancount/parser/grammar.c"
    break;

  case 30:
#line 401 "beancount/parser/grammar.y"
    {
            (yyval.character) = '#';
        }
#line 1896 "beancount/parser/grammar.c"
    break;

  case 32:
#line 407 "beancount/parser/grammar.y"
    {
                     (yyval.pyobj) = (yyvsp[0].pyobj);
                 }
#line 1904 "beancount/parser/grammar.c"
    break;

  case 33:
#line 412 "beancount/parser/grammar.y"
    {
            CALL(DECREF((yyvsp[0].pyobj)),
                 (yyval.pyobj), "account", "O", (yyvsp[0].pyobj));
        }
#line 1913 "beancount/parser/grammar.c"
    break;

  case 34:
#line 418 "beancount/parser/grammar.y"
    {
            CALL(DECREF((yyvsp[0].pyobj)),
                 (yyval.pyobj), "currency", "O", (yyvsp[0].pyobj));
        }
#line 1922 "beancount/parser/grammar.c"
    break;

  case 35:
#line 424 "beancount/parser/grammar.y"
    {
            CALL(DECREF((yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[-1].pyobj)),
                 (yyval.pyobj), "posting", "OOOOOb", (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[-1].pyobj), Py_None, Py_False, (yyvsp[-4].character));
        }
#line 1931 "beancount/parser/grammar.c"
    break;

  case 36:
#line 429 "beancount/parser/grammar.y"
    {
            CALL(DECREF((yyvsp[-5].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj)),
                 (yyval.pyobj), "posting", "OOOOOb", (yyvsp[-5].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj), Py_False, (yyvsp[-6].character));
        }
#line 1940 "beancount/parser/grammar.c"
    break;

  case 37:
#line 434 "beancount/parser/grammar.y"
    {
            CALL(DECREF((yyvsp[-5].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj)),
                 (yyval.pyobj), "posting", "OOOOOb", (yyvsp[-5].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj), Py_True, (yyvsp[-6].character));
        }
#line 1949 "beancount/parser/grammar.c"
    break;

  case 38:
#line 439 "beancount/parser/grammar.y"
    {
            CALL(DECREF((yyvsp[-1].pyobj)),
                 (yyval.pyobj), "posting", "OOOOOb", (yyvsp[-1].pyobj), missing, Py_None, Py_None, Py_False, (yyvsp[-2].character));
        }
#line 1958 "beancount/parser/grammar.c"
    break;

  case 39:
#line 445 "beancount/parser/grammar.y"
    {
              CALL(DECREF((yyvsp[-1].string), (yyvsp[0].pyobj)),
                   (yyval.pyobj), "key_value", "OO", (yyvsp[-1].string), (yyvsp[0].pyobj));
          }
#line 1967 "beancount/parser/grammar.c"
    break;

  case 40:
#line 451 "beancount/parser/grammar.y"
    {
                   (yyval.pyobj) = (yyvsp[-1].pyobj);
               }
#line 1975 "beancount/parser/grammar.c"
    break;

  case 49:
#line 464 "beancount/parser/grammar.y"
    {
                    (yyval.pyobj) = (yyvsp[0].pyobj);
                }
#line 1983 "beancount/parser/grammar.c"
    break;

  case 50:
#line 468 "beancount/parser/grammar.y"
    {
                    Py_INCREF(Py_None);
                    (yyval.pyobj) = Py_None;
                }
#line 1992 "beancount/parser/grammar.c"
    break;

  case 51:
#line 474 "beancount/parser/grammar.y"
    {
                       Py_INCREF(Py_None);
                       (yyval.pyobj) = Py_None;
                   }
#line 2001 "beancount/parser/grammar.c"
    break;

  case 52:
#line 479 "beancount/parser/grammar.y"
    {
                       (yyval.pyobj) = (yyvsp[-3].pyobj);
                   }
#line 2009 "beancount/parser/grammar.c"
    break;

  case 53:
#line 483 "beancount/parser/grammar.y"
    {
                       CALL(DECREF((yyvsp[-3].pyobj), (yyvsp[-1].pyobj)),
                            (yyval.pyobj), "handle_list", "OO", (yyvsp[-3].pyobj), (yyvsp[-1].pyobj));
                   }
#line 2018 "beancount/parser/grammar.c"
    break;

  case 54:
#line 488 "beancount/parser/grammar.y"
    {
                       CALL(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                            (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                   }
#line 2027 "beancount/parser/grammar.c"
    break;

  case 55:
#line 493 "beancount/parser/grammar.y"
    {
                       CALL(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                            (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                   }
#line 2036 "beancount/parser/grammar.c"
    break;

  case 56:
#line 499 "beancount/parser/grammar.y"
    {
                   Py_INCREF(Py_None);
                   (yyval.pyobj) = Py_None;
               }
#line 2045 "beancount/parser/grammar.c"
    break;

  case 57:
#line 504 "beancount/parser/grammar.y"
    {
                   CALL(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                        (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
               }
#line 2054 "beancount/parser/grammar.c"
    break;

  case 58:
#line 510 "beancount/parser/grammar.y"
    {
                  Py_INCREF(Py_None);
                  (yyval.pyobj) = Py_None;
              }
#line 2063 "beancount/parser/grammar.c"
    break;

  case 59:
#line 515 "beancount/parser/grammar.y"
    {
                  CALL(DECREF((yyvsp[0].pyobj)),
                       (yyval.pyobj), "handle_list", "OO", Py_None, (yyvsp[0].pyobj));
              }
#line 2072 "beancount/parser/grammar.c"
    break;

  case 60:
#line 520 "beancount/parser/grammar.y"
    {
                  CALL(DECREF((yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                       (yyval.pyobj), "handle_list", "OO", (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
              }
#line 2081 "beancount/parser/grammar.c"
    break;

  case 61:
#line 526 "beancount/parser/grammar.y"
    {
             CALL(DECREF((yyvsp[-1].pyobj)),
                  (yyval.pyobj), "pushtag", "O", (yyvsp[-1].pyobj));
         }
#line 2090 "beancount/parser/grammar.c"
    break;

  case 62:
#line 532 "beancount/parser/grammar.y"
    {
           CALL(DECREF((yyvsp[-1].pyobj)),
                (yyval.pyobj), "poptag", "O", (yyvsp[-1].pyobj));
       }
#line 2099 "beancount/parser/grammar.c"
    break;

  case 63:
#line 538 "beancount/parser/grammar.y"
    {
             /* Note: key_value is a tuple, Py_BuildValue() won't wrap it up
              * within a tuple, so expand in the method (it receives two
              * objects). See https://docs.python.org/3.4/c-api/arg.html. */
             CALL(DECREF((yyvsp[-1].pyobj)),
                  (yyval.pyobj), "pushmeta", "O", (yyvsp[-1].pyobj));
         }
#line 2111 "beancount/parser/grammar.c"
    break;

  case 64:
#line 547 "beancount/parser/grammar.y"
    {
            CALL(DECREF((yyvsp[-2].pyobj)),
                 (yyval.pyobj), "popmeta", "O", (yyvsp[-2].pyobj));
        }
#line 2120 "beancount/parser/grammar.c"
    break;

  case 65:
#line 553 "beancount/parser/grammar.y"
    {
         CALL(DECREF((yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
              (yyval.pyobj), "open", "OOOOO", (yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
         ;
     }
#line 2130 "beancount/parser/grammar.c"
    break;

  case 66:
#line 560 "beancount/parser/grammar.y"
    {
                (yyval.pyobj) = (yyvsp[0].pyobj);
            }
#line 2138 "beancount/parser/grammar.c"
    break;

  case 67:
#line 564 "beancount/parser/grammar.y"
    {
                Py_INCREF(Py_None);
                (yyval.pyobj) = Py_None;
            }
#line 2147 "beancount/parser/grammar.c"
    break;

  case 68:
#line 570 "beancount/parser/grammar.y"
    {
          CALL(DECREF((yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "close", "OOO", (yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      }
#line 2156 "beancount/parser/grammar.c"
    break;

  case 69:
#line 576 "beancount/parser/grammar.y"
    {
              CALL(DECREF((yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                   (yyval.pyobj), "commodity", "OOO", (yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
          }
#line 2165 "beancount/parser/grammar.c"
    break;

  case 70:
#line 582 "beancount/parser/grammar.y"
    {
        CALL(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
             (yyval.pyobj), "pad", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
    }
#line 2174 "beancount/parser/grammar.c"
    break;

  case 71:
#line 588 "beancount/parser/grammar.y"
    {
            CALL(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[0].pyobj), (yyvsp[-2].pairobj).pyobj1, (yyvsp[-2].pairobj).pyobj2),
                 (yyval.pyobj), "balance", "OOOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pairobj).pyobj1, (yyvsp[-2].pairobj).pyobj2, (yyvsp[0].pyobj));
        }
#line 2183 "beancount/parser/grammar.c"
    break;

  case 72:
#line 594 "beancount/parser/grammar.y"
    {
           CALL(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                (yyval.pyobj), "amount", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
       }
#line 2192 "beancount/parser/grammar.c"
    break;

  case 73:
#line 600 "beancount/parser/grammar.y"
    {
                     CALL(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                          (yyval.pairobj).pyobj1, "amount", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                     (yyval.pairobj).pyobj2 = Py_None;
                     Py_INCREF(Py_None);
                     ;
                 }
#line 2204 "beancount/parser/grammar.c"
    break;

  case 74:
#line 608 "beancount/parser/grammar.y"
    {
                     CALL(DECREF((yyvsp[-3].pyobj), (yyvsp[0].pyobj)),
                          (yyval.pairobj).pyobj1, "amount", "OO", (yyvsp[-3].pyobj), (yyvsp[0].pyobj));
                     (yyval.pairobj).pyobj2 = (yyvsp[-1].pyobj);
                 }
#line 2214 "beancount/parser/grammar.c"
    break;

  case 75:
#line 615 "beancount/parser/grammar.y"
    {
                 Py_INCREF(missing);
                 (yyval.pyobj) = missing;
             }
#line 2223 "beancount/parser/grammar.c"
    break;

  case 76:
#line 620 "beancount/parser/grammar.y"
    {
                 (yyval.pyobj) = (yyvsp[0].pyobj);
             }
#line 2231 "beancount/parser/grammar.c"
    break;

  case 77:
#line 625 "beancount/parser/grammar.y"
    {
                 Py_INCREF(missing);
                 (yyval.pyobj) = missing;
             }
#line 2240 "beancount/parser/grammar.c"
    break;

  case 78:
#line 630 "beancount/parser/grammar.y"
    {
                 (yyval.pyobj) = (yyvsp[0].pyobj);
             }
#line 2248 "beancount/parser/grammar.c"
    break;

  case 79:
#line 635 "beancount/parser/grammar.y"
    {
                    CALL(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                         (yyval.pyobj), "compound_amount", "OOO", (yyvsp[-1].pyobj), Py_None, (yyvsp[0].pyobj));
                }
#line 2257 "beancount/parser/grammar.c"
    break;

  case 80:
#line 640 "beancount/parser/grammar.y"
    {
                    CALL(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                         (yyval.pyobj), "compound_amount", "OOO", (yyvsp[-1].pyobj), Py_None, (yyvsp[0].pyobj));
                }
#line 2266 "beancount/parser/grammar.c"
    break;

  case 81:
#line 645 "beancount/parser/grammar.y"
    {
                    CALL(DECREF((yyvsp[-3].pyobj), (yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                         (yyval.pyobj), "compound_amount", "OOO", (yyvsp[-3].pyobj), (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                    ;
                }
#line 2276 "beancount/parser/grammar.c"
    break;

  case 82:
#line 652 "beancount/parser/grammar.y"
    {
                      CALL(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                           (yyval.pyobj), "amount", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                 }
#line 2285 "beancount/parser/grammar.c"
    break;

  case 83:
#line 658 "beancount/parser/grammar.y"
    {
              CALL(DECREF((yyvsp[-1].pyobj)),
                   (yyval.pyobj), "cost_spec", "OO", (yyvsp[-1].pyobj), Py_False);
          }
#line 2294 "beancount/parser/grammar.c"
    break;

  case 84:
#line 663 "beancount/parser/grammar.y"
    {
              CALL(DECREF((yyvsp[-1].pyobj)),
                   (yyval.pyobj), "cost_spec", "OO", (yyvsp[-1].pyobj), Py_True);
          }
#line 2303 "beancount/parser/grammar.c"
    break;

  case 85:
#line 668 "beancount/parser/grammar.y"
    {
              Py_INCREF(Py_None);
              (yyval.pyobj) = Py_None;
          }
#line 2312 "beancount/parser/grammar.c"
    break;

  case 86:
#line 674 "beancount/parser/grammar.y"
    {
                   /* We indicate that there was a cost if there */
                   (yyval.pyobj) = PyList_New(0);
               }
#line 2321 "beancount/parser/grammar.c"
    break;

  case 87:
#line 679 "beancount/parser/grammar.y"
    {
                   CALL(DECREF((yyvsp[0].pyobj)),
                        (yyval.pyobj), "handle_list", "OO", Py_None, (yyvsp[0].pyobj));
               }
#line 2330 "beancount/parser/grammar.c"
    break;

  case 88:
#line 684 "beancount/parser/grammar.y"
    {
                   CALL(DECREF((yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                        (yyval.pyobj), "handle_list", "OO", (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
               }
#line 2339 "beancount/parser/grammar.c"
    break;

  case 89:
#line 690 "beancount/parser/grammar.y"
    {
              (yyval.pyobj) = (yyvsp[0].pyobj);
          }
#line 2347 "beancount/parser/grammar.c"
    break;

  case 90:
#line 694 "beancount/parser/grammar.y"
    {
              (yyval.pyobj) = (yyvsp[0].pyobj);
          }
#line 2355 "beancount/parser/grammar.c"
    break;

  case 91:
#line 698 "beancount/parser/grammar.y"
    {
              (yyval.pyobj) = (yyvsp[0].pyobj);
          }
#line 2363 "beancount/parser/grammar.c"
    break;

  case 92:
#line 702 "beancount/parser/grammar.y"
    {
              CALL(,
                   (yyval.pyobj), "cost_merge", "O", Py_None);
          }
#line 2372 "beancount/parser/grammar.c"
    break;

  case 93:
#line 709 "beancount/parser/grammar.y"
    {
          CALL(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "price", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      }
#line 2381 "beancount/parser/grammar.c"
    break;

  case 94:
#line 715 "beancount/parser/grammar.y"
    {
          CALL(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "event", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      }
#line 2390 "beancount/parser/grammar.c"
    break;

  case 95:
#line 721 "beancount/parser/grammar.y"
    {
             CALL(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                  (yyval.pyobj), "query", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
         }
#line 2399 "beancount/parser/grammar.c"
    break;

  case 96:
#line 727 "beancount/parser/grammar.y"
    {
          CALL(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "note", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      }
#line 2408 "beancount/parser/grammar.c"
    break;

  case 98:
#line 735 "beancount/parser/grammar.y"
    {
             CALL(DECREF((yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                  (yyval.pyobj), "document", "OOOOO", (yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
         }
#line 2417 "beancount/parser/grammar.c"
    break;

  case 99:
#line 742 "beancount/parser/grammar.y"
    {
                 CALL(DECREF((yyvsp[0].pyobj)),
                      (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
             }
#line 2426 "beancount/parser/grammar.c"
    break;

  case 100:
#line 747 "beancount/parser/grammar.y"
    {
                 CALL(DECREF((yyvsp[0].pyobj)),
                      (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
             }
#line 2435 "beancount/parser/grammar.c"
    break;

  case 101:
#line 752 "beancount/parser/grammar.y"
    {
                 CALL(DECREF((yyvsp[0].pyobj)),
                      (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
             }
#line 2444 "beancount/parser/grammar.c"
    break;

  case 102:
#line 757 "beancount/parser/grammar.y"
    {
                 CALL(DECREF((yyvsp[0].pyobj)),
                      (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
             }
#line 2453 "beancount/parser/grammar.c"
    break;

  case 103:
#line 762 "beancount/parser/grammar.y"
    {
                 CALL(DECREF((yyvsp[0].pyobj)),
                      (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
             }
#line 2462 "beancount/parser/grammar.c"
    break;

  case 104:
#line 767 "beancount/parser/grammar.y"
    {
                 /* Obtain beancount.core.account.TYPE */
                 PyObject* module = PyImport_ImportModule("beancount.core.account");
                 PyObject* dtype = PyObject_GetAttrString(module, "TYPE");
                 Py_DECREF(module);
                 CALL(DECREF((yyvsp[0].pyobj), dtype),
                      (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), dtype);
             }
#line 2475 "beancount/parser/grammar.c"
    break;

  case 105:
#line 777 "beancount/parser/grammar.y"
    {
                      Py_INCREF(Py_None);
                      (yyval.pyobj) = Py_None;
                  }
#line 2484 "beancount/parser/grammar.c"
    break;

  case 106:
#line 782 "beancount/parser/grammar.y"
    {
                      CALL(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                           (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                  }
#line 2493 "beancount/parser/grammar.c"
    break;

  case 107:
#line 788 "beancount/parser/grammar.y"
    {
           CALL(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                (yyval.pyobj), "custom", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
       }
#line 2502 "beancount/parser/grammar.c"
    break;

  case 119:
#line 806 "beancount/parser/grammar.y"
    {
          (yyval.pyobj) = (yyvsp[0].pyobj);
      }
#line 2510 "beancount/parser/grammar.c"
    break;

  case 120:
#line 811 "beancount/parser/grammar.y"
    {
           CALL(DECREF((yyvsp[-2].pyobj), (yyvsp[-1].pyobj)),
                (yyval.pyobj), "option", "OO", (yyvsp[-2].pyobj), (yyvsp[-1].pyobj));
       }
#line 2519 "beancount/parser/grammar.c"
    break;

  case 121:
#line 817 "beancount/parser/grammar.y"
    {
           CALL(DECREF((yyvsp[-1].pyobj)),
                (yyval.pyobj), "include", "O", (yyvsp[-1].pyobj));
       }
#line 2528 "beancount/parser/grammar.c"
    break;

  case 122:
#line 823 "beancount/parser/grammar.y"
    {
           CALL(DECREF((yyvsp[-1].pyobj)),
                (yyval.pyobj), "plugin", "OO", (yyvsp[-1].pyobj), Py_None);
       }
#line 2537 "beancount/parser/grammar.c"
    break;

  case 123:
#line 828 "beancount/parser/grammar.y"
    {
           CALL(DECREF((yyvsp[-2].pyobj), (yyvsp[-1].pyobj)),
                (yyval.pyobj), "plugin", "OO", (yyvsp[-2].pyobj), (yyvsp[-1].pyobj));
       }
#line 2546 "beancount/parser/grammar.c"
    break;

  case 132:
#line 844 "beancount/parser/grammar.y"
    {
                 (yyval.pyobj) = (yyvsp[-1].pyobj);
             }
#line 2554 "beancount/parser/grammar.c"
    break;

  case 133:
#line 848 "beancount/parser/grammar.y"
    {
                 CALL(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                      (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
             }
#line 2563 "beancount/parser/grammar.c"
    break;

  case 134:
#line 853 "beancount/parser/grammar.y"
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
#line 2585 "beancount/parser/grammar.c"
    break;

  case 135:
#line 871 "beancount/parser/grammar.y"
    {
                  Py_INCREF(Py_None);
                  (yyval.pyobj) = Py_None;
             }
#line 2594 "beancount/parser/grammar.c"
    break;

  case 136:
#line 878 "beancount/parser/grammar.y"
    {
         /* If a Python exception has been raised and not handled, abort. In
          * case of unrecoverable error, the lexer raises a Python exception and
          * the yylex() function returns -1, whcih is translated by Bison into
          * an EOF token, handled here. */
         if (PyErr_Occurred()) {
             YYABORT;
         }
         CALL(DECREF((yyvsp[0].pyobj)),
              (yyval.pyobj), "store_result", "O", (yyvsp[0].pyobj));
     }
#line 2610 "beancount/parser/grammar.c"
    break;


#line 2614 "beancount/parser/grammar.c"

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
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

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
      yyerror (&yylloc, scanner, builder, YY_("syntax error"));
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
        yyerror (&yylloc, scanner, builder, yymsgp);
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
                      yytoken, &yylval, &yylloc, scanner, builder);
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
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;

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
                  yystos[yystate], yyvsp, yylsp, scanner, builder);
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
  yyerror (&yylloc, scanner, builder, YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif


/*-----------------------------------------------------.
| yyreturn -- parsing is finished, return the result.  |
`-----------------------------------------------------*/
yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, &yylloc, scanner, builder);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp, yylsp, scanner, builder);
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
#line 893 "beancount/parser/grammar.y"


/* Get a printable version of a token name. */
const char* token_to_string(int token)
{
    return yytname[YYTRANSLATE(token)];
}

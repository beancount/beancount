/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2013 Free Software Foundation, Inc.

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
#define YYBISON_VERSION "3.0.2"

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

/* First line of reported file/line string. This is used as #line. */
int yy_firstline;

#define FILE_LINE_ARGS  yy_filename, ((yyloc).first_line + yy_firstline)

/* Error-handling function. */
void yyerror(char const* message)
{
    /* Register a syntax error with the builder. */
    BUILD("error", "ssi", message, yy_filename, yylineno + yy_firstline);
}

/* Get a printable version of a token name. */
const char* getTokenName(int token);


/* #define DECREF1(x)  Py_DECREF(x); */
#define DECREF1(x1)
#define DECREF2(x1, x2)
#define DECREF3(x1, x2, x3)
#define DECREF4(x1, x2, x3, x4)
#define DECREF5(x1, x2, x3, x4, x5)
#define DECREF6(x1, x2, x3, x4, x5, x6)


#line 99 "src/python/beancount/parser/grammar.c" /* yacc.c:339  */

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
    ERROR = 258,
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
    SLASH = 273,
    PLUS = 274,
    FLAG = 275,
    TXN = 276,
    BALANCE = 277,
    OPEN = 278,
    CLOSE = 279,
    COMMODITY = 280,
    PAD = 281,
    EVENT = 282,
    PRICE = 283,
    NOTE = 284,
    DOCUMENT = 285,
    PUSHTAG = 286,
    POPTAG = 287,
    OPTION = 288,
    INCLUDE = 289,
    PLUGIN = 290,
    BOOL = 291,
    DATE = 292,
    ACCOUNT = 293,
    CURRENCY = 294,
    STRING = 295,
    NUMBER = 296,
    TAG = 297,
    LINK = 298,
    KEY = 299
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE YYSTYPE;
union YYSTYPE
{
#line 59 "src/python/beancount/parser/grammar.y" /* yacc.c:355  */

    char character;
    const char* string;
    PyObject* pyobj;
    struct {
        PyObject* pyobj1;
        PyObject* pyobj2;
    } pairobj;

#line 194 "src/python/beancount/parser/grammar.c" /* yacc.c:355  */
};
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

#line 222 "src/python/beancount/parser/grammar.c" /* yacc.c:358  */

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
#define YYLAST   168

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  45
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  43
/* YYNRULES -- Number of rules.  */
#define YYNRULES  99
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  175

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   299

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
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   159,   159,   162,   166,   171,   172,   176,   177,   178,
     179,   180,   186,   190,   195,   199,   204,   209,   214,   221,
     227,   231,   233,   238,   243,   248,   254,   260,   261,   262,
     263,   264,   265,   266,   267,   271,   277,   282,   287,   293,
     298,   304,   309,   314,   320,   326,   332,   338,   342,   348,
     354,   360,   366,   373,   379,   386,   393,   398,   403,   408,
     414,   419,   425,   431,   436,   441,   447,   451,   455,   461,
     467,   473,   479,   481,   487,   488,   489,   490,   491,   492,
     493,   494,   495,   496,   501,   507,   513,   518,   524,   525,
     526,   527,   528,   529,   530,   533,   537,   542,   546,   552
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "ERROR", "INDENT", "EOL", "COMMENT",
  "SKIPPED", "PIPE", "ATAT", "AT", "LCURLCURL", "RCURLCURL", "LCURL",
  "RCURL", "EQUAL", "COMMA", "TILDE", "SLASH", "PLUS", "FLAG", "TXN",
  "BALANCE", "OPEN", "CLOSE", "COMMODITY", "PAD", "EVENT", "PRICE", "NOTE",
  "DOCUMENT", "PUSHTAG", "POPTAG", "OPTION", "INCLUDE", "PLUGIN", "BOOL",
  "DATE", "ACCOUNT", "CURRENCY", "STRING", "NUMBER", "TAG", "LINK", "KEY",
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
     295,   296,   297,   298,   299
};
# endif

#define YYPACT_NINF -115

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-115)))

#define YYTABLE_NINF -100

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -115,  -115,    54,     9,  -115,    15,  -115,    20,  -115,    -9,
      35,    41,    42,    50,    75,  -115,  -115,  -115,  -115,  -115,
    -115,  -115,  -115,  -115,  -115,  -115,  -115,  -115,  -115,  -115,
    -115,  -115,  -115,  -115,  -115,  -115,    13,    13,    52,    13,
       6,  -115,  -115,    46,    55,    68,    40,    72,    81,    83,
      85,    86,  -115,  -115,   120,  -115,  -115,    13,  -115,    13,
    -115,    88,    91,    13,    13,    94,    93,    88,    95,    96,
    -115,     2,  -115,  -115,  -115,  -115,    -2,    13,  -115,  -115,
     -11,  -115,  -115,    13,    13,     4,    13,    13,  -115,    13,
    -115,  -115,  -115,  -115,  -115,    88,    97,  -115,  -115,    98,
    -115,  -115,    13,  -115,   130,   130,  -115,  -115,  -115,  -115,
    -115,  -115,  -115,   135,    39,  -115,   130,  -115,  -115,    99,
    -115,   130,   130,   130,   130,   130,   -14,  -115,  -115,  -115,
     130,    78,  -115,  -115,   102,  -115,  -115,  -115,  -115,  -115,
      67,  -115,  -115,    13,  -115,     8,  -115,  -115,   128,    65,
      71,  -115,    88,    88,  -115,  -115,  -115,  -115,    10,    44,
    -115,    18,  -115,    13,    13,   101,  -115,  -115,    71,  -115,
    -115,  -115,   105,  -115,  -115
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,    98,     0,     0,    97,    10,     7,    11,    88,     0,
       0,     0,     0,     0,     0,    89,    74,    90,    91,    76,
      77,    83,    78,    75,    82,    79,    80,    81,    96,    92,
      93,    94,    95,     1,     9,     8,     0,     0,     0,     0,
       0,     4,     3,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     2,     5,     0,    44,    45,     0,    85,     0,
      86,     0,     2,     0,     0,     0,     0,     0,     0,     0,
      14,     0,     6,    84,    87,    12,     0,     0,    42,    41,
       2,     2,     2,     0,     0,     0,     0,     0,    72,     0,
      18,    15,    17,    16,     2,     0,     0,    54,     2,     0,
      47,    48,     0,    39,    49,    50,     2,     2,    53,     2,
       2,     2,    36,    19,     0,    13,    52,    43,     2,     0,
      40,    51,    70,    69,    71,    73,     2,    38,    37,    55,
      46,     2,    21,    20,     0,    33,    29,    28,    30,    27,
      12,    31,    35,     0,    34,     0,    26,    25,    60,     0,
       2,    61,     0,     0,    22,    67,    68,    57,    56,     0,
      66,     0,    64,     0,     0,     2,    58,    62,     2,    24,
      23,    56,     0,    65,    59
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
    -115,     0,  -115,   -36,  -115,   -59,  -115,  -115,  -115,  -115,
      32,  -115,  -115,   -42,  -115,  -115,  -115,  -115,  -115,  -115,
    -115,  -115,  -115,  -114,  -115,   -19,  -115,  -115,  -115,  -115,
     -21,  -115,  -115,  -115,  -115,  -115,  -115,  -115,  -115,  -115,
    -115,  -115,  -115
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,   103,    52,    55,    15,    85,    71,    16,   134,   127,
     120,   143,   113,   104,    80,    17,    18,    19,   102,    20,
      21,    22,    23,    86,    77,   159,   160,   149,   151,   161,
     162,    24,    25,    26,    89,    27,    28,    29,    30,    31,
      32,     2,     3
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
       1,    56,    76,    58,    60,    99,   132,    53,    54,    33,
      90,    53,    54,    53,    54,    95,    96,   144,    53,    54,
      34,    73,    96,    74,   -63,    35,   -63,    81,    82,   100,
     131,   148,   167,    36,   168,    94,   114,    97,   163,   164,
     105,    98,    91,   108,    92,    93,    59,   106,   107,    75,
     109,   110,    70,   111,   -99,     4,   116,    96,     5,     6,
       7,     8,    79,   165,   121,   122,   118,   123,   124,   125,
      53,    54,   -32,   -32,   152,   153,   130,    37,   129,    64,
     101,    38,    39,   166,    61,     9,    10,    11,    12,    13,
      40,    14,    57,    62,   112,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    63,   146,   155,   147,
      65,   156,   157,   154,   135,   136,   137,   138,   139,   140,
     141,    66,    67,    68,    69,    72,   133,   169,   170,    75,
      78,   142,    83,    84,   119,    87,    88,   117,   115,   126,
     145,   150,   157,   131,   174,   128,   172,   173,     0,     0,
     158,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   171,     0,     0,   171
};

static const yytype_int16 yycheck[] =
{
       0,    37,    61,    39,    40,    16,    20,     5,     6,     0,
       8,     5,     6,     5,     6,    17,    18,   131,     5,     6,
       5,    57,    18,    59,    14,     5,    16,    63,    64,    40,
      44,   145,    14,    42,    16,    71,    95,    39,   152,   153,
      82,    77,    40,    39,    42,    43,    40,    83,    84,    41,
      86,    87,    52,    89,     0,     1,    98,    18,     4,     5,
       6,     7,    62,    19,   106,   107,   102,   109,   110,   111,
       5,     6,     5,     6,     9,    10,   118,    42,    39,    39,
      80,    40,    40,    39,    38,    31,    32,    33,    34,    35,
      40,    37,    40,    38,    94,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    38,   143,    37,   145,
      38,    40,    41,   149,    36,    37,    38,    39,    40,    41,
      42,    40,    39,    38,    38,     5,   126,   163,   164,    41,
      39,   131,    38,    40,     4,    40,    40,    39,    41,     4,
      38,    13,    41,    44,    39,   113,   165,   168,    -1,    -1,
     150,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   165,    -1,    -1,   168
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    46,    86,    87,     1,     4,     5,     6,     7,    31,
      32,    33,    34,    35,    37,    49,    52,    60,    61,    62,
      64,    65,    66,    67,    76,    77,    78,    80,    81,    82,
      83,    84,    85,     0,     5,     5,    42,    42,    40,    40,
      40,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    47,     5,     6,    48,    48,    40,    48,    40,
      48,    38,    38,    38,    39,    38,    40,    39,    38,    38,
      46,    51,     5,    48,    48,    41,    50,    69,    39,    46,
      59,    48,    48,    38,    40,    50,    68,    40,    40,    79,
       8,    40,    42,    43,    48,    17,    18,    39,    48,    16,
      40,    46,    63,    46,    58,    58,    48,    48,    39,    48,
      48,    48,    46,    57,    50,    41,    58,    39,    48,     4,
      55,    58,    58,    58,    58,    58,     4,    54,    55,    39,
      58,    44,    20,    46,    53,    36,    37,    38,    39,    40,
      41,    42,    46,    56,    68,    38,    48,    48,    68,    72,
      13,    73,     9,    10,    48,    37,    40,    41,    46,    70,
      71,    74,    75,    68,    68,    19,    39,    14,    16,    48,
      48,    46,    70,    75,    39
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    45,    46,    47,    47,    48,    48,    49,    49,    49,
      49,    49,    50,    50,    51,    51,    51,    51,    51,    52,
      53,    53,    54,    54,    54,    54,    55,    56,    56,    56,
      56,    56,    56,    56,    56,    56,    57,    57,    57,    58,
      58,    59,    59,    59,    60,    61,    62,    63,    63,    64,
      65,    66,    67,    68,    69,    69,    70,    70,    71,    71,
      72,    72,    73,    74,    74,    74,    75,    75,    75,    76,
      77,    78,    79,    80,    81,    81,    81,    81,    81,    81,
      81,    81,    81,    81,    82,    83,    84,    84,    85,    85,
      85,    85,    85,    85,    85,    86,    86,    86,    86,    87
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     1,     1,     1,     2,     1,     2,     2,
       1,     1,     1,     3,     1,     2,     2,     2,     2,     5,
       1,     1,     5,     7,     7,     4,     4,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     2,     2,     1,
       2,     1,     1,     3,     3,     3,     7,     1,     1,     5,
       5,     6,     6,     2,     2,     4,     1,     1,     2,     4,
       1,     2,     3,     1,     1,     3,     1,     1,     1,     6,
       6,     6,     1,     6,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     4,     3,     3,     4,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     2,     1,     1
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
#line 163 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
        (yyval.character) = '*';
    }
#line 1529 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 4:
#line 167 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
        (yyval.character) = (yyvsp[0].character);
    }
#line 1537 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 12:
#line 187 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
                (yyval.pyobj) = (yyvsp[0].pyobj);
            }
#line 1545 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 13:
#line 191 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
                (yyval.pyobj) = PyNumber_TrueDivide((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 1553 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 14:
#line 196 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
               (yyval.pyobj) = BUILD_NOARGS("txn_field_new");
           }
#line 1561 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 15:
#line 200 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
               (yyval.pyobj) = BUILD("txn_field_STRING", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
               DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj));
           }
#line 1570 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 16:
#line 205 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
               (yyval.pyobj) = BUILD("txn_field_LINK", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
               DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj));
           }
#line 1579 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 17:
#line 210 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
               (yyval.pyobj) = BUILD("txn_field_TAG", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
               DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj));
           }
#line 1588 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 18:
#line 215 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
               /* Mark PIPE as present for backwards compatibility and raise an error */
               (yyval.pyobj) = BUILD("txn_field_PIPE", "OO", (yyvsp[-1].pyobj), Py_None);
               DECREF1((yyvsp[-1].pyobj));
           }
#line 1598 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 19:
#line 222 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
                (yyval.pyobj) = BUILD("transaction", "siObOO", FILE_LINE_ARGS, (yyvsp[-4].pyobj), (yyvsp[-3].character), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
                DECREF4((yyvsp[-4].pyobj), (yyvsp[-3].character), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 1607 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 20:
#line 228 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
            (yyval.character) = '\0';
        }
#line 1615 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 22:
#line 234 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
            (yyval.pyobj) = BUILD("posting", "siOOOOb", FILE_LINE_ARGS, (yyvsp[-2].pyobj), (yyvsp[-1].pyobj), Py_None, Py_False, (yyvsp[-3].character));
            DECREF2((yyvsp[-2].pyobj), (yyvsp[-1].pyobj));
        }
#line 1624 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 23:
#line 239 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
            (yyval.pyobj) = BUILD("posting", "siOOOOb", FILE_LINE_ARGS, (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj), Py_False, (yyvsp[-5].character));
            DECREF3((yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj));
        }
#line 1633 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 24:
#line 244 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
            (yyval.pyobj) = BUILD("posting", "siOOOOb", FILE_LINE_ARGS, (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj), Py_True, (yyvsp[-5].character));
            DECREF3((yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj));
        }
#line 1642 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 25:
#line 249 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
            (yyval.pyobj) = BUILD("posting", "siOOOOb", FILE_LINE_ARGS, (yyvsp[-1].pyobj), Py_None, Py_None, Py_False, (yyvsp[-2].character));
            DECREF1((yyvsp[-1].pyobj));
        }
#line 1651 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 26:
#line 255 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
              (yyval.pyobj) = BUILD("key_value", "OO", (yyvsp[-2].pyobj), (yyvsp[-1].pyobj));
              DECREF2((yyvsp[-2].pyobj), (yyvsp[-1].pyobj));
          }
#line 1660 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 34:
#line 268 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
                    (yyval.pyobj) = (yyvsp[0].pyobj);
                }
#line 1668 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 35:
#line 272 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
                    Py_INCREF(Py_None);
                    (yyval.pyobj) = Py_None;
                }
#line 1677 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 36:
#line 278 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
                       Py_INCREF(Py_None);
                       (yyval.pyobj) = Py_None;
                   }
#line 1686 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 37:
#line 283 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
                       (yyval.pyobj) = BUILD("handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                       DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                   }
#line 1695 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 38:
#line 288 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
                       (yyval.pyobj) = BUILD("handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                       DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                   }
#line 1704 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 39:
#line 294 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
                   Py_INCREF(Py_None);
                   (yyval.pyobj) = Py_None;
               }
#line 1713 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 40:
#line 299 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
                   (yyval.pyobj) = BUILD("handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                   DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj));
               }
#line 1722 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 41:
#line 305 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
                  Py_INCREF(Py_None);
                  (yyval.pyobj) = Py_None;
              }
#line 1731 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 42:
#line 310 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
                  (yyval.pyobj) = BUILD("handle_list", "OO", Py_None, (yyvsp[0].pyobj));
                  DECREF1((yyvsp[0].pyobj));
              }
#line 1740 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 43:
#line 315 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
                  (yyval.pyobj) = BUILD("handle_list", "OO", (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
                  DECREF2((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
              }
#line 1749 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 44:
#line 321 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
             BUILD("pushtag", "O", (yyvsp[-1].pyobj));
             DECREF1((yyvsp[-1].pyobj));
         }
#line 1758 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 45:
#line 327 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
           BUILD("poptag", "O", (yyvsp[-1].pyobj));
           DECREF1((yyvsp[-1].pyobj));
       }
#line 1767 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 46:
#line 333 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
         (yyval.pyobj) = BUILD("open", "siOOOOO", FILE_LINE_ARGS, (yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
         DECREF5((yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
     }
#line 1776 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 47:
#line 339 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
                (yyval.pyobj) = (yyvsp[0].pyobj);
            }
#line 1784 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 48:
#line 343 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
                Py_INCREF(Py_None);
                (yyval.pyobj) = Py_None;
            }
#line 1793 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 49:
#line 349 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
          (yyval.pyobj) = BUILD("close", "siOOO", FILE_LINE_ARGS, (yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
          DECREF3((yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      }
#line 1802 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 50:
#line 355 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
              (yyval.pyobj) = BUILD("commodity", "siOOO", FILE_LINE_ARGS, (yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
              DECREF3((yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
          }
#line 1811 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 51:
#line 361 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
        (yyval.pyobj) = BUILD("pad", "siOOOO", FILE_LINE_ARGS, (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
        DECREF4((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
    }
#line 1820 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 52:
#line 367 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
            (yyval.pyobj) = BUILD("balance", "siOOOOO", FILE_LINE_ARGS, (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pairobj).pyobj1, (yyvsp[-2].pairobj).pyobj2, (yyvsp[0].pyobj));
            DECREF3((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[0].pyobj));
            DECREF2((yyvsp[-2].pairobj).pyobj1, (yyvsp[-2].pairobj).pyobj2);
        }
#line 1830 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 53:
#line 374 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
         (yyval.pyobj) = BUILD("amount", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
         DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj));
       }
#line 1839 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 54:
#line 380 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
                     (yyval.pairobj).pyobj1 = BUILD("amount", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                     (yyval.pairobj).pyobj2 = Py_None;
                     Py_INCREF(Py_None);
                     DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                 }
#line 1850 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 55:
#line 387 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
                     (yyval.pairobj).pyobj1 = BUILD("amount", "OO", (yyvsp[-3].pyobj), (yyvsp[0].pyobj));
                     (yyval.pairobj).pyobj2 = (yyvsp[-1].pyobj);
                     DECREF3((yyvsp[-3].pyobj), (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                 }
#line 1860 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 56:
#line 394 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
                 Py_INCREF(Py_None);
                 (yyval.pyobj) = Py_None;
             }
#line 1869 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 57:
#line 399 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
                 (yyval.pyobj) = (yyvsp[0].pyobj);
             }
#line 1877 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 58:
#line 404 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
                    (yyval.pyobj) = BUILD("compound_amount", "OOO", (yyvsp[-1].pyobj), Py_None, (yyvsp[0].pyobj));
                    DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                }
#line 1886 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 59:
#line 409 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
                    (yyval.pyobj) = BUILD("compound_amount", "OOO", (yyvsp[-3].pyobj), (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                    DECREF3((yyvsp[-3].pyobj), (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                }
#line 1895 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 60:
#line 415 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
             (yyval.pyobj) = BUILD("position", "siOO", FILE_LINE_ARGS, (yyvsp[0].pyobj), Py_None);
             DECREF1((yyvsp[0].pyobj));
         }
#line 1904 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 61:
#line 420 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
             (yyval.pyobj) = BUILD("position", "siOO", FILE_LINE_ARGS, (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
             DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj));
         }
#line 1913 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 62:
#line 426 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
             (yyval.pyobj) = BUILD("lot_spec", "O", (yyvsp[-1].pyobj));
             DECREF1((yyvsp[-1].pyobj));
         }
#line 1922 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 63:
#line 432 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
                  Py_INCREF(Py_None);
                  (yyval.pyobj) = Py_None;
              }
#line 1931 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 64:
#line 437 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
                  (yyval.pyobj) = BUILD("handle_list", "OO", Py_None, (yyvsp[0].pyobj));
                  DECREF1((yyvsp[0].pyobj));
              }
#line 1940 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 65:
#line 442 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
                  (yyval.pyobj) = BUILD("handle_list", "OO", (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
                  DECREF2((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
              }
#line 1949 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 66:
#line 448 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
             (yyval.pyobj) = (yyvsp[0].pyobj);
         }
#line 1957 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 67:
#line 452 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
             (yyval.pyobj) = (yyvsp[0].pyobj);
         }
#line 1965 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 68:
#line 456 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
             (yyval.pyobj) = (yyvsp[0].pyobj);
         }
#line 1973 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 69:
#line 462 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
          (yyval.pyobj) = BUILD("price", "siOOOO", FILE_LINE_ARGS, (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
          DECREF4((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      }
#line 1982 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 70:
#line 468 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
          (yyval.pyobj) = BUILD("event", "siOOOO", FILE_LINE_ARGS, (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
          DECREF4((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      }
#line 1991 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 71:
#line 474 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
          (yyval.pyobj) = BUILD("note", "siOOOO", FILE_LINE_ARGS, (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
          DECREF4((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      }
#line 2000 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 73:
#line 482 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
             (yyval.pyobj) = BUILD("document", "siOOOO", FILE_LINE_ARGS, (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
             DECREF4((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
         }
#line 2009 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 83:
#line 497 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
          (yyval.pyobj) = (yyvsp[0].pyobj);
      }
#line 2017 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 84:
#line 502 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
          BUILD("option", "siOO", FILE_LINE_ARGS, (yyvsp[-2].pyobj), (yyvsp[-1].pyobj));
          DECREF2((yyvsp[-2].pyobj), (yyvsp[-1].pyobj));
       }
#line 2026 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 85:
#line 508 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
          BUILD("include", "siO", FILE_LINE_ARGS, (yyvsp[-1].pyobj));
          DECREF1((yyvsp[-1].pyobj));
       }
#line 2035 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 86:
#line 514 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
          BUILD("plugin", "siOO", FILE_LINE_ARGS, (yyvsp[-1].pyobj), Py_None);
          DECREF1((yyvsp[-1].pyobj));
       }
#line 2044 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 87:
#line 519 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
          BUILD("plugin", "siOO", FILE_LINE_ARGS, (yyvsp[-2].pyobj), (yyvsp[-1].pyobj));
          DECREF2((yyvsp[-2].pyobj), (yyvsp[-1].pyobj));
       }
#line 2053 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 95:
#line 534 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
                 (yyval.pyobj) = (yyvsp[-1].pyobj);
             }
#line 2061 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 96:
#line 538 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
                 (yyval.pyobj) = BUILD("handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                 DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj));
             }
#line 2070 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 97:
#line 543 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
                 (yyval.pyobj) = (yyvsp[-1].pyobj);
             }
#line 2078 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 98:
#line 547 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
                  Py_INCREF(Py_None);
                  (yyval.pyobj) = Py_None;
             }
#line 2087 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;

  case 99:
#line 553 "src/python/beancount/parser/grammar.y" /* yacc.c:1646  */
    {
         BUILD("store_result", "O", (yyvsp[0].pyobj));
     }
#line 2095 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
    break;


#line 2099 "src/python/beancount/parser/grammar.c" /* yacc.c:1646  */
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
#line 560 "src/python/beancount/parser/grammar.y" /* yacc.c:1906  */


/* A function that will convert a token name to a string, used in debugging. */
const char* getTokenName(int token)
{
    switch ( token ) {
        case ERROR    : return "ERROR";
        case INDENT   : return "INDENT";
        case EOL      : return "EOL";
        case COMMENT  : return "COMMENT";
        case SKIPPED  : return "SKIPPED";
        case PIPE     : return "PIPE";
        case ATAT     : return "ATAT";
        case AT       : return "AT";
        case LCURL    : return "LCURL";
        case RCURL    : return "RCURL";
        case EQUAL    : return "EQUAL";
        case COMMA    : return "COMMA";
        case SLASH    : return "SLASH";
        case PLUS     : return "PLUS";
        case FLAG     : return "FLAG";
        case TXN      : return "TXN";
        case BALANCE  : return "BALANCE";
        case OPEN     : return "OPEN";
        case CLOSE    : return "CLOSE";
        case PAD      : return "PAD";
        case EVENT    : return "EVENT";
        case PRICE    : return "PRICE";
        case NOTE     : return "NOTE";
        case DOCUMENT : return "DOCUMENT";
        case PUSHTAG  : return "PUSHTAG";
        case POPTAG   : return "POPTAG";
        case OPTION   : return "OPTION";
        case DATE     : return "DATE";
        case ACCOUNT  : return "ACCOUNT";
        case CURRENCY : return "CURRENCY";
        case STRING   : return "STRING";
        case NUMBER   : return "NUMBER";
        case TAG      : return "TAG";
        case LINK     : return "LINK";
        case KEY      : return "KEY";
    }
    return 0;
}

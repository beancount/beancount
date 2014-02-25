/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

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
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Using locations.  */
#define YYLSP_NEEDED 1



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
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
     SLASH = 272,
     FLAG = 273,
     TXN = 274,
     BALANCE = 275,
     OPEN = 276,
     CLOSE = 277,
     PAD = 278,
     EVENT = 279,
     PRICE = 280,
     NOTE = 281,
     DOCUMENT = 282,
     PUSHTAG = 283,
     POPTAG = 284,
     OPTION = 285,
     DATE = 286,
     ACCOUNT = 287,
     CURRENCY = 288,
     STRING = 289,
     NUMBER = 290,
     TAG = 291,
     LINK = 292
   };
#endif
/* Tokens.  */
#define ERROR 258
#define INDENT 259
#define EOL 260
#define COMMENT 261
#define SKIPPED 262
#define PIPE 263
#define ATAT 264
#define AT 265
#define LCURLCURL 266
#define RCURLCURL 267
#define LCURL 268
#define RCURL 269
#define EQUAL 270
#define COMMA 271
#define SLASH 272
#define FLAG 273
#define TXN 274
#define BALANCE 275
#define OPEN 276
#define CLOSE 277
#define PAD 278
#define EVENT 279
#define PRICE 280
#define NOTE 281
#define DOCUMENT 282
#define PUSHTAG 283
#define POPTAG 284
#define OPTION 285
#define DATE 286
#define ACCOUNT 287
#define CURRENCY 288
#define STRING 289
#define NUMBER 290
#define TAG 291
#define LINK 292




/* Copy the first part of user declarations.  */
#line 11 "src/python/beancount/parser/grammar.y"


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



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 62 "src/python/beancount/parser/grammar.y"
{
    char character;
    const char* string;
    PyObject* pyobj;
}
/* Line 193 of yacc.c.  */
#line 209 "src/python/beancount/parser/grammar.c"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
} YYLTYPE;
# define yyltype YYLTYPE /* obsolescent; will be withdrawn */
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif


/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 234 "src/python/beancount/parser/grammar.c"

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
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
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
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
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
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
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
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
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
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
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
  yytype_int16 yyss;
  YYSTYPE yyvs;
    YYLTYPE yyls;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE) + sizeof (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  28
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   118

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  38
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  31
/* YYNRULES -- Number of rules.  */
#define YYNRULES  66
/* YYNRULES -- Number of states.  */
#define YYNSTATES  127

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   292

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
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
      35,    36,    37
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint8 yyprhs[] =
{
       0,     0,     3,     4,     6,     8,    10,    13,    15,    18,
      21,    23,    25,    27,    30,    32,    35,    43,    53,    55,
      57,    63,    71,    79,    84,    86,    89,    91,    93,    97,
     101,   105,   111,   116,   122,   128,   131,   133,   136,   140,
     146,   150,   156,   162,   168,   174,   176,   182,   184,   186,
     188,   190,   192,   194,   196,   198,   200,   205,   207,   209,
     211,   213,   215,   218,   221,   224,   226
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      68,     0,    -1,    -1,    19,    -1,    18,    -1,     5,    -1,
       6,     5,    -1,     5,    -1,     6,     5,    -1,     4,     5,
      -1,     4,    -1,     6,    -1,    39,    -1,    43,    36,    -1,
      39,    -1,    44,    37,    -1,    31,    40,    34,    43,    44,
      41,    48,    -1,    31,    40,    34,     8,    34,    43,    44,
      41,    48,    -1,    39,    -1,    18,    -1,     4,    46,    32,
      57,    41,    -1,     4,    46,    32,    57,    10,    56,    41,
      -1,     4,    46,    32,    57,     9,    56,    41,    -1,     4,
      46,    32,    41,    -1,    39,    -1,    48,    47,    -1,    39,
      -1,    33,    -1,    49,    16,    33,    -1,    28,    36,    41,
      -1,    29,    36,    41,    -1,    31,    21,    32,    49,    41,
      -1,    31,    22,    32,    41,    -1,    31,    23,    32,    32,
      41,    -1,    31,    20,    32,    56,    41,    -1,    35,    33,
      -1,    56,    -1,    56,    58,    -1,    13,    56,    14,    -1,
      13,    56,    17,    31,    14,    -1,    11,    56,    12,    -1,
      11,    56,    17,    31,    12,    -1,    31,    25,    33,    56,
      41,    -1,    31,    24,    34,    34,    41,    -1,    31,    26,
      32,    34,    41,    -1,    34,    -1,    31,    27,    32,    62,
      41,    -1,    45,    -1,    55,    -1,    52,    -1,    53,    -1,
      54,    -1,    60,    -1,    61,    -1,    63,    -1,    59,    -1,
      30,    34,    34,    41,    -1,     7,    -1,    42,    -1,    50,
      -1,    51,    -1,    65,    -1,    67,    66,    -1,    67,    64,
      -1,    67,     1,    -1,    39,    -1,    67,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   141,   141,   144,   148,   153,   154,   157,   158,   159,
     160,   161,   163,   168,   174,   179,   185,   190,   196,   200,
     202,   207,   212,   217,   223,   228,   234,   239,   244,   250,
     256,   262,   268,   274,   280,   286,   293,   298,   304,   309,
     314,   319,   326,   332,   338,   344,   346,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   365,   371,   372,   373,
     374,   375,   378,   382,   387,   391,   397
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "ERROR", "INDENT", "EOL", "COMMENT",
  "SKIPPED", "PIPE", "ATAT", "AT", "LCURLCURL", "RCURLCURL", "LCURL",
  "RCURL", "EQUAL", "COMMA", "SLASH", "FLAG", "TXN", "BALANCE", "OPEN",
  "CLOSE", "PAD", "EVENT", "PRICE", "NOTE", "DOCUMENT", "PUSHTAG",
  "POPTAG", "OPTION", "DATE", "ACCOUNT", "CURRENCY", "STRING", "NUMBER",
  "TAG", "LINK", "$accept", "empty", "txn", "eol", "empty_line",
  "tags_list", "links_list", "transaction", "optflag", "posting",
  "posting_list", "currency_list", "pushtag", "poptag", "open", "close",
  "pad", "balance", "amount", "position", "lot_cost_date", "price",
  "event", "note", "filename", "document", "entry", "option", "directive",
  "declarations", "file", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    38,    39,    40,    40,    41,    41,    42,    42,    42,
      42,    42,    43,    43,    44,    44,    45,    45,    46,    46,
      47,    47,    47,    47,    48,    48,    49,    49,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    57,    58,    58,
      58,    58,    59,    60,    61,    62,    63,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    65,    66,    66,    66,
      66,    66,    67,    67,    67,    67,    68
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     1,     1,     1,     2,     1,     2,     2,
       1,     1,     1,     2,     1,     2,     7,     9,     1,     1,
       5,     7,     7,     4,     1,     2,     1,     1,     3,     3,
       3,     5,     4,     5,     5,     2,     1,     2,     3,     5,
       3,     5,     5,     5,     5,     1,     5,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     4,     1,     1,     1,
       1,     1,     2,     2,     2,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,    65,     0,     0,    64,    10,     7,    11,    57,     0,
       0,     0,     0,    58,    47,    59,    60,    49,    50,    51,
      48,    55,    52,    53,    54,    63,    61,    62,     1,     9,
       8,     0,     0,     0,     4,     3,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     5,     0,    29,    30,     0,
       0,     2,     0,     0,     0,     0,     0,     0,     2,     6,
      56,     0,     0,    27,    26,     0,    32,     0,     0,     0,
       0,    45,     0,     0,    12,     2,    35,    34,     0,    31,
      33,    43,    42,    44,    46,     2,    13,    14,     0,    28,
       2,    15,     2,     0,    24,    16,     2,     2,    25,    17,
      19,    18,     0,     0,    23,    36,     0,     0,     0,    37,
       0,     0,    20,     0,     0,     0,     0,    40,     0,    38,
       0,    22,    21,     0,     0,    41,    39
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,    74,    44,    47,    13,    75,    88,    14,   102,    98,
      95,    65,    15,    16,    17,    18,    19,    20,    62,   106,
     109,    21,    22,    23,    72,    24,    25,    26,    27,     2,
       3
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -55
static const yytype_int8 yypact[] =
{
     -55,   -55,    77,    15,   -55,    18,   -55,    22,   -55,   -16,
      -8,    -5,    91,   -55,   -55,   -55,   -55,   -55,   -55,   -55,
     -55,   -55,   -55,   -55,   -55,   -55,   -55,   -55,   -55,   -55,
     -55,    20,    20,    -4,   -55,   -55,     2,    11,    12,    13,
      -3,    14,    16,    23,    25,   -55,    28,   -55,   -55,    20,
      17,    27,    20,    29,    30,    17,    31,    32,    38,   -55,
     -55,    34,    20,   -55,   -55,     1,   -55,    20,    20,    20,
      20,   -55,    20,    35,   -55,    26,   -55,   -55,    37,   -55,
     -55,   -55,   -55,   -55,   -55,   -55,   -55,   -55,    -1,   -55,
      26,   -55,   -55,    -1,   -55,    46,   -55,    50,   -55,    46,
     -55,   -55,    39,     6,   -55,     5,     4,    17,    17,   -55,
      17,    17,   -55,    -9,     7,    20,    20,   -55,    41,   -55,
      43,   -55,   -55,    67,    66,   -55,   -55
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -55,     0,   -55,   -30,   -55,     3,     8,   -55,   -55,   -55,
      -7,   -55,   -55,   -55,   -55,   -55,   -55,   -55,   -54,   -55,
     -55,   -55,   -55,   -55,   -55,   -55,   -55,   -55,   -55,   -55,
     -55
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -67
static const yytype_int8 yytable[] =
{
       1,    69,    48,   117,    45,    46,    45,    46,   118,    45,
      46,    45,    46,   110,   111,    28,   107,    78,   108,    60,
      31,   119,    66,    29,   120,    45,    46,    30,    32,    33,
      49,    54,    77,    59,    50,    79,    91,    80,    81,    82,
      83,    61,    84,    51,    52,    53,    73,    55,    56,   105,
      97,    64,    61,   113,   114,    57,   115,   116,    92,    58,
      63,    67,    86,    96,    68,    70,    71,    76,   100,    85,
      89,   103,   123,   104,   124,    87,   112,   -66,     4,   125,
     126,     5,     6,     7,     8,   121,   122,     0,    90,    99,
      87,     0,    94,     0,     0,     0,    94,   101,    93,     0,
       0,     0,     0,     0,     0,     9,    10,    11,    12,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43
};

static const yytype_int8 yycheck[] =
{
       0,    55,    32,    12,     5,     6,     5,     6,    17,     5,
       6,     5,     6,     9,    10,     0,    11,    16,    13,    49,
      36,    14,    52,     5,    17,     5,     6,     5,    36,    34,
      34,    34,    62,     5,    32,    65,    37,    67,    68,    69,
      70,    35,    72,    32,    32,    32,     8,    33,    32,   103,
       4,    51,    35,   107,   108,    32,   110,   111,    88,    34,
      33,    32,    36,    93,    34,    34,    34,    33,    18,    34,
      33,    32,    31,   103,    31,    75,   106,     0,     1,    12,
      14,     4,     5,     6,     7,   115,   116,    -1,    85,    96,
      90,    -1,    92,    -1,    -1,    -1,    96,    97,    90,    -1,
      -1,    -1,    -1,    -1,    -1,    28,    29,    30,    31,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    39,    67,    68,     1,     4,     5,     6,     7,    28,
      29,    30,    31,    42,    45,    50,    51,    52,    53,    54,
      55,    59,    60,    61,    63,    64,    65,    66,     0,     5,
       5,    36,    36,    34,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    40,     5,     6,    41,    41,    34,
      32,    32,    32,    32,    34,    33,    32,    32,    34,     5,
      41,    35,    56,    33,    39,    49,    41,    32,    34,    56,
      34,    34,    62,     8,    39,    43,    33,    41,    16,    41,
      41,    41,    41,    41,    41,    34,    36,    39,    44,    33,
      43,    37,    41,    44,    39,    48,    41,     4,    47,    48,
      18,    39,    46,    32,    41,    56,    57,    11,    13,    58,
       9,    10,    41,    56,    56,    56,    56,    12,    17,    14,
      17,    41,    41,    31,    31,    12,    14
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (&yylval, &yylloc, YYLEX_PARAM)
#else
# define YYLEX yylex (&yylval, &yylloc)
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value, Location); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep, yylocationp)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
    YYLTYPE const * const yylocationp;
#endif
{
  if (!yyvaluep)
    return;
  YYUSE (yylocationp);
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep, yylocationp)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
    YYLTYPE const * const yylocationp;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  YY_LOCATION_PRINT (yyoutput, *yylocationp);
  YYFPRINTF (yyoutput, ": ");
  yy_symbol_value_print (yyoutput, yytype, yyvaluep, yylocationp);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, YYLTYPE *yylsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yylsp, yyrule)
    YYSTYPE *yyvsp;
    YYLTYPE *yylsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       , &(yylsp[(yyi + 1) - (yynrhs)])		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, yylsp, Rule); \
} while (YYID (0))

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
#ifndef	YYINITDEPTH
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
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
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
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
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

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, YYLTYPE *yylocationp)
#else
static void
yydestruct (yymsg, yytype, yyvaluep, yylocationp)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
    YYLTYPE *yylocationp;
#endif
{
  YYUSE (yyvaluep);
  YYUSE (yylocationp);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */






/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  /* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;
/* Location data for the look-ahead symbol.  */
YYLTYPE yylloc;

  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;

  /* The location stack.  */
  YYLTYPE yylsa[YYINITDEPTH];
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;
  /* The locations where the error started and ended.  */
  YYLTYPE yyerror_range[2];

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;
  yylsp = yyls;
#if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  /* Initialize the default location before parsing starts.  */
  yylloc.first_line   = yylloc.last_line   = 1;
  yylloc.first_column = yylloc.last_column = 0;
#endif

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
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);
	YYSTACK_RELOCATE (yyls);
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

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
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
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;
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
     `$$ = $1'.

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
#line 145 "src/python/beancount/parser/grammar.y"
    {
        (yyval.character) = '*';
    ;}
    break;

  case 4:
#line 149 "src/python/beancount/parser/grammar.y"
    {
        (yyval.character) = (yyvsp[(1) - (1)].character);
    ;}
    break;

  case 12:
#line 164 "src/python/beancount/parser/grammar.y"
    {
              Py_INCREF(Py_None);
              (yyval.pyobj) = Py_None;
          ;}
    break;

  case 13:
#line 169 "src/python/beancount/parser/grammar.y"
    {
              (yyval.pyobj) = BUILD("handle_list", "OO", (yyvsp[(1) - (2)].pyobj), (yyvsp[(2) - (2)].pyobj));
              DECREF2((yyvsp[(1) - (2)].pyobj), (yyvsp[(2) - (2)].pyobj));
          ;}
    break;

  case 14:
#line 175 "src/python/beancount/parser/grammar.y"
    {
              Py_INCREF(Py_None);
              (yyval.pyobj) = Py_None;
          ;}
    break;

  case 15:
#line 180 "src/python/beancount/parser/grammar.y"
    {
              (yyval.pyobj) = BUILD("handle_list", "OO", (yyvsp[(1) - (2)].pyobj), (yyvsp[(2) - (2)].pyobj));
              DECREF2((yyvsp[(1) - (2)].pyobj), (yyvsp[(2) - (2)].pyobj));
          ;}
    break;

  case 16:
#line 186 "src/python/beancount/parser/grammar.y"
    {
                (yyval.pyobj) = BUILD("transaction", "siObOOOOO", FILE_LINE_ARGS, (yyvsp[(1) - (7)].pyobj), (yyvsp[(2) - (7)].character), Py_None, (yyvsp[(3) - (7)].pyobj), (yyvsp[(4) - (7)].pyobj), (yyvsp[(5) - (7)].pyobj), (yyvsp[(7) - (7)].pyobj));
                DECREF5((yyvsp[(1) - (7)].pyobj), (yyvsp[(3) - (7)].pyobj), (yyvsp[(4) - (7)].pyobj), (yyvsp[(5) - (7)].pyobj), (yyvsp[(7) - (7)].pyobj));
            ;}
    break;

  case 17:
#line 191 "src/python/beancount/parser/grammar.y"
    {
                (yyval.pyobj) = BUILD("transaction", "siObOOOOO", FILE_LINE_ARGS, (yyvsp[(1) - (9)].pyobj), (yyvsp[(2) - (9)].character), (yyvsp[(3) - (9)].pyobj), (yyvsp[(5) - (9)].pyobj), (yyvsp[(6) - (9)].pyobj), (yyvsp[(7) - (9)].pyobj), (yyvsp[(9) - (9)].pyobj));
                DECREF6((yyvsp[(1) - (9)].pyobj), (yyvsp[(3) - (9)].pyobj), (yyvsp[(5) - (9)].pyobj), (yyvsp[(6) - (9)].pyobj), (yyvsp[(7) - (9)].pyobj), (yyvsp[(9) - (9)].pyobj));
            ;}
    break;

  case 18:
#line 197 "src/python/beancount/parser/grammar.y"
    {
            (yyval.character) = '\0';
        ;}
    break;

  case 20:
#line 203 "src/python/beancount/parser/grammar.y"
    {
            (yyval.pyobj) = BUILD("posting", "OOOOb", (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj), Py_None, Py_False, (yyvsp[(2) - (5)].character));
            DECREF2((yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
        ;}
    break;

  case 21:
#line 208 "src/python/beancount/parser/grammar.y"
    {
            (yyval.pyobj) = BUILD("posting", "OOOOb", (yyvsp[(3) - (7)].pyobj), (yyvsp[(4) - (7)].pyobj), (yyvsp[(6) - (7)].pyobj), Py_False, (yyvsp[(2) - (7)].character));
            DECREF3((yyvsp[(3) - (7)].pyobj), (yyvsp[(4) - (7)].pyobj), (yyvsp[(6) - (7)].pyobj));
        ;}
    break;

  case 22:
#line 213 "src/python/beancount/parser/grammar.y"
    {
            (yyval.pyobj) = BUILD("posting", "OOOOb", (yyvsp[(3) - (7)].pyobj), (yyvsp[(4) - (7)].pyobj), (yyvsp[(6) - (7)].pyobj), Py_True, (yyvsp[(2) - (7)].character));
            DECREF3((yyvsp[(3) - (7)].pyobj), (yyvsp[(4) - (7)].pyobj), (yyvsp[(6) - (7)].pyobj));
        ;}
    break;

  case 23:
#line 218 "src/python/beancount/parser/grammar.y"
    {
            (yyval.pyobj) = BUILD("posting", "OOOOb", (yyvsp[(3) - (4)].pyobj), Py_None, Py_None, Py_False, (yyvsp[(2) - (4)].character));
            DECREF1((yyvsp[(3) - (4)].pyobj));
        ;}
    break;

  case 24:
#line 224 "src/python/beancount/parser/grammar.y"
    {
                 Py_INCREF(Py_None);
                 (yyval.pyobj) = Py_None;
             ;}
    break;

  case 25:
#line 229 "src/python/beancount/parser/grammar.y"
    {
                 (yyval.pyobj) = BUILD("handle_list", "OO", (yyvsp[(1) - (2)].pyobj), (yyvsp[(2) - (2)].pyobj));
                 DECREF2((yyvsp[(1) - (2)].pyobj), (yyvsp[(2) - (2)].pyobj));
             ;}
    break;

  case 26:
#line 235 "src/python/beancount/parser/grammar.y"
    {
                  Py_INCREF(Py_None);
                  (yyval.pyobj) = Py_None;
              ;}
    break;

  case 27:
#line 240 "src/python/beancount/parser/grammar.y"
    {
                  (yyval.pyobj) = BUILD("handle_list", "OO", Py_None, (yyvsp[(1) - (1)].pyobj));
                  DECREF1((yyvsp[(1) - (1)].pyobj));
              ;}
    break;

  case 28:
#line 245 "src/python/beancount/parser/grammar.y"
    {
                  (yyval.pyobj) = BUILD("handle_list", "OO", (yyvsp[(1) - (3)].pyobj), (yyvsp[(3) - (3)].pyobj));
                  DECREF2((yyvsp[(1) - (3)].pyobj), (yyvsp[(3) - (3)].pyobj));
              ;}
    break;

  case 29:
#line 251 "src/python/beancount/parser/grammar.y"
    {
             BUILD("pushtag", "O", (yyvsp[(2) - (3)].pyobj));
             DECREF1((yyvsp[(2) - (3)].pyobj));
         ;}
    break;

  case 30:
#line 257 "src/python/beancount/parser/grammar.y"
    {
           BUILD("poptag", "O", (yyvsp[(2) - (3)].pyobj));
           DECREF1((yyvsp[(2) - (3)].pyobj));
       ;}
    break;

  case 31:
#line 263 "src/python/beancount/parser/grammar.y"
    {
         (yyval.pyobj) = BUILD("open", "siOOO", FILE_LINE_ARGS, (yyvsp[(1) - (5)].pyobj), (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
         DECREF3((yyvsp[(1) - (5)].pyobj), (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
     ;}
    break;

  case 32:
#line 269 "src/python/beancount/parser/grammar.y"
    {
          (yyval.pyobj) = BUILD("close", "siOO", FILE_LINE_ARGS, (yyvsp[(1) - (4)].pyobj), (yyvsp[(3) - (4)].pyobj));
          DECREF2((yyvsp[(1) - (4)].pyobj), (yyvsp[(3) - (4)].pyobj));
      ;}
    break;

  case 33:
#line 275 "src/python/beancount/parser/grammar.y"
    {
        (yyval.pyobj) = BUILD("pad", "siOOO", FILE_LINE_ARGS, (yyvsp[(1) - (5)].pyobj), (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
        DECREF3((yyvsp[(1) - (5)].pyobj), (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
    ;}
    break;

  case 34:
#line 281 "src/python/beancount/parser/grammar.y"
    {
          (yyval.pyobj) = BUILD("balance", "siOOO", FILE_LINE_ARGS, (yyvsp[(1) - (5)].pyobj), (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
          DECREF3((yyvsp[(1) - (5)].pyobj), (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
      ;}
    break;

  case 35:
#line 287 "src/python/beancount/parser/grammar.y"
    {
         PyObject* o = BUILD("amount", "OO", (yyvsp[(1) - (2)].pyobj), (yyvsp[(2) - (2)].pyobj));
         (yyval.pyobj) = o;
         DECREF2((yyvsp[(1) - (2)].pyobj), (yyvsp[(2) - (2)].pyobj));
       ;}
    break;

  case 36:
#line 294 "src/python/beancount/parser/grammar.y"
    {
               (yyval.pyobj) = BUILD("position", "OO", (yyvsp[(1) - (1)].pyobj), Py_None);
               DECREF1((yyvsp[(1) - (1)].pyobj));
           ;}
    break;

  case 37:
#line 299 "src/python/beancount/parser/grammar.y"
    {
               (yyval.pyobj) = BUILD("position", "OO", (yyvsp[(1) - (2)].pyobj), (yyvsp[(2) - (2)].pyobj));
               DECREF2((yyvsp[(1) - (2)].pyobj), (yyvsp[(2) - (2)].pyobj));
           ;}
    break;

  case 38:
#line 305 "src/python/beancount/parser/grammar.y"
    {
             (yyval.pyobj) = BUILD("lot_cost_date", "OOO", (yyvsp[(2) - (3)].pyobj), Py_None, Py_False);
             DECREF1((yyvsp[(2) - (3)].pyobj));
         ;}
    break;

  case 39:
#line 310 "src/python/beancount/parser/grammar.y"
    {
             (yyval.pyobj) = BUILD("lot_cost_date", "OOO", (yyvsp[(2) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj), Py_False);
             DECREF2((yyvsp[(2) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
         ;}
    break;

  case 40:
#line 315 "src/python/beancount/parser/grammar.y"
    {
             (yyval.pyobj) = BUILD("lot_cost_date", "OOO", (yyvsp[(2) - (3)].pyobj), Py_None, Py_True);
             DECREF1((yyvsp[(2) - (3)].pyobj));
         ;}
    break;

  case 41:
#line 320 "src/python/beancount/parser/grammar.y"
    {
             (yyval.pyobj) = BUILD("lot_cost_date", "OOO", (yyvsp[(2) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj), Py_True);
             DECREF2((yyvsp[(2) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
         ;}
    break;

  case 42:
#line 327 "src/python/beancount/parser/grammar.y"
    {
          (yyval.pyobj) = BUILD("price", "siOOO", FILE_LINE_ARGS, (yyvsp[(1) - (5)].pyobj), (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
          DECREF3((yyvsp[(1) - (5)].pyobj), (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
      ;}
    break;

  case 43:
#line 333 "src/python/beancount/parser/grammar.y"
    {
          (yyval.pyobj) = BUILD("event", "siOOO", FILE_LINE_ARGS, (yyvsp[(1) - (5)].pyobj), (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
          DECREF3((yyvsp[(1) - (5)].pyobj), (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
      ;}
    break;

  case 44:
#line 339 "src/python/beancount/parser/grammar.y"
    {
          (yyval.pyobj) = BUILD("note", "siOOO", FILE_LINE_ARGS, (yyvsp[(1) - (5)].pyobj), (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
          DECREF3((yyvsp[(1) - (5)].pyobj), (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
      ;}
    break;

  case 46:
#line 347 "src/python/beancount/parser/grammar.y"
    {
          (yyval.pyobj) = BUILD("document", "siOOO", FILE_LINE_ARGS, (yyvsp[(1) - (5)].pyobj), (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
          DECREF3((yyvsp[(1) - (5)].pyobj), (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
      ;}
    break;

  case 55:
#line 361 "src/python/beancount/parser/grammar.y"
    {
          (yyval.pyobj) = (yyvsp[(1) - (1)].pyobj);
      ;}
    break;

  case 56:
#line 366 "src/python/beancount/parser/grammar.y"
    {
          BUILD("option", "siOO", FILE_LINE_ARGS, (yyvsp[(2) - (4)].pyobj), (yyvsp[(3) - (4)].pyobj));
          DECREF2((yyvsp[(2) - (4)].pyobj), (yyvsp[(3) - (4)].pyobj));
       ;}
    break;

  case 62:
#line 379 "src/python/beancount/parser/grammar.y"
    {
                 (yyval.pyobj) = (yyvsp[(1) - (2)].pyobj);
             ;}
    break;

  case 63:
#line 383 "src/python/beancount/parser/grammar.y"
    {
                 (yyval.pyobj) = BUILD("handle_list", "OO", (yyvsp[(1) - (2)].pyobj), (yyvsp[(2) - (2)].pyobj));
                 DECREF2((yyvsp[(1) - (2)].pyobj), (yyvsp[(2) - (2)].pyobj));
             ;}
    break;

  case 64:
#line 388 "src/python/beancount/parser/grammar.y"
    {
                 (yyval.pyobj) = (yyvsp[(1) - (2)].pyobj);
             ;}
    break;

  case 65:
#line 392 "src/python/beancount/parser/grammar.y"
    {
                  Py_INCREF(Py_None);
                  (yyval.pyobj) = Py_None;
             ;}
    break;

  case 66:
#line 398 "src/python/beancount/parser/grammar.y"
    {
         BUILD("store_result", "O", (yyvsp[(1) - (1)].pyobj));
     ;}
    break;


/* Line 1267 of yacc.c.  */
#line 1893 "src/python/beancount/parser/grammar.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }

  yyerror_range[0] = yylloc;

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
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

  /* Else will try to reuse look-ahead token after shifting the error
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

  yyerror_range[0] = yylsp[1-yylen];
  /* Do not reclaim the symbols of the rule which action triggered
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
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
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

      yyerror_range[0] = *yylsp;
      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp, yylsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;

  yyerror_range[1] = yylloc;
  /* Using YYLLOC is tempting, but would change the location of
     the look-ahead.  YYLOC is available though.  */
  YYLLOC_DEFAULT (yyloc, (yyerror_range - 1), 2);
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

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval, &yylloc);
  /* Do not reclaim the symbols of the rule which action triggered
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
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 405 "src/python/beancount/parser/grammar.y"


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
    }
    return 0;
}


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
     LCURL = 266,
     RCURL = 267,
     EQUAL = 268,
     COMMA = 269,
     SLASH = 270,
     FLAG = 271,
     TXN = 272,
     CHECK = 273,
     OPEN = 274,
     CLOSE = 275,
     PAD = 276,
     EVENT = 277,
     PRICE = 278,
     NOTE = 279,
     DOCUMENT = 280,
     PUSHTAG = 281,
     POPTAG = 282,
     OPTION = 283,
     DATE = 284,
     ACCOUNT = 285,
     CURRENCY = 286,
     STRING = 287,
     NUMBER = 288,
     TAG = 289,
     LINK = 290
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
#define LCURL 266
#define RCURL 267
#define EQUAL 268
#define COMMA 269
#define SLASH 270
#define FLAG 271
#define TXN 272
#define CHECK 273
#define OPEN 274
#define CLOSE 275
#define PAD 276
#define EVENT 277
#define PRICE 278
#define NOTE 279
#define DOCUMENT 280
#define PUSHTAG 281
#define POPTAG 282
#define OPTION 283
#define DATE 284
#define ACCOUNT 285
#define CURRENCY 286
#define STRING 287
#define NUMBER 288
#define TAG 289
#define LINK 290




/* Copy the first part of user declarations.  */
#line 11 "src/python/beancount/parser/grammar.y"


#include <stdio.h>
#include <assert.h>
#include "parser.h"
#include "lexer.h"

/* FIXME: set real filename here. */

/* Error-handling function. */
void yyerror(char const* message)
{
    fprintf(stderr, "%s:%d:: %s\n", yy_filename, yylineno, message);
}

void report_error(char const* message, YYLTYPE* yylloc)
{
    fprintf(stderr, "%s:%d:: %s\n", yy_filename, yylloc->first_line, message);
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


#define FILE_LINE_ARGS  yy_filename, (yyloc).first_line




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
#line 67 "src/python/beancount/parser/grammar.y"
{
    char character;
    const char* string;
    PyObject* pyobj;
}
/* Line 193 of yacc.c.  */
#line 210 "src/python/beancount/parser/grammar.c"
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
#line 235 "src/python/beancount/parser/grammar.c"

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
#define YYFINAL  27
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   106

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  36
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  31
/* YYNRULES -- Number of rules.  */
#define YYNRULES  63
/* YYNRULES -- Number of states.  */
#define YYNSTATES  120

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   290

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
      35
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
     146,   152,   158,   164,   166,   172,   174,   176,   178,   180,
     182,   184,   186,   188,   190,   195,   197,   199,   201,   203,
     205,   208,   211,   213
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      66,     0,    -1,    -1,    17,    -1,    16,    -1,     5,    -1,
       6,     5,    -1,     5,    -1,     6,     5,    -1,     4,     5,
      -1,     4,    -1,     6,    -1,    37,    -1,    41,    34,    -1,
      37,    -1,    42,    35,    -1,    29,    38,    32,    41,    42,
      39,    46,    -1,    29,    38,    32,     8,    32,    41,    42,
      39,    46,    -1,    37,    -1,    16,    -1,     4,    44,    30,
      55,    39,    -1,     4,    44,    30,    55,    10,    54,    39,
      -1,     4,    44,    30,    55,     9,    54,    39,    -1,     4,
      44,    30,    39,    -1,    37,    -1,    46,    45,    -1,    37,
      -1,    31,    -1,    47,    14,    31,    -1,    26,    34,    39,
      -1,    27,    34,    39,    -1,    29,    19,    30,    47,    39,
      -1,    29,    20,    30,    39,    -1,    29,    21,    30,    30,
      39,    -1,    29,    18,    30,    54,    39,    -1,    33,    31,
      -1,    54,    -1,    54,    56,    -1,    11,    54,    12,    -1,
      11,    54,    15,    29,    12,    -1,    29,    23,    31,    54,
      39,    -1,    29,    22,    32,    32,    39,    -1,    29,    24,
      30,    32,    39,    -1,    32,    -1,    29,    25,    30,    60,
      39,    -1,    43,    -1,    53,    -1,    50,    -1,    51,    -1,
      52,    -1,    58,    -1,    59,    -1,    61,    -1,    57,    -1,
      28,    32,    32,    39,    -1,     7,    -1,    40,    -1,    48,
      -1,    49,    -1,    63,    -1,    65,    64,    -1,    65,    62,
      -1,    37,    -1,    65,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   144,   144,   147,   151,   156,   157,   160,   161,   162,
     163,   164,   166,   171,   177,   182,   188,   193,   199,   203,
     205,   210,   215,   220,   226,   231,   237,   242,   247,   253,
     259,   265,   271,   277,   283,   289,   296,   301,   307,   312,
     319,   325,   331,   337,   339,   345,   346,   347,   348,   349,
     350,   351,   352,   353,   358,   364,   365,   366,   367,   368,
     370,   374,   379,   385
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "ERROR", "INDENT", "EOL", "COMMENT",
  "SKIPPED", "PIPE", "ATAT", "AT", "LCURL", "RCURL", "EQUAL", "COMMA",
  "SLASH", "FLAG", "TXN", "CHECK", "OPEN", "CLOSE", "PAD", "EVENT",
  "PRICE", "NOTE", "DOCUMENT", "PUSHTAG", "POPTAG", "OPTION", "DATE",
  "ACCOUNT", "CURRENCY", "STRING", "NUMBER", "TAG", "LINK", "$accept",
  "empty", "txn", "eol", "empty_line", "tags_list", "links_list",
  "transaction", "optflag", "posting", "posting_list", "currency_list",
  "pushtag", "poptag", "open", "close", "pad", "check", "amount",
  "position", "lot_cost_date", "price", "event", "note", "filename",
  "document", "entry", "option", "directive", "declarations", "file", 0
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
     285,   286,   287,   288,   289,   290
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    36,    37,    38,    38,    39,    39,    40,    40,    40,
      40,    40,    41,    41,    42,    42,    43,    43,    44,    44,
      45,    45,    45,    45,    46,    46,    47,    47,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    55,    56,    56,
      57,    58,    59,    60,    61,    62,    62,    62,    62,    62,
      62,    62,    62,    62,    63,    64,    64,    64,    64,    64,
      65,    65,    65,    66
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     1,     1,     1,     2,     1,     2,     2,
       1,     1,     1,     2,     1,     2,     7,     9,     1,     1,
       5,     7,     7,     4,     1,     2,     1,     1,     3,     3,
       3,     5,     4,     5,     5,     2,     1,     2,     3,     5,
       5,     5,     5,     1,     5,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     4,     1,     1,     1,     1,     1,
       2,     2,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,    62,    63,     0,    10,     7,    11,    55,     0,     0,
       0,     0,    56,    45,    57,    58,    47,    48,    49,    46,
      53,    50,    51,    52,    61,    59,    60,     1,     9,     8,
       0,     0,     0,     4,     3,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     5,     0,    29,    30,     0,     0,
       2,     0,     0,     0,     0,     0,     0,     2,     6,    54,
       0,     0,    27,    26,     0,    32,     0,     0,     0,     0,
      43,     0,     0,    12,     2,    35,    34,     0,    31,    33,
      41,    40,    42,    44,     2,    13,    14,     0,    28,     2,
      15,     2,     0,    24,    16,     2,     2,    25,    17,    19,
      18,     0,     0,    23,    36,     0,     0,    37,     0,     0,
      20,     0,     0,     0,    38,     0,    22,    21,     0,    39
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,    73,    43,    46,    12,    74,    87,    13,   101,    97,
      94,    64,    14,    15,    16,    17,    18,    19,    61,   105,
     107,    20,    21,    22,    71,    23,    24,    25,    26,     2,
       3
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -51
static const yytype_int8 yypact[] =
{
     -51,   -51,    18,    20,    21,   -51,    22,   -51,   -29,    -5,
      -4,    81,   -51,   -51,   -51,   -51,   -51,   -51,   -51,   -51,
     -51,   -51,   -51,   -51,   -51,   -51,   -51,   -51,   -51,   -51,
       9,     9,    -2,   -51,   -51,     3,    10,    12,    13,    16,
      23,    19,    25,    28,   -51,    46,   -51,   -51,     9,    30,
      33,     9,    31,    34,    30,    35,    36,    45,   -51,   -51,
      38,     9,   -51,   -51,     5,   -51,     9,     9,     9,     9,
     -51,     9,    39,   -51,    42,   -51,   -51,    47,   -51,   -51,
     -51,   -51,   -51,   -51,   -51,   -51,   -51,    -3,   -51,    42,
     -51,   -51,    -3,   -51,    61,   -51,    54,   -51,    61,   -51,
     -51,    43,     2,   -51,    66,     7,    30,   -51,    30,    30,
     -51,    -6,     9,     9,   -51,    50,   -51,   -51,    68,   -51
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -51,     0,   -51,   -30,   -51,     1,    -8,   -51,   -51,   -51,
     -11,   -51,   -51,   -51,   -51,   -51,   -51,   -51,   -50,   -51,
     -51,   -51,   -51,   -51,   -51,   -51,   -51,   -51,   -51,   -51,
     -51
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint8 yytable[] =
{
       1,    47,    44,    45,    68,    30,   114,    44,    45,   115,
      44,    45,    44,    45,    44,    45,   108,   109,    59,    77,
      27,    65,     4,     5,     6,     7,    28,    29,    32,    31,
      48,    76,    90,    49,    78,    60,    79,    80,    81,    82,
      50,    83,    51,    52,     8,     9,    10,    11,    53,    55,
      63,    58,   104,    72,    54,    56,   111,    91,   112,   113,
      57,    66,    95,    60,    62,    96,    67,    69,    70,    75,
      99,    84,   103,   102,    86,   110,    85,   106,    88,   118,
     119,    92,   116,   117,    98,    89,     0,     0,     0,    86,
       0,    93,     0,     0,     0,    93,   100,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42
};

static const yytype_int8 yycheck[] =
{
       0,    31,     5,     6,    54,    34,    12,     5,     6,    15,
       5,     6,     5,     6,     5,     6,     9,    10,    48,    14,
       0,    51,     4,     5,     6,     7,     5,     5,    32,    34,
      32,    61,    35,    30,    64,    33,    66,    67,    68,    69,
      30,    71,    30,    30,    26,    27,    28,    29,    32,    30,
      50,     5,   102,     8,    31,    30,   106,    87,   108,   109,
      32,    30,    92,    33,    31,     4,    32,    32,    32,    31,
      16,    32,   102,    30,    74,   105,    34,    11,    31,    29,
      12,    89,   112,   113,    95,    84,    -1,    -1,    -1,    89,
      -1,    91,    -1,    -1,    -1,    95,    96,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    37,    65,    66,     4,     5,     6,     7,    26,    27,
      28,    29,    40,    43,    48,    49,    50,    51,    52,    53,
      57,    58,    59,    61,    62,    63,    64,     0,     5,     5,
      34,    34,    32,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    38,     5,     6,    39,    39,    32,    30,
      30,    30,    30,    32,    31,    30,    30,    32,     5,    39,
      33,    54,    31,    37,    47,    39,    30,    32,    54,    32,
      32,    60,     8,    37,    41,    31,    39,    14,    39,    39,
      39,    39,    39,    39,    32,    34,    37,    42,    31,    41,
      35,    39,    42,    37,    46,    39,     4,    45,    46,    16,
      37,    44,    30,    39,    54,    55,    11,    56,     9,    10,
      39,    54,    54,    54,    12,    15,    39,    39,    29,    12
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
#line 148 "src/python/beancount/parser/grammar.y"
    {
        (yyval.character) = '*';
    ;}
    break;

  case 4:
#line 152 "src/python/beancount/parser/grammar.y"
    {
        (yyval.character) = (yyvsp[(1) - (1)].character);
    ;}
    break;

  case 12:
#line 167 "src/python/beancount/parser/grammar.y"
    {
              Py_INCREF(Py_None);
              (yyval.pyobj) = Py_None;
          ;}
    break;

  case 13:
#line 172 "src/python/beancount/parser/grammar.y"
    {
              (yyval.pyobj) = BUILD("handle_list", "OO", (yyvsp[(1) - (2)].pyobj), (yyvsp[(2) - (2)].pyobj));
              DECREF2((yyvsp[(1) - (2)].pyobj), (yyvsp[(2) - (2)].pyobj));
          ;}
    break;

  case 14:
#line 178 "src/python/beancount/parser/grammar.y"
    {
              Py_INCREF(Py_None);
              (yyval.pyobj) = Py_None;
          ;}
    break;

  case 15:
#line 183 "src/python/beancount/parser/grammar.y"
    {
              (yyval.pyobj) = BUILD("handle_list", "OO", (yyvsp[(1) - (2)].pyobj), (yyvsp[(2) - (2)].pyobj));
              DECREF2((yyvsp[(1) - (2)].pyobj), (yyvsp[(2) - (2)].pyobj));
          ;}
    break;

  case 16:
#line 189 "src/python/beancount/parser/grammar.y"
    {
                (yyval.pyobj) = BUILD("transaction", "siObOOOOO", FILE_LINE_ARGS, (yyvsp[(1) - (7)].pyobj), (yyvsp[(2) - (7)].character), Py_None, (yyvsp[(3) - (7)].pyobj), (yyvsp[(4) - (7)].pyobj), (yyvsp[(5) - (7)].pyobj), (yyvsp[(7) - (7)].pyobj));
                DECREF5((yyvsp[(1) - (7)].pyobj), (yyvsp[(3) - (7)].pyobj), (yyvsp[(4) - (7)].pyobj), (yyvsp[(5) - (7)].pyobj), (yyvsp[(7) - (7)].pyobj));
            ;}
    break;

  case 17:
#line 194 "src/python/beancount/parser/grammar.y"
    {
                (yyval.pyobj) = BUILD("transaction", "siObOOOOO", FILE_LINE_ARGS, (yyvsp[(1) - (9)].pyobj), (yyvsp[(2) - (9)].character), (yyvsp[(3) - (9)].pyobj), (yyvsp[(5) - (9)].pyobj), (yyvsp[(6) - (9)].pyobj), (yyvsp[(7) - (9)].pyobj), (yyvsp[(9) - (9)].pyobj));
                DECREF6((yyvsp[(1) - (9)].pyobj), (yyvsp[(3) - (9)].pyobj), (yyvsp[(5) - (9)].pyobj), (yyvsp[(6) - (9)].pyobj), (yyvsp[(7) - (9)].pyobj), (yyvsp[(9) - (9)].pyobj));
            ;}
    break;

  case 18:
#line 200 "src/python/beancount/parser/grammar.y"
    {
            (yyval.character) = '\0';
        ;}
    break;

  case 20:
#line 206 "src/python/beancount/parser/grammar.y"
    {
            (yyval.pyobj) = BUILD("posting", "OOOOb", (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj), Py_None, Py_False, (yyvsp[(2) - (5)].character));
            DECREF2((yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
        ;}
    break;

  case 21:
#line 211 "src/python/beancount/parser/grammar.y"
    {
            (yyval.pyobj) = BUILD("posting", "OOOOb", (yyvsp[(3) - (7)].pyobj), (yyvsp[(4) - (7)].pyobj), (yyvsp[(6) - (7)].pyobj), Py_False, (yyvsp[(2) - (7)].character));
            DECREF3((yyvsp[(3) - (7)].pyobj), (yyvsp[(4) - (7)].pyobj), (yyvsp[(6) - (7)].pyobj));
        ;}
    break;

  case 22:
#line 216 "src/python/beancount/parser/grammar.y"
    {
            (yyval.pyobj) = BUILD("posting", "OOOOb", (yyvsp[(3) - (7)].pyobj), (yyvsp[(4) - (7)].pyobj), (yyvsp[(6) - (7)].pyobj), Py_True, (yyvsp[(2) - (7)].character));
            DECREF3((yyvsp[(3) - (7)].pyobj), (yyvsp[(4) - (7)].pyobj), (yyvsp[(6) - (7)].pyobj));
        ;}
    break;

  case 23:
#line 221 "src/python/beancount/parser/grammar.y"
    {
            (yyval.pyobj) = BUILD("posting", "OOOOb", (yyvsp[(3) - (4)].pyobj), Py_None, Py_None, Py_False, (yyvsp[(2) - (4)].character));
            DECREF1((yyvsp[(3) - (4)].pyobj));
        ;}
    break;

  case 24:
#line 227 "src/python/beancount/parser/grammar.y"
    {
                 Py_INCREF(Py_None);
                 (yyval.pyobj) = Py_None;
             ;}
    break;

  case 25:
#line 232 "src/python/beancount/parser/grammar.y"
    {
                 (yyval.pyobj) = BUILD("handle_list", "OO", (yyvsp[(1) - (2)].pyobj), (yyvsp[(2) - (2)].pyobj));
                 DECREF2((yyvsp[(1) - (2)].pyobj), (yyvsp[(2) - (2)].pyobj));
             ;}
    break;

  case 26:
#line 238 "src/python/beancount/parser/grammar.y"
    {
                  Py_INCREF(Py_None);
                  (yyval.pyobj) = Py_None;
              ;}
    break;

  case 27:
#line 243 "src/python/beancount/parser/grammar.y"
    {
                  (yyval.pyobj) = BUILD("handle_list", "OO", Py_None, (yyvsp[(1) - (1)].pyobj));
                  DECREF1((yyvsp[(1) - (1)].pyobj));
              ;}
    break;

  case 28:
#line 248 "src/python/beancount/parser/grammar.y"
    {
                  (yyval.pyobj) = BUILD("handle_list", "OO", (yyvsp[(1) - (3)].pyobj), (yyvsp[(3) - (3)].pyobj));
                  DECREF2((yyvsp[(1) - (3)].pyobj), (yyvsp[(3) - (3)].pyobj));
              ;}
    break;

  case 29:
#line 254 "src/python/beancount/parser/grammar.y"
    {
             BUILD("pushtag", "O", (yyvsp[(2) - (3)].pyobj));
             DECREF1((yyvsp[(2) - (3)].pyobj));
         ;}
    break;

  case 30:
#line 260 "src/python/beancount/parser/grammar.y"
    {
           BUILD("poptag", "O", (yyvsp[(2) - (3)].pyobj));
           DECREF1((yyvsp[(2) - (3)].pyobj));
       ;}
    break;

  case 31:
#line 266 "src/python/beancount/parser/grammar.y"
    {
         (yyval.pyobj) = BUILD("open", "siOOO", FILE_LINE_ARGS, (yyvsp[(1) - (5)].pyobj), (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
         DECREF3((yyvsp[(1) - (5)].pyobj), (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
     ;}
    break;

  case 32:
#line 272 "src/python/beancount/parser/grammar.y"
    {
          (yyval.pyobj) = BUILD("close", "siOO", FILE_LINE_ARGS, (yyvsp[(1) - (4)].pyobj), (yyvsp[(3) - (4)].pyobj));
          DECREF2((yyvsp[(1) - (4)].pyobj), (yyvsp[(3) - (4)].pyobj));
      ;}
    break;

  case 33:
#line 278 "src/python/beancount/parser/grammar.y"
    {
        (yyval.pyobj) = BUILD("pad", "siOOO", FILE_LINE_ARGS, (yyvsp[(1) - (5)].pyobj), (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
        DECREF3((yyvsp[(1) - (5)].pyobj), (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
    ;}
    break;

  case 34:
#line 284 "src/python/beancount/parser/grammar.y"
    {
          (yyval.pyobj) = BUILD("check", "siOOO", FILE_LINE_ARGS, (yyvsp[(1) - (5)].pyobj), (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
          DECREF3((yyvsp[(1) - (5)].pyobj), (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
      ;}
    break;

  case 35:
#line 290 "src/python/beancount/parser/grammar.y"
    {
         PyObject* o = BUILD("amount", "OO", (yyvsp[(1) - (2)].pyobj), (yyvsp[(2) - (2)].pyobj));
         (yyval.pyobj) = o;
         DECREF2((yyvsp[(1) - (2)].pyobj), (yyvsp[(2) - (2)].pyobj));
       ;}
    break;

  case 36:
#line 297 "src/python/beancount/parser/grammar.y"
    {
               (yyval.pyobj) = BUILD("position", "OO", (yyvsp[(1) - (1)].pyobj), Py_None);
               DECREF1((yyvsp[(1) - (1)].pyobj));
           ;}
    break;

  case 37:
#line 302 "src/python/beancount/parser/grammar.y"
    {
               (yyval.pyobj) = BUILD("position", "OO", (yyvsp[(1) - (2)].pyobj), (yyvsp[(2) - (2)].pyobj));
               DECREF2((yyvsp[(1) - (2)].pyobj), (yyvsp[(2) - (2)].pyobj));
           ;}
    break;

  case 38:
#line 308 "src/python/beancount/parser/grammar.y"
    {
             (yyval.pyobj) = BUILD("lot_cost_date", "OO", (yyvsp[(2) - (3)].pyobj), Py_None);
             DECREF1((yyvsp[(2) - (3)].pyobj));
         ;}
    break;

  case 39:
#line 313 "src/python/beancount/parser/grammar.y"
    {
             (yyval.pyobj) = BUILD("lot_cost_date", "OO", (yyvsp[(2) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
             DECREF2((yyvsp[(2) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
         ;}
    break;

  case 40:
#line 320 "src/python/beancount/parser/grammar.y"
    {
          (yyval.pyobj) = BUILD("price", "siOOO", FILE_LINE_ARGS, (yyvsp[(1) - (5)].pyobj), (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
          DECREF3((yyvsp[(1) - (5)].pyobj), (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
      ;}
    break;

  case 41:
#line 326 "src/python/beancount/parser/grammar.y"
    {
          (yyval.pyobj) = BUILD("event", "siOOO", FILE_LINE_ARGS, (yyvsp[(1) - (5)].pyobj), (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
          DECREF3((yyvsp[(1) - (5)].pyobj), (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
      ;}
    break;

  case 42:
#line 332 "src/python/beancount/parser/grammar.y"
    {
          (yyval.pyobj) = BUILD("note", "siOOO", FILE_LINE_ARGS, (yyvsp[(1) - (5)].pyobj), (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
          DECREF3((yyvsp[(1) - (5)].pyobj), (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
      ;}
    break;

  case 44:
#line 340 "src/python/beancount/parser/grammar.y"
    {
          (yyval.pyobj) = BUILD("document", "siOOO", FILE_LINE_ARGS, (yyvsp[(1) - (5)].pyobj), (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
          DECREF3((yyvsp[(1) - (5)].pyobj), (yyvsp[(3) - (5)].pyobj), (yyvsp[(4) - (5)].pyobj));
      ;}
    break;

  case 53:
#line 354 "src/python/beancount/parser/grammar.y"
    {
          (yyval.pyobj) = (yyvsp[(1) - (1)].pyobj);
      ;}
    break;

  case 54:
#line 359 "src/python/beancount/parser/grammar.y"
    {
          BUILD("option", "siOO", FILE_LINE_ARGS, (yyvsp[(2) - (4)].pyobj), (yyvsp[(3) - (4)].pyobj));
          DECREF2((yyvsp[(2) - (4)].pyobj), (yyvsp[(3) - (4)].pyobj));
       ;}
    break;

  case 60:
#line 371 "src/python/beancount/parser/grammar.y"
    {
                 (yyval.pyobj) = (yyvsp[(1) - (2)].pyobj);
             ;}
    break;

  case 61:
#line 375 "src/python/beancount/parser/grammar.y"
    {
                 (yyval.pyobj) = BUILD("handle_list", "OO", (yyvsp[(1) - (2)].pyobj), (yyvsp[(2) - (2)].pyobj));
                 DECREF2((yyvsp[(1) - (2)].pyobj), (yyvsp[(2) - (2)].pyobj));
             ;}
    break;

  case 62:
#line 380 "src/python/beancount/parser/grammar.y"
    {
                  Py_INCREF(Py_None);
                  (yyval.pyobj) = Py_None;
             ;}
    break;

  case 63:
#line 386 "src/python/beancount/parser/grammar.y"
    {
         BUILD("store_result", "O", (yyvsp[(1) - (1)].pyobj));
     ;}
    break;


/* Line 1267 of yacc.c.  */
#line 1864 "src/python/beancount/parser/grammar.c"
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


#line 393 "src/python/beancount/parser/grammar.y"


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
        case CHECK    : return "CHECK";
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


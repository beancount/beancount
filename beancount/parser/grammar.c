/* A Bison parser, made by GNU Bison 3.6.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.6.4"

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
#define BUILDY(clean, target, method_name, format, ...)                         \
    target = PyObject_CallMethod(builder, method_name, "Oi" format,             \
                                 FILENAME, LINENO, ## __VA_ARGS__);             \
    clean;                                                                      \
    if (target == NULL) {                                                       \
        build_grammar_error_from_exception(&yyloc, builder);                    \
        YYERROR;                                                                \
    }

#define FILENAME (yyloc).file_name
#define LINENO (yyloc).first_line

/* Build a grammar error from the exception context. */
void build_grammar_error_from_exception(YYLTYPE* loc, PyObject* builder)
{
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
        PyObject* rv = PyObject_CallMethod(builder, "build_grammar_error", "OiOOO",
                                           loc->file_name, loc->first_line,
                                           pvalue, ptype, ptraceback);
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

    Py_XDECREF(ptype);
    Py_XDECREF(pvalue);
    Py_XDECREF(ptraceback);
}

/* Error-handling function. {ca6aab8b9748} */
void yyerror(YYLTYPE* loc, yyscan_t scanner, PyObject* builder, char const* message)
{
    /* Register a syntax error with the builder. */
    PyObject* rv = PyObject_CallMethod(builder, "build_grammar_error", "Ois",
                                       loc->file_name, loc->first_line,
                                       message);
    if (rv == NULL) {
        PyErr_SetString(PyExc_RuntimeError,
                        "Internal error: Building exception from yyerror()");
    }
    Py_XDECREF(rv);
}

#define DECREF(...) _CC_FUNC(Py_DECREF, __VA_ARGS__)


#line 148 "beancount/parser/grammar.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
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


#line 223 "beancount/parser/grammar.c"

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    INDENT = 258,                  /* INDENT  */
    EOL = 259,                     /* EOL  */
    COMMENT = 260,                 /* COMMENT  */
    SKIPPED = 261,                 /* SKIPPED  */
    PIPE = 262,                    /* PIPE  */
    ATAT = 263,                    /* ATAT  */
    AT = 264,                      /* AT  */
    LCURLCURL = 265,               /* LCURLCURL  */
    RCURLCURL = 266,               /* RCURLCURL  */
    LCURL = 267,                   /* LCURL  */
    RCURL = 268,                   /* RCURL  */
    COMMA = 269,                   /* COMMA  */
    TILDE = 270,                   /* TILDE  */
    HASH = 271,                    /* HASH  */
    ASTERISK = 272,                /* ASTERISK  */
    SLASH = 273,                   /* SLASH  */
    COLON = 274,                   /* COLON  */
    PLUS = 275,                    /* PLUS  */
    MINUS = 276,                   /* MINUS  */
    LPAREN = 277,                  /* LPAREN  */
    RPAREN = 278,                  /* RPAREN  */
    FLAG = 279,                    /* FLAG  */
    TXN = 280,                     /* TXN  */
    BALANCE = 281,                 /* BALANCE  */
    OPEN = 282,                    /* OPEN  */
    CLOSE = 283,                   /* CLOSE  */
    COMMODITY = 284,               /* COMMODITY  */
    PAD = 285,                     /* PAD  */
    EVENT = 286,                   /* EVENT  */
    PRICE = 287,                   /* PRICE  */
    NOTE = 288,                    /* NOTE  */
    DOCUMENT = 289,                /* DOCUMENT  */
    QUERY = 290,                   /* QUERY  */
    CUSTOM = 291,                  /* CUSTOM  */
    PUSHTAG = 292,                 /* PUSHTAG  */
    POPTAG = 293,                  /* POPTAG  */
    PUSHMETA = 294,                /* PUSHMETA  */
    POPMETA = 295,                 /* POPMETA  */
    OPTION = 296,                  /* OPTION  */
    INCLUDE = 297,                 /* INCLUDE  */
    PLUGIN = 298,                  /* PLUGIN  */
    NONE = 299,                    /* NONE  */
    BOOL = 300,                    /* BOOL  */
    DATE = 301,                    /* DATE  */
    ACCOUNT = 302,                 /* ACCOUNT  */
    CURRENCY = 303,                /* CURRENCY  */
    STRING = 304,                  /* STRING  */
    NUMBER = 305,                  /* NUMBER  */
    TAG = 306,                     /* TAG  */
    LINK = 307,                    /* LINK  */
    KEY = 308,                     /* KEY  */
    NEGATIVE = 309                 /* NEGATIVE  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 144 "beancount/parser/grammar.y"

    char character;
    const char* string;
    PyObject* pyobj;
    struct {
        PyObject* pyobj1;
        PyObject* pyobj2;
    } pairobj;

#line 304 "beancount/parser/grammar.c"

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
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_INDENT = 3,                     /* INDENT  */
  YYSYMBOL_EOL = 4,                        /* EOL  */
  YYSYMBOL_COMMENT = 5,                    /* COMMENT  */
  YYSYMBOL_SKIPPED = 6,                    /* SKIPPED  */
  YYSYMBOL_PIPE = 7,                       /* PIPE  */
  YYSYMBOL_ATAT = 8,                       /* ATAT  */
  YYSYMBOL_AT = 9,                         /* AT  */
  YYSYMBOL_LCURLCURL = 10,                 /* LCURLCURL  */
  YYSYMBOL_RCURLCURL = 11,                 /* RCURLCURL  */
  YYSYMBOL_LCURL = 12,                     /* LCURL  */
  YYSYMBOL_RCURL = 13,                     /* RCURL  */
  YYSYMBOL_COMMA = 14,                     /* COMMA  */
  YYSYMBOL_TILDE = 15,                     /* TILDE  */
  YYSYMBOL_HASH = 16,                      /* HASH  */
  YYSYMBOL_ASTERISK = 17,                  /* ASTERISK  */
  YYSYMBOL_SLASH = 18,                     /* SLASH  */
  YYSYMBOL_COLON = 19,                     /* COLON  */
  YYSYMBOL_PLUS = 20,                      /* PLUS  */
  YYSYMBOL_MINUS = 21,                     /* MINUS  */
  YYSYMBOL_LPAREN = 22,                    /* LPAREN  */
  YYSYMBOL_RPAREN = 23,                    /* RPAREN  */
  YYSYMBOL_FLAG = 24,                      /* FLAG  */
  YYSYMBOL_TXN = 25,                       /* TXN  */
  YYSYMBOL_BALANCE = 26,                   /* BALANCE  */
  YYSYMBOL_OPEN = 27,                      /* OPEN  */
  YYSYMBOL_CLOSE = 28,                     /* CLOSE  */
  YYSYMBOL_COMMODITY = 29,                 /* COMMODITY  */
  YYSYMBOL_PAD = 30,                       /* PAD  */
  YYSYMBOL_EVENT = 31,                     /* EVENT  */
  YYSYMBOL_PRICE = 32,                     /* PRICE  */
  YYSYMBOL_NOTE = 33,                      /* NOTE  */
  YYSYMBOL_DOCUMENT = 34,                  /* DOCUMENT  */
  YYSYMBOL_QUERY = 35,                     /* QUERY  */
  YYSYMBOL_CUSTOM = 36,                    /* CUSTOM  */
  YYSYMBOL_PUSHTAG = 37,                   /* PUSHTAG  */
  YYSYMBOL_POPTAG = 38,                    /* POPTAG  */
  YYSYMBOL_PUSHMETA = 39,                  /* PUSHMETA  */
  YYSYMBOL_POPMETA = 40,                   /* POPMETA  */
  YYSYMBOL_OPTION = 41,                    /* OPTION  */
  YYSYMBOL_INCLUDE = 42,                   /* INCLUDE  */
  YYSYMBOL_PLUGIN = 43,                    /* PLUGIN  */
  YYSYMBOL_NONE = 44,                      /* NONE  */
  YYSYMBOL_BOOL = 45,                      /* BOOL  */
  YYSYMBOL_DATE = 46,                      /* DATE  */
  YYSYMBOL_ACCOUNT = 47,                   /* ACCOUNT  */
  YYSYMBOL_CURRENCY = 48,                  /* CURRENCY  */
  YYSYMBOL_STRING = 49,                    /* STRING  */
  YYSYMBOL_NUMBER = 50,                    /* NUMBER  */
  YYSYMBOL_TAG = 51,                       /* TAG  */
  YYSYMBOL_LINK = 52,                      /* LINK  */
  YYSYMBOL_KEY = 53,                       /* KEY  */
  YYSYMBOL_NEGATIVE = 54,                  /* NEGATIVE  */
  YYSYMBOL_YYACCEPT = 55,                  /* $accept  */
  YYSYMBOL_txn = 56,                       /* txn  */
  YYSYMBOL_eol = 57,                       /* eol  */
  YYSYMBOL_empty_line = 58,                /* empty_line  */
  YYSYMBOL_number_expr = 59,               /* number_expr  */
  YYSYMBOL_txn_strings = 60,               /* txn_strings  */
  YYSYMBOL_tags_links = 61,                /* tags_links  */
  YYSYMBOL_transaction = 62,               /* transaction  */
  YYSYMBOL_optflag = 63,                   /* optflag  */
  YYSYMBOL_price_annotation = 64,          /* price_annotation  */
  YYSYMBOL_account = 65,                   /* account  */
  YYSYMBOL_posting = 66,                   /* posting  */
  YYSYMBOL_key_value = 67,                 /* key_value  */
  YYSYMBOL_key_value_line = 68,            /* key_value_line  */
  YYSYMBOL_key_value_value = 69,           /* key_value_value  */
  YYSYMBOL_posting_or_kv_list = 70,        /* posting_or_kv_list  */
  YYSYMBOL_key_value_list = 71,            /* key_value_list  */
  YYSYMBOL_currency_list = 72,             /* currency_list  */
  YYSYMBOL_pushtag = 73,                   /* pushtag  */
  YYSYMBOL_poptag = 74,                    /* poptag  */
  YYSYMBOL_pushmeta = 75,                  /* pushmeta  */
  YYSYMBOL_popmeta = 76,                   /* popmeta  */
  YYSYMBOL_open = 77,                      /* open  */
  YYSYMBOL_opt_booking = 78,               /* opt_booking  */
  YYSYMBOL_close = 79,                     /* close  */
  YYSYMBOL_commodity = 80,                 /* commodity  */
  YYSYMBOL_pad = 81,                       /* pad  */
  YYSYMBOL_balance = 82,                   /* balance  */
  YYSYMBOL_amount = 83,                    /* amount  */
  YYSYMBOL_amount_tolerance = 84,          /* amount_tolerance  */
  YYSYMBOL_maybe_number = 85,              /* maybe_number  */
  YYSYMBOL_maybe_currency = 86,            /* maybe_currency  */
  YYSYMBOL_compound_amount = 87,           /* compound_amount  */
  YYSYMBOL_incomplete_amount = 88,         /* incomplete_amount  */
  YYSYMBOL_cost_spec = 89,                 /* cost_spec  */
  YYSYMBOL_cost_comp_list = 90,            /* cost_comp_list  */
  YYSYMBOL_cost_comp = 91,                 /* cost_comp  */
  YYSYMBOL_price = 92,                     /* price  */
  YYSYMBOL_event = 93,                     /* event  */
  YYSYMBOL_query = 94,                     /* query  */
  YYSYMBOL_note = 95,                      /* note  */
  YYSYMBOL_filename = 96,                  /* filename  */
  YYSYMBOL_document = 97,                  /* document  */
  YYSYMBOL_custom_value = 98,              /* custom_value  */
  YYSYMBOL_custom_value_list = 99,         /* custom_value_list  */
  YYSYMBOL_custom = 100,                   /* custom  */
  YYSYMBOL_entry = 101,                    /* entry  */
  YYSYMBOL_option = 102,                   /* option  */
  YYSYMBOL_include = 103,                  /* include  */
  YYSYMBOL_plugin = 104,                   /* plugin  */
  YYSYMBOL_directive = 105,                /* directive  */
  YYSYMBOL_declarations = 106,             /* declarations  */
  YYSYMBOL_file = 107                      /* file  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_uint8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

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


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                            \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
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

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if 1

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
#endif /* 1 */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
             && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE) \
             + YYSIZEOF (YYLTYPE)) \
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
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  38
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   271

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  55
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  53
/* YYNRULES -- Number of rules.  */
#define YYNRULES  134
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  227

#define YYMAXUTOK   309


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
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
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   278,   278,   282,   286,   290,   295,   296,   300,   301,
     302,   303,   309,   313,   318,   323,   328,   333,   338,   342,
     347,   352,   357,   364,   369,   374,   380,   386,   390,   394,
     398,   400,   405,   411,   416,   421,   426,   432,   438,   443,
     444,   445,   446,   447,   448,   449,   450,   451,   455,   461,
     466,   470,   475,   480,   486,   491,   497,   502,   507,   513,
     519,   525,   534,   540,   547,   551,   557,   563,   569,   575,
     581,   587,   595,   602,   607,   612,   617,   622,   627,   632,
     639,   645,   650,   655,   661,   666,   671,   677,   681,   685,
     689,   696,   702,   708,   714,   720,   722,   729,   734,   739,
     744,   749,   754,   764,   769,   775,   782,   783,   784,   785,
     786,   787,   788,   789,   790,   791,   792,   793,   798,   804,
     810,   815,   821,   822,   823,   824,   825,   826,   827,   828,
     831,   835,   840,   858,   865
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if 1
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "INDENT", "EOL",
  "COMMENT", "SKIPPED", "PIPE", "ATAT", "AT", "LCURLCURL", "RCURLCURL",
  "LCURL", "RCURL", "COMMA", "TILDE", "HASH", "ASTERISK", "SLASH", "COLON",
  "PLUS", "MINUS", "LPAREN", "RPAREN", "FLAG", "TXN", "BALANCE", "OPEN",
  "CLOSE", "COMMODITY", "PAD", "EVENT", "PRICE", "NOTE", "DOCUMENT",
  "QUERY", "CUSTOM", "PUSHTAG", "POPTAG", "PUSHMETA", "POPMETA", "OPTION",
  "INCLUDE", "PLUGIN", "NONE", "BOOL", "DATE", "ACCOUNT", "CURRENCY",
  "STRING", "NUMBER", "TAG", "LINK", "KEY", "NEGATIVE", "$accept", "txn",
  "eol", "empty_line", "number_expr", "txn_strings", "tags_links",
  "transaction", "optflag", "price_annotation", "account", "posting",
  "key_value", "key_value_line", "key_value_value", "posting_or_kv_list",
  "key_value_list", "currency_list", "pushtag", "poptag", "pushmeta",
  "popmeta", "open", "opt_booking", "close", "commodity", "pad", "balance",
  "amount", "amount_tolerance", "maybe_number", "maybe_currency",
  "compound_amount", "incomplete_amount", "cost_spec", "cost_comp_list",
  "cost_comp", "price", "event", "query", "note", "filename", "document",
  "custom_value", "custom_value_list", "custom", "entry", "option",
  "include", "plugin", "directive", "declarations", "file", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_int16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309
};
#endif

#define YYPACT_NINF (-111)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-135)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -111,   101,    23,  -111,  -111,  -111,  -111,  -111,   -24,   -15,
       8,    15,    -6,    28,    32,   230,  -111,  -111,  -111,  -111,
    -111,  -111,  -111,  -111,  -111,  -111,  -111,  -111,  -111,  -111,
    -111,  -111,  -111,  -111,  -111,  -111,  -111,  -111,  -111,     5,
       5,    51,     5,    81,    59,     5,    42,  -111,  -111,  -111,
    -111,    64,    64,    64,    69,    64,    63,    70,    64,    64,
      67,    71,  -111,  -111,   120,  -111,  -111,   185,  -111,     5,
       5,  -111,     5,  -111,  -111,    77,    78,     5,     5,    64,
      83,    77,    84,    87,    99,  -111,     1,  -111,    77,    77,
      77,  -111,  -111,  -111,  -111,  -111,  -111,  -111,   197,  -111,
    -111,  -111,  -111,  -111,  -111,   172,     5,  -111,   -10,  -111,
    -111,     5,     5,   197,     5,     5,  -111,  -111,     5,   109,
    -111,  -111,    37,  -111,  -111,   148,    77,    77,    77,    77,
    -111,    77,  -111,  -111,   102,  -111,     5,   149,   149,  -111,
    -111,  -111,  -111,    37,  -111,  -111,  -111,  -111,  -111,   197,
    -111,  -111,  -111,  -111,  -111,  -111,  -111,  -111,  -111,    17,
      17,   204,   149,  -111,  -111,     8,  -111,   149,   149,   149,
     149,  -111,   149,   149,   154,  -111,   149,     5,   149,    68,
    -111,  -111,  -111,   168,  -111,  -111,  -111,    -3,    64,  -111,
    -111,   141,  -111,   250,   128,    21,  -111,  -111,   153,    73,
     233,  -111,  -111,  -111,   180,    -4,  -111,     4,  -111,    44,
      77,    77,  -111,  -111,    77,  -111,  -111,   163,  -111,     5,
    -111,     5,   129,  -111,  -111,  -111,  -111
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
     133,     0,     0,   132,    10,     8,     9,    11,     0,     0,
       0,     0,     0,     0,     0,     0,   122,   106,   123,   124,
     125,   126,   108,   109,   115,   110,   107,   114,   113,   116,
     112,   111,   117,   131,   127,   128,   129,   130,     1,     0,
       0,     0,     0,     0,     0,     0,     0,     5,     4,     3,
       2,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    20,     6,     0,    59,    60,    48,    61,     0,
       0,   119,     0,   120,    32,     0,    56,     0,     0,     0,
       0,     0,     0,     0,     0,   103,    23,     7,     0,     0,
       0,    45,    44,    41,    42,    39,    12,    43,    46,    40,
      37,    47,    62,   118,   121,     0,     0,    57,    65,    54,
      54,     0,     0,     0,     0,     0,    95,    23,     0,     0,
      22,    21,     0,    18,    17,     0,     0,     0,     0,     0,
      70,     0,    71,    54,     0,    64,     0,    66,    67,    54,
      54,    54,    54,     0,    54,    99,    98,    97,    54,   101,
     102,   100,   104,    25,    24,    49,    19,    15,    16,    13,
      14,     0,    69,    58,    54,     0,    55,    68,    92,    91,
      94,    54,    93,   105,    26,    72,    63,     0,    96,    23,
      53,    52,    38,     0,    29,    28,    30,     0,     0,    50,
      51,    73,    36,    74,    75,    83,    76,    80,    73,    73,
       0,    90,    88,    89,    75,     0,    87,     0,    85,     0,
      73,    73,    33,    78,    73,    77,    82,    73,    81,     0,
      31,     0,     0,    86,    35,    34,    79
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -111,  -111,   -40,  -111,   -64,  -111,  -110,  -111,  -111,   -33,
     -39,  -111,   176,    14,  -111,  -111,   -88,  -111,  -111,  -111,
    -111,  -111,  -111,  -111,  -111,  -111,  -111,  -111,   -60,  -111,
     -89,   -22,  -111,     3,  -111,     9,   -13,  -111,  -111,  -111,
    -111,  -111,  -111,  -111,  -111,  -111,  -111,  -111,  -111,  -111,
    -111,  -111,  -111
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    62,    65,    16,   193,    86,   122,    17,   188,   219,
      75,   180,   177,   166,   100,   174,   137,   108,    18,    19,
      20,    21,    22,   136,    23,    24,    25,    26,   101,   106,
     194,   197,   206,   220,   200,   207,   208,    27,    28,    29,
      30,   117,    31,   152,   119,    32,    33,    34,    35,    36,
      37,     1,     2
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      66,   190,    68,    98,   134,    71,    73,   143,   120,    63,
      64,   105,   214,    76,    77,   216,    79,   113,   217,    82,
      83,   114,   138,    38,   123,   124,   125,    39,    99,   102,
     103,   198,   104,   199,   126,   127,    40,   109,   110,   135,
     111,    63,    64,    44,   215,   162,    63,    64,   153,   154,
     121,   167,   168,   169,   170,   149,   172,   218,   217,   151,
     173,    41,   157,   158,   159,   160,   133,   161,    43,   187,
      67,   139,   140,   183,   141,   142,   176,    45,   144,   148,
     150,    46,   155,   178,   184,   185,   -84,   -84,   153,   154,
     201,    72,   186,    88,    89,    90,   164,    88,    89,    90,
      69,  -134,     3,   171,     4,     5,     6,     7,    70,   205,
     205,    74,    80,    63,    64,   -27,    84,    78,    81,   202,
      85,    41,   203,    96,    87,   222,   107,    96,   205,    88,
      89,    90,   112,   115,   204,   204,   116,   182,     8,     9,
      10,    11,    12,    13,    14,    63,    64,    15,   118,   191,
     163,   192,   165,   204,   145,   146,    74,   179,   147,    96,
     212,    88,    89,    90,   -84,   126,   127,   -84,   128,   129,
     201,   156,   189,    88,    89,    90,   196,   226,   221,   224,
     201,   225,   213,    88,    89,    90,    42,   131,   181,   126,
     127,    96,   128,   129,   195,     0,   -74,   126,   127,   202,
     128,   129,   203,    96,   223,    88,    89,    90,   209,   202,
       0,     0,   203,    96,   126,   127,     0,   128,   129,     0,
     132,   126,   127,     0,   128,   129,     0,     0,   196,    91,
      92,    93,    74,    94,    95,    96,    97,    63,    64,     0,
       0,   210,   211,     0,     0,   130,    47,    48,     0,     0,
       0,     0,   175,     0,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,   126,   127,     0,
     128,   129
};

static const yytype_int16 yycheck[] =
{
      40,     4,    42,    67,    14,    45,    46,   117,     7,     4,
       5,    75,    16,    52,    53,    11,    55,    81,    14,    58,
      59,    81,   110,     0,    88,    89,    90,    51,    67,    69,
      70,    10,    72,    12,    17,    18,    51,    77,    78,    49,
      79,     4,     5,    49,    48,   133,     4,     5,    51,    52,
      49,   139,   140,   141,   142,   119,   144,    13,    14,   119,
     148,    53,   126,   127,   128,   129,   106,   131,    53,   179,
      19,   111,   112,     5,   114,   115,   164,    49,   118,   119,
     119,    49,   122,   171,    16,    17,    13,    14,    51,    52,
      17,    49,    24,    20,    21,    22,   136,    20,    21,    22,
      19,     0,     1,   143,     3,     4,     5,     6,    49,   198,
     199,    47,    49,     4,     5,    47,    49,    48,    48,    46,
      49,    53,    49,    50,     4,   214,    48,    50,   217,    20,
      21,    22,    49,    49,   198,   199,    49,   177,    37,    38,
      39,    40,    41,    42,    43,     4,     5,    46,    49,   188,
      48,   191,     3,   217,    45,    46,    47,     3,    49,    50,
     200,    20,    21,    22,    11,    17,    18,    14,    20,    21,
      17,    23,     4,    20,    21,    22,    48,    48,   211,   219,
      17,   221,   204,    20,    21,    22,    10,    15,   174,    17,
      18,    50,    20,    21,   191,    -1,    16,    17,    18,    46,
      20,    21,    49,    50,   217,    20,    21,    22,   199,    46,
      -1,    -1,    49,    50,    17,    18,    -1,    20,    21,    -1,
      48,    17,    18,    -1,    20,    21,    -1,    -1,    48,    44,
      45,    46,    47,    48,    49,    50,    51,     4,     5,    -1,
      -1,     8,     9,    -1,    -1,    48,    16,    17,    -1,    -1,
      -1,    -1,    48,    -1,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    17,    18,    -1,
      20,    21
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,   106,   107,     1,     3,     4,     5,     6,    37,    38,
      39,    40,    41,    42,    43,    46,    58,    62,    73,    74,
      75,    76,    77,    79,    80,    81,    82,    92,    93,    94,
      95,    97,   100,   101,   102,   103,   104,   105,     0,    51,
      51,    53,    67,    53,    49,    49,    49,    16,    17,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    56,     4,     5,    57,    57,    19,    57,    19,
      49,    57,    49,    57,    47,    65,    65,    65,    48,    65,
      49,    48,    65,    65,    49,    49,    60,     4,    20,    21,
      22,    44,    45,    46,    48,    49,    50,    51,    59,    65,
      69,    83,    57,    57,    57,    59,    84,    48,    72,    57,
      57,    65,    49,    59,    83,    49,    49,    96,    49,    99,
       7,    49,    61,    59,    59,    59,    17,    18,    20,    21,
      48,    15,    48,    57,    14,    49,    78,    71,    71,    57,
      57,    57,    57,    61,    57,    45,    46,    49,    57,    59,
      65,    83,    98,    51,    52,    57,    23,    59,    59,    59,
      59,    59,    71,    48,    57,     3,    68,    71,    71,    71,
      71,    57,    71,    71,    70,    48,    71,    67,    71,     3,
      66,    68,    57,     5,    16,    17,    24,    61,    63,     4,
       4,    65,    57,    59,    85,    88,    48,    86,    10,    12,
      89,    17,    46,    49,    59,    85,    87,    90,    91,    90,
       8,     9,    57,    86,    16,    48,    11,    14,    13,    64,
      88,    64,    85,    91,    57,    57,    48
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int8 yyr1[] =
{
       0,    55,    56,    56,    56,    56,    57,    57,    58,    58,
      58,    58,    59,    59,    59,    59,    59,    59,    59,    59,
      60,    60,    60,    61,    61,    61,    62,    63,    63,    63,
      63,    64,    65,    66,    66,    66,    66,    67,    68,    69,
      69,    69,    69,    69,    69,    69,    69,    69,    69,    70,
      70,    70,    70,    70,    71,    71,    72,    72,    72,    73,
      74,    75,    76,    77,    78,    78,    79,    80,    81,    82,
      83,    84,    84,    85,    85,    86,    86,    87,    87,    87,
      88,    89,    89,    89,    90,    90,    90,    91,    91,    91,
      91,    92,    93,    94,    95,    96,    97,    98,    98,    98,
      98,    98,    98,    99,    99,   100,   101,   101,   101,   101,
     101,   101,   101,   101,   101,   101,   101,   101,   102,   103,
     104,   104,   105,   105,   105,   105,   105,   105,   105,   105,
     106,   106,   106,   106,   107
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     1,     1,     1,     1,     2,     1,     1,
       1,     1,     1,     3,     3,     3,     3,     2,     2,     3,
       0,     2,     2,     0,     2,     2,     6,     0,     1,     1,
       1,     1,     1,     6,     8,     8,     4,     3,     3,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     0,     0,
       4,     4,     2,     2,     0,     2,     0,     1,     3,     3,
       3,     3,     4,     7,     1,     0,     5,     5,     6,     6,
       2,     2,     4,     0,     1,     0,     1,     2,     2,     4,
       2,     3,     3,     0,     0,     1,     3,     1,     1,     1,
       1,     6,     6,     6,     6,     1,     7,     1,     1,     1,
       1,     1,     1,     0,     2,     6,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     4,     3,
       3,     4,     1,     1,     1,     1,     1,     1,     1,     1,
       2,     2,     2,     0,     1
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

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

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF

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

# ifndef YY_LOCATION_PRINT
#  if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

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

#   define YY_LOCATION_PRINT(File, Loc)          \
  yy_location_print_ (File, &(Loc))

#  else
#   define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#  endif
# endif /* !defined YY_LOCATION_PRINT */


# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value, Location, scanner, builder); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, yyscan_t scanner, PyObject* builder)
{
  FILE *yyoutput = yyo;
  YYUSE (yyoutput);
  YYUSE (yylocationp);
  YYUSE (scanner);
  YYUSE (builder);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yykind < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yykind], *yyvaluep);
# endif
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, yyscan_t scanner, PyObject* builder)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  YY_LOCATION_PRINT (yyo, *yylocationp);
  YYFPRINTF (yyo, ": ");
  yy_symbol_value_print (yyo, yykind, yyvaluep, yylocationp, scanner, builder);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
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
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp,
                 int yyrule, yyscan_t scanner, PyObject* builder)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)],
                       &(yylsp[(yyi + 1) - (yynrhs)]), scanner, builder);
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
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
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


/* Context of a parse error.  */
typedef struct
{
  yy_state_t *yyssp;
  yysymbol_kind_t yytoken;
  YYLTYPE *yylloc;
} yypcontext_t;

/* Put in YYARG at most YYARGN of the expected tokens given the
   current YYCTX, and return the number of tokens stored in YYARG.  If
   YYARG is null, return the number of expected tokens (guaranteed to
   be less than YYNTOKENS).  Return YYENOMEM on memory exhaustion.
   Return 0 if there are more than YYARGN expected tokens, yet fill
   YYARG up to YYARGN. */
static int
yypcontext_expected_tokens (const yypcontext_t *yyctx,
                            yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  int yyn = yypact[+*yyctx->yyssp];
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
        if (yycheck[yyx + yyn] == yyx && yyx != YYSYMBOL_YYerror
            && !yytable_value_is_error (yytable[yyx + yyn]))
          {
            if (!yyarg)
              ++yycount;
            else if (yycount == yyargn)
              return 0;
            else
              yyarg[yycount++] = YY_CAST (yysymbol_kind_t, yyx);
          }
    }
  if (yyarg && yycount == 0 && 0 < yyargn)
    yyarg[0] = YYSYMBOL_YYEMPTY;
  return yycount;
}




#ifndef yystrlen
# if defined __GLIBC__ && defined _STRING_H
#  define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
# else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
# endif
#endif

#ifndef yystpcpy
# if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#  define yystpcpy stpcpy
# else
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
# endif
#endif

#ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
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

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
}
#endif


static int
yy_syntax_error_arguments (const yypcontext_t *yyctx,
                           yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
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
  if (yyctx->yytoken != YYSYMBOL_YYEMPTY)
    {
      int yyn;
      if (yyarg)
        yyarg[yycount] = yyctx->yytoken;
      ++yycount;
      yyn = yypcontext_expected_tokens (yyctx,
                                        yyarg ? yyarg + 1 : yyarg, yyargn - 1);
      if (yyn == YYENOMEM)
        return YYENOMEM;
      else
        yycount += yyn;
    }
  return yycount;
}

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return -1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return YYENOMEM if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                const yypcontext_t *yyctx)
{
  enum { YYARGS_MAX = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  yysymbol_kind_t yyarg[YYARGS_MAX];
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* Actual size of YYARG. */
  int yycount = yy_syntax_error_arguments (yyctx, yyarg, YYARGS_MAX);
  if (yycount == YYENOMEM)
    return YYENOMEM;

  switch (yycount)
    {
#define YYCASE_(N, S)                       \
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
#undef YYCASE_
    }

  /* Compute error message size.  Don't count the "%s"s, but reserve
     room for the terminator.  */
  yysize = yystrlen (yyformat) - 2 * yycount + 1;
  {
    int yyi;
    for (yyi = 0; yyi < yycount; ++yyi)
      {
        YYPTRDIFF_T yysize1
          = yysize + yytnamerr (YY_NULLPTR, yytname[yyarg[yyi]]);
        if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
          yysize = yysize1;
        else
          return YYENOMEM;
      }
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return -1;
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
          yyp += yytnamerr (yyp, yytname[yyarg[yyi++]]);
          yyformat += 2;
        }
      else
        {
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep, YYLTYPE *yylocationp, yyscan_t scanner, PyObject* builder)
{
  YYUSE (yyvaluep);
  YYUSE (yylocationp);
  YYUSE (scanner);
  YYUSE (builder);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yykind);
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

    yy_state_fast_t yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.
       'yyls': related to locations.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize;

    /* The state stack.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss;
    yy_state_t *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    /* The location stack.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls;
    YYLTYPE *yylsp;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

  /* The locations where the error started and ended.  */
  YYLTYPE yyerror_range[3];

  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yynerrs = 0;
  yystate = 0;
  yyerrstatus = 0;

  yystacksize = YYINITDEPTH;
  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yylsp = yyls = yylsa;


  YYDPRINTF ((stderr, "Starting parse\n"));

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
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    goto yyexhaustedlab;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yyls1, yysize * YYSIZEOF (*yylsp),
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
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
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

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

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

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex (&yylval, &yylloc, scanner, builder);
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      yyerror_range[1] = yylloc;
      goto yyerrlab1;
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
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
  *++yylsp = yylloc;

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
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
  case 2:
#line 279 "beancount/parser/grammar.y"
    {
        (yyval.character) = '*';
    }
#line 1974 "beancount/parser/grammar.c"
    break;

  case 3:
#line 283 "beancount/parser/grammar.y"
    {
        (yyval.character) = (yyvsp[0].character);
    }
#line 1982 "beancount/parser/grammar.c"
    break;

  case 4:
#line 287 "beancount/parser/grammar.y"
    {
        (yyval.character) = '*';
    }
#line 1990 "beancount/parser/grammar.c"
    break;

  case 5:
#line 291 "beancount/parser/grammar.y"
    {
        (yyval.character) = '#';
    }
#line 1998 "beancount/parser/grammar.c"
    break;

  case 12:
#line 310 "beancount/parser/grammar.y"
            {
                (yyval.pyobj) = (yyvsp[0].pyobj);
            }
#line 2006 "beancount/parser/grammar.c"
    break;

  case 13:
#line 314 "beancount/parser/grammar.y"
            {
                (yyval.pyobj) = PyNumber_Add((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
                DECREF((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 2015 "beancount/parser/grammar.c"
    break;

  case 14:
#line 319 "beancount/parser/grammar.y"
            {
                (yyval.pyobj) = PyNumber_Subtract((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
                DECREF((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 2024 "beancount/parser/grammar.c"
    break;

  case 15:
#line 324 "beancount/parser/grammar.y"
            {
                (yyval.pyobj) = PyNumber_Multiply((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
                DECREF((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 2033 "beancount/parser/grammar.c"
    break;

  case 16:
#line 329 "beancount/parser/grammar.y"
            {
                (yyval.pyobj) = PyNumber_TrueDivide((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
                DECREF((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 2042 "beancount/parser/grammar.c"
    break;

  case 17:
#line 334 "beancount/parser/grammar.y"
            {
                (yyval.pyobj) = PyNumber_Negative((yyvsp[0].pyobj));
                DECREF((yyvsp[0].pyobj));
            }
#line 2051 "beancount/parser/grammar.c"
    break;

  case 18:
#line 339 "beancount/parser/grammar.y"
            {
                (yyval.pyobj) = (yyvsp[0].pyobj);
            }
#line 2059 "beancount/parser/grammar.c"
    break;

  case 19:
#line 343 "beancount/parser/grammar.y"
            {
                (yyval.pyobj) = (yyvsp[-1].pyobj);
            }
#line 2067 "beancount/parser/grammar.c"
    break;

  case 20:
#line 348 "beancount/parser/grammar.y"
            {
                Py_INCREF(Py_None);
                (yyval.pyobj) = Py_None;
            }
#line 2076 "beancount/parser/grammar.c"
    break;

  case 21:
#line 353 "beancount/parser/grammar.y"
            {
                BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                       (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
            }
#line 2085 "beancount/parser/grammar.c"
    break;

  case 22:
#line 358 "beancount/parser/grammar.y"
            {
                BUILDY(,
                       (yyval.pyobj), "pipe_deprecated_error", "");
                (yyval.pyobj) = (yyvsp[-1].pyobj);
            }
#line 2095 "beancount/parser/grammar.c"
    break;

  case 23:
#line 365 "beancount/parser/grammar.y"
           {
               BUILDY(,
                      (yyval.pyobj), "tag_link_new", "");
           }
#line 2104 "beancount/parser/grammar.c"
    break;

  case 24:
#line 370 "beancount/parser/grammar.y"
           {
               BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                      (yyval.pyobj), "tag_link_LINK", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
           }
#line 2113 "beancount/parser/grammar.c"
    break;

  case 25:
#line 375 "beancount/parser/grammar.y"
           {
               BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                      (yyval.pyobj), "tag_link_TAG", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
           }
#line 2122 "beancount/parser/grammar.c"
    break;

  case 26:
#line 381 "beancount/parser/grammar.y"
            {
                BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                       (yyval.pyobj), "transaction", "ObOOO", (yyvsp[-5].pyobj), (yyvsp[-4].character), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 2131 "beancount/parser/grammar.c"
    break;

  case 27:
#line 387 "beancount/parser/grammar.y"
        {
            (yyval.character) = '\0';
        }
#line 2139 "beancount/parser/grammar.c"
    break;

  case 28:
#line 391 "beancount/parser/grammar.y"
        {
            (yyval.character) = '*';
        }
#line 2147 "beancount/parser/grammar.c"
    break;

  case 29:
#line 395 "beancount/parser/grammar.y"
        {
            (yyval.character) = '#';
        }
#line 2155 "beancount/parser/grammar.c"
    break;

  case 31:
#line 401 "beancount/parser/grammar.y"
                 {
                     (yyval.pyobj) = (yyvsp[0].pyobj);
                 }
#line 2163 "beancount/parser/grammar.c"
    break;

  case 32:
#line 406 "beancount/parser/grammar.y"
        {
            BUILDY(DECREF((yyvsp[0].pyobj)),
                   (yyval.pyobj), "account", "O", (yyvsp[0].pyobj));
        }
#line 2172 "beancount/parser/grammar.c"
    break;

  case 33:
#line 412 "beancount/parser/grammar.y"
        {
            BUILDY(DECREF((yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[-1].pyobj)),
                   (yyval.pyobj), "posting", "OOOOOb", (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[-1].pyobj), Py_None, Py_False, (yyvsp[-4].character));
        }
#line 2181 "beancount/parser/grammar.c"
    break;

  case 34:
#line 417 "beancount/parser/grammar.y"
        {
            BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj)),
                   (yyval.pyobj), "posting", "OOOOOb", (yyvsp[-5].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj), Py_False, (yyvsp[-6].character));
        }
#line 2190 "beancount/parser/grammar.c"
    break;

  case 35:
#line 422 "beancount/parser/grammar.y"
        {
            BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj)),
                   (yyval.pyobj), "posting", "OOOOOb", (yyvsp[-5].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj), Py_True, (yyvsp[-6].character));
        }
#line 2199 "beancount/parser/grammar.c"
    break;

  case 36:
#line 427 "beancount/parser/grammar.y"
        {
            BUILDY(DECREF((yyvsp[-1].pyobj)),
                   (yyval.pyobj), "posting", "OOOOOb", (yyvsp[-1].pyobj), missing_obj, Py_None, Py_None, Py_False, (yyvsp[-2].character));
        }
#line 2208 "beancount/parser/grammar.c"
    break;

  case 37:
#line 433 "beancount/parser/grammar.y"
          {
              BUILDY(DECREF((yyvsp[-1].string), (yyvsp[0].pyobj)),
                     (yyval.pyobj), "key_value", "OO", (yyvsp[-1].string), (yyvsp[0].pyobj));
          }
#line 2217 "beancount/parser/grammar.c"
    break;

  case 38:
#line 439 "beancount/parser/grammar.y"
               {
                   (yyval.pyobj) = (yyvsp[-1].pyobj);
               }
#line 2225 "beancount/parser/grammar.c"
    break;

  case 47:
#line 452 "beancount/parser/grammar.y"
                {
                    (yyval.pyobj) = (yyvsp[0].pyobj);
                }
#line 2233 "beancount/parser/grammar.c"
    break;

  case 48:
#line 456 "beancount/parser/grammar.y"
                {
                    Py_INCREF(Py_None);
                    (yyval.pyobj) = Py_None;
                }
#line 2242 "beancount/parser/grammar.c"
    break;

  case 49:
#line 462 "beancount/parser/grammar.y"
                   {
                       Py_INCREF(Py_None);
                       (yyval.pyobj) = Py_None;
                   }
#line 2251 "beancount/parser/grammar.c"
    break;

  case 50:
#line 467 "beancount/parser/grammar.y"
                   {
                       (yyval.pyobj) = (yyvsp[-3].pyobj);
                   }
#line 2259 "beancount/parser/grammar.c"
    break;

  case 51:
#line 471 "beancount/parser/grammar.y"
                   {
                       BUILDY(DECREF((yyvsp[-3].pyobj), (yyvsp[-1].pyobj)),
                              (yyval.pyobj), "handle_list", "OO", (yyvsp[-3].pyobj), (yyvsp[-1].pyobj));
                   }
#line 2268 "beancount/parser/grammar.c"
    break;

  case 52:
#line 476 "beancount/parser/grammar.y"
                   {
                       BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                              (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                   }
#line 2277 "beancount/parser/grammar.c"
    break;

  case 53:
#line 481 "beancount/parser/grammar.y"
                   {
                       BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                              (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                   }
#line 2286 "beancount/parser/grammar.c"
    break;

  case 54:
#line 487 "beancount/parser/grammar.y"
               {
                   Py_INCREF(Py_None);
                   (yyval.pyobj) = Py_None;
               }
#line 2295 "beancount/parser/grammar.c"
    break;

  case 55:
#line 492 "beancount/parser/grammar.y"
               {
                   BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                          (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
               }
#line 2304 "beancount/parser/grammar.c"
    break;

  case 56:
#line 498 "beancount/parser/grammar.y"
              {
                  Py_INCREF(Py_None);
                  (yyval.pyobj) = Py_None;
              }
#line 2313 "beancount/parser/grammar.c"
    break;

  case 57:
#line 503 "beancount/parser/grammar.y"
              {
                  BUILDY(DECREF((yyvsp[0].pyobj)),
                         (yyval.pyobj), "handle_list", "OO", Py_None, (yyvsp[0].pyobj));
              }
#line 2322 "beancount/parser/grammar.c"
    break;

  case 58:
#line 508 "beancount/parser/grammar.y"
              {
                  BUILDY(DECREF((yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                         (yyval.pyobj), "handle_list", "OO", (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
              }
#line 2331 "beancount/parser/grammar.c"
    break;

  case 59:
#line 514 "beancount/parser/grammar.y"
         {
             BUILDY(DECREF((yyvsp[-1].pyobj)),
                    (yyval.pyobj), "pushtag", "O", (yyvsp[-1].pyobj));
         }
#line 2340 "beancount/parser/grammar.c"
    break;

  case 60:
#line 520 "beancount/parser/grammar.y"
       {
           BUILDY(DECREF((yyvsp[-1].pyobj)),
                  (yyval.pyobj), "poptag", "O", (yyvsp[-1].pyobj));
       }
#line 2349 "beancount/parser/grammar.c"
    break;

  case 61:
#line 526 "beancount/parser/grammar.y"
         {
             /* Note: key_value is a tuple, Py_BuildValue() won't wrap it up
              * within a tuple, so expand in the method (it receives two
              * objects). See https://docs.python.org/3.4/c-api/arg.html. */
             BUILDY(DECREF((yyvsp[-1].pyobj)),
                    (yyval.pyobj), "pushmeta", "O", (yyvsp[-1].pyobj));
         }
#line 2361 "beancount/parser/grammar.c"
    break;

  case 62:
#line 535 "beancount/parser/grammar.y"
        {
            BUILDY(DECREF((yyvsp[-2].pyobj)),
                   (yyval.pyobj), "popmeta", "O", (yyvsp[-2].pyobj));
        }
#line 2370 "beancount/parser/grammar.c"
    break;

  case 63:
#line 541 "beancount/parser/grammar.y"
     {
         BUILDY(DECREF((yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                (yyval.pyobj), "open", "OOOOO", (yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
         ;
     }
#line 2380 "beancount/parser/grammar.c"
    break;

  case 64:
#line 548 "beancount/parser/grammar.y"
            {
                (yyval.pyobj) = (yyvsp[0].pyobj);
            }
#line 2388 "beancount/parser/grammar.c"
    break;

  case 65:
#line 552 "beancount/parser/grammar.y"
            {
                Py_INCREF(Py_None);
                (yyval.pyobj) = Py_None;
            }
#line 2397 "beancount/parser/grammar.c"
    break;

  case 66:
#line 558 "beancount/parser/grammar.y"
      {
          BUILDY(DECREF((yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                 (yyval.pyobj), "close", "OOO", (yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      }
#line 2406 "beancount/parser/grammar.c"
    break;

  case 67:
#line 564 "beancount/parser/grammar.y"
          {
              BUILDY(DECREF((yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                     (yyval.pyobj), "commodity", "OOO", (yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
          }
#line 2415 "beancount/parser/grammar.c"
    break;

  case 68:
#line 570 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "pad", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
    }
#line 2424 "beancount/parser/grammar.c"
    break;

  case 69:
#line 576 "beancount/parser/grammar.y"
        {
            BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[0].pyobj), (yyvsp[-2].pairobj).pyobj1, (yyvsp[-2].pairobj).pyobj2),
                   (yyval.pyobj), "balance", "OOOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pairobj).pyobj1, (yyvsp[-2].pairobj).pyobj2, (yyvsp[0].pyobj));
        }
#line 2433 "beancount/parser/grammar.c"
    break;

  case 70:
#line 582 "beancount/parser/grammar.y"
       {
           BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                  (yyval.pyobj), "amount", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
       }
#line 2442 "beancount/parser/grammar.c"
    break;

  case 71:
#line 588 "beancount/parser/grammar.y"
                 {
                     BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                            (yyval.pairobj).pyobj1, "amount", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                     (yyval.pairobj).pyobj2 = Py_None;
                     Py_INCREF(Py_None);
                     ;
                 }
#line 2454 "beancount/parser/grammar.c"
    break;

  case 72:
#line 596 "beancount/parser/grammar.y"
                 {
                     BUILDY(DECREF((yyvsp[-3].pyobj), (yyvsp[0].pyobj)),
                            (yyval.pairobj).pyobj1, "amount", "OO", (yyvsp[-3].pyobj), (yyvsp[0].pyobj));
                     (yyval.pairobj).pyobj2 = (yyvsp[-1].pyobj);
                 }
#line 2464 "beancount/parser/grammar.c"
    break;

  case 73:
#line 603 "beancount/parser/grammar.y"
             {
                 Py_INCREF(missing_obj);
                 (yyval.pyobj) = missing_obj;
             }
#line 2473 "beancount/parser/grammar.c"
    break;

  case 74:
#line 608 "beancount/parser/grammar.y"
             {
                 (yyval.pyobj) = (yyvsp[0].pyobj);
             }
#line 2481 "beancount/parser/grammar.c"
    break;

  case 75:
#line 613 "beancount/parser/grammar.y"
             {
                 Py_INCREF(missing_obj);
                 (yyval.pyobj) = missing_obj;
             }
#line 2490 "beancount/parser/grammar.c"
    break;

  case 76:
#line 618 "beancount/parser/grammar.y"
             {
                 (yyval.pyobj) = (yyvsp[0].pyobj);
             }
#line 2498 "beancount/parser/grammar.c"
    break;

  case 77:
#line 623 "beancount/parser/grammar.y"
                {
                    BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                           (yyval.pyobj), "compound_amount", "OOO", (yyvsp[-1].pyobj), Py_None, (yyvsp[0].pyobj));
                }
#line 2507 "beancount/parser/grammar.c"
    break;

  case 78:
#line 628 "beancount/parser/grammar.y"
                {
                    BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                           (yyval.pyobj), "compound_amount", "OOO", (yyvsp[-1].pyobj), Py_None, (yyvsp[0].pyobj));
                }
#line 2516 "beancount/parser/grammar.c"
    break;

  case 79:
#line 633 "beancount/parser/grammar.y"
                {
                    BUILDY(DECREF((yyvsp[-3].pyobj), (yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                           (yyval.pyobj), "compound_amount", "OOO", (yyvsp[-3].pyobj), (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                    ;
                }
#line 2526 "beancount/parser/grammar.c"
    break;

  case 80:
#line 640 "beancount/parser/grammar.y"
                  {
                      BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                             (yyval.pyobj), "amount", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                 }
#line 2535 "beancount/parser/grammar.c"
    break;

  case 81:
#line 646 "beancount/parser/grammar.y"
          {
              BUILDY(DECREF((yyvsp[-1].pyobj)),
                     (yyval.pyobj), "cost_spec", "OO", (yyvsp[-1].pyobj), Py_False);
          }
#line 2544 "beancount/parser/grammar.c"
    break;

  case 82:
#line 651 "beancount/parser/grammar.y"
          {
              BUILDY(DECREF((yyvsp[-1].pyobj)),
                     (yyval.pyobj), "cost_spec", "OO", (yyvsp[-1].pyobj), Py_True);
          }
#line 2553 "beancount/parser/grammar.c"
    break;

  case 83:
#line 656 "beancount/parser/grammar.y"
          {
              Py_INCREF(Py_None);
              (yyval.pyobj) = Py_None;
          }
#line 2562 "beancount/parser/grammar.c"
    break;

  case 84:
#line 662 "beancount/parser/grammar.y"
               {
                   /* We indicate that there was a cost if there */
                   (yyval.pyobj) = PyList_New(0);
               }
#line 2571 "beancount/parser/grammar.c"
    break;

  case 85:
#line 667 "beancount/parser/grammar.y"
               {
                   BUILDY(DECREF((yyvsp[0].pyobj)),
                          (yyval.pyobj), "handle_list", "OO", Py_None, (yyvsp[0].pyobj));
               }
#line 2580 "beancount/parser/grammar.c"
    break;

  case 86:
#line 672 "beancount/parser/grammar.y"
               {
                   BUILDY(DECREF((yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                          (yyval.pyobj), "handle_list", "OO", (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
               }
#line 2589 "beancount/parser/grammar.c"
    break;

  case 87:
#line 678 "beancount/parser/grammar.y"
          {
              (yyval.pyobj) = (yyvsp[0].pyobj);
          }
#line 2597 "beancount/parser/grammar.c"
    break;

  case 88:
#line 682 "beancount/parser/grammar.y"
          {
              (yyval.pyobj) = (yyvsp[0].pyobj);
          }
#line 2605 "beancount/parser/grammar.c"
    break;

  case 89:
#line 686 "beancount/parser/grammar.y"
          {
              (yyval.pyobj) = (yyvsp[0].pyobj);
          }
#line 2613 "beancount/parser/grammar.c"
    break;

  case 90:
#line 690 "beancount/parser/grammar.y"
          {
              BUILDY(,
                     (yyval.pyobj), "cost_merge", "O", Py_None);
          }
#line 2622 "beancount/parser/grammar.c"
    break;

  case 91:
#line 697 "beancount/parser/grammar.y"
      {
          BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                 (yyval.pyobj), "price", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      }
#line 2631 "beancount/parser/grammar.c"
    break;

  case 92:
#line 703 "beancount/parser/grammar.y"
      {
          BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                 (yyval.pyobj), "event", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      }
#line 2640 "beancount/parser/grammar.c"
    break;

  case 93:
#line 709 "beancount/parser/grammar.y"
         {
             BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                    (yyval.pyobj), "query", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
         }
#line 2649 "beancount/parser/grammar.c"
    break;

  case 94:
#line 715 "beancount/parser/grammar.y"
      {
          BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                 (yyval.pyobj), "note", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      }
#line 2658 "beancount/parser/grammar.c"
    break;

  case 96:
#line 723 "beancount/parser/grammar.y"
         {
             BUILDY(DECREF((yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                    (yyval.pyobj), "document", "OOOOO", (yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
         }
#line 2667 "beancount/parser/grammar.c"
    break;

  case 97:
#line 730 "beancount/parser/grammar.y"
             {
                 BUILDY(DECREF((yyvsp[0].pyobj)),
                        (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
             }
#line 2676 "beancount/parser/grammar.c"
    break;

  case 98:
#line 735 "beancount/parser/grammar.y"
             {
                 BUILDY(DECREF((yyvsp[0].pyobj)),
                        (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
             }
#line 2685 "beancount/parser/grammar.c"
    break;

  case 99:
#line 740 "beancount/parser/grammar.y"
             {
                 BUILDY(DECREF((yyvsp[0].pyobj)),
                        (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
             }
#line 2694 "beancount/parser/grammar.c"
    break;

  case 100:
#line 745 "beancount/parser/grammar.y"
             {
                 BUILDY(DECREF((yyvsp[0].pyobj)),
                        (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
             }
#line 2703 "beancount/parser/grammar.c"
    break;

  case 101:
#line 750 "beancount/parser/grammar.y"
             {
                 BUILDY(DECREF((yyvsp[0].pyobj)),
                        (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
             }
#line 2712 "beancount/parser/grammar.c"
    break;

  case 102:
#line 755 "beancount/parser/grammar.y"
             {
                 /* Obtain beancount.core.account.TYPE */
                 PyObject* module = PyImport_ImportModule("beancount.core.account");
                 PyObject* dtype = PyObject_GetAttrString(module, "TYPE");
                 Py_DECREF(module);
                 BUILDY(DECREF((yyvsp[0].pyobj), dtype),
                        (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), dtype);
             }
#line 2725 "beancount/parser/grammar.c"
    break;

  case 103:
#line 765 "beancount/parser/grammar.y"
                  {
                      Py_INCREF(Py_None);
                      (yyval.pyobj) = Py_None;
                  }
#line 2734 "beancount/parser/grammar.c"
    break;

  case 104:
#line 770 "beancount/parser/grammar.y"
                  {
                      BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                             (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                  }
#line 2743 "beancount/parser/grammar.c"
    break;

  case 105:
#line 776 "beancount/parser/grammar.y"
       {
           BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                  (yyval.pyobj), "custom", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
       }
#line 2752 "beancount/parser/grammar.c"
    break;

  case 117:
#line 794 "beancount/parser/grammar.y"
      {
          (yyval.pyobj) = (yyvsp[0].pyobj);
      }
#line 2760 "beancount/parser/grammar.c"
    break;

  case 118:
#line 799 "beancount/parser/grammar.y"
       {
           BUILDY(DECREF((yyvsp[-2].pyobj), (yyvsp[-1].pyobj)),
                  (yyval.pyobj), "option", "OO", (yyvsp[-2].pyobj), (yyvsp[-1].pyobj));
       }
#line 2769 "beancount/parser/grammar.c"
    break;

  case 119:
#line 805 "beancount/parser/grammar.y"
       {
           BUILDY(DECREF((yyvsp[-1].pyobj)),
                  (yyval.pyobj), "include", "O", (yyvsp[-1].pyobj));
       }
#line 2778 "beancount/parser/grammar.c"
    break;

  case 120:
#line 811 "beancount/parser/grammar.y"
       {
           BUILDY(DECREF((yyvsp[-1].pyobj)),
                  (yyval.pyobj), "plugin", "OO", (yyvsp[-1].pyobj), Py_None);
       }
#line 2787 "beancount/parser/grammar.c"
    break;

  case 121:
#line 816 "beancount/parser/grammar.y"
       {
           BUILDY(DECREF((yyvsp[-2].pyobj), (yyvsp[-1].pyobj)),
                  (yyval.pyobj), "plugin", "OO", (yyvsp[-2].pyobj), (yyvsp[-1].pyobj));
       }
#line 2796 "beancount/parser/grammar.c"
    break;

  case 130:
#line 832 "beancount/parser/grammar.y"
             {
                 (yyval.pyobj) = (yyvsp[-1].pyobj);
             }
#line 2804 "beancount/parser/grammar.c"
    break;

  case 131:
#line 836 "beancount/parser/grammar.y"
             {
                 BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                        (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
             }
#line 2813 "beancount/parser/grammar.c"
    break;

  case 132:
#line 841 "beancount/parser/grammar.y"
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
                  * appearance of an error but makes the parser errors
                  * skip the next valid directive, so we just have to make sure
                  * repeated runs of this rule's handling code are idempotent.
                  */
                 (yyval.pyobj) = (yyvsp[-1].pyobj);
             }
#line 2835 "beancount/parser/grammar.c"
    break;

  case 133:
#line 859 "beancount/parser/grammar.y"
             {
                  Py_INCREF(Py_None);
                  (yyval.pyobj) = Py_None;
             }
#line 2844 "beancount/parser/grammar.c"
    break;

  case 134:
#line 866 "beancount/parser/grammar.y"
     {
         BUILDY(DECREF((yyvsp[0].pyobj)),
                (yyval.pyobj), "store_result", "O", (yyvsp[0].pyobj));
     }
#line 2853 "beancount/parser/grammar.c"
    break;


#line 2857 "beancount/parser/grammar.c"

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
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

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
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      {
        yypcontext_t yyctx
          = {yyssp, yytoken, &yylloc};
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == -1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *,
                             YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (yymsg)
              {
                yysyntax_error_status
                  = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
                yymsgp = yymsg;
              }
            else
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = YYENOMEM;
              }
          }
        yyerror (&yylloc, scanner, builder, yymsgp);
        if (yysyntax_error_status == YYENOMEM)
          goto yyexhaustedlab;
      }
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

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
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
                  YY_ACCESSING_SYMBOL (yystate), yyvsp, yylsp, scanner, builder);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  ++yylsp;
  YYLLOC_DEFAULT (*yylsp, yyerror_range, 2);

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

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


#if 1
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
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp, yylsp, scanner, builder);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
  return yyresult;
}

#line 874 "beancount/parser/grammar.y"


/* Get a printable version of a token name. */
const char* token_to_string(int token)
{
    return yytname[YYTRANSLATE(token)];
}

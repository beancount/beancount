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

/* Macros to clean up memory for temporaries in rule reductions. */
#define DECREF1(x1)                        Py_DECREF(x1);
#define DECREF2(x1, x2)                    DECREF1(x1); Py_DECREF(x2);
#define DECREF3(x1, x2, x3)                DECREF2(x1, x2); Py_DECREF(x3);
#define DECREF4(x1, x2, x3, x4)            DECREF3(x1, x2, x3); Py_DECREF(x4);
#define DECREF5(x1, x2, x3, x4, x5)        DECREF4(x1, x2, x3, x4); Py_DECREF(x5);
#define DECREF6(x1, x2, x3, x4, x5, x6)    DECREF5(x1, x2, x3, x4, x5); Py_DECREF(x6);


#line 155 "beancount/parser/grammar.c"

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


#line 230 "beancount/parser/grammar.c"

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
    EQUAL = 269,                   /* EQUAL  */
    COMMA = 270,                   /* COMMA  */
    TILDE = 271,                   /* TILDE  */
    HASH = 272,                    /* HASH  */
    ASTERISK = 273,                /* ASTERISK  */
    SLASH = 274,                   /* SLASH  */
    COLON = 275,                   /* COLON  */
    PLUS = 276,                    /* PLUS  */
    MINUS = 277,                   /* MINUS  */
    LPAREN = 278,                  /* LPAREN  */
    RPAREN = 279,                  /* RPAREN  */
    FLAG = 280,                    /* FLAG  */
    TXN = 281,                     /* TXN  */
    BALANCE = 282,                 /* BALANCE  */
    OPEN = 283,                    /* OPEN  */
    CLOSE = 284,                   /* CLOSE  */
    COMMODITY = 285,               /* COMMODITY  */
    PAD = 286,                     /* PAD  */
    EVENT = 287,                   /* EVENT  */
    PRICE = 288,                   /* PRICE  */
    NOTE = 289,                    /* NOTE  */
    DOCUMENT = 290,                /* DOCUMENT  */
    QUERY = 291,                   /* QUERY  */
    CUSTOM = 292,                  /* CUSTOM  */
    PUSHTAG = 293,                 /* PUSHTAG  */
    POPTAG = 294,                  /* POPTAG  */
    PUSHMETA = 295,                /* PUSHMETA  */
    POPMETA = 296,                 /* POPMETA  */
    OPTION = 297,                  /* OPTION  */
    INCLUDE = 298,                 /* INCLUDE  */
    PLUGIN = 299,                  /* PLUGIN  */
    NONE = 300,                    /* NONE  */
    BOOL = 301,                    /* BOOL  */
    DATE = 302,                    /* DATE  */
    ACCOUNT = 303,                 /* ACCOUNT  */
    CURRENCY = 304,                /* CURRENCY  */
    STRING = 305,                  /* STRING  */
    NUMBER = 306,                  /* NUMBER  */
    TAG = 307,                     /* TAG  */
    LINK = 308,                    /* LINK  */
    KEY = 309,                     /* KEY  */
    NEGATIVE = 310                 /* NEGATIVE  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 151 "beancount/parser/grammar.y"

    char character;
    const char* string;
    PyObject* pyobj;
    struct {
        PyObject* pyobj1;
        PyObject* pyobj2;
    } pairobj;

#line 312 "beancount/parser/grammar.c"

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
  YYSYMBOL_EQUAL = 14,                     /* EQUAL  */
  YYSYMBOL_COMMA = 15,                     /* COMMA  */
  YYSYMBOL_TILDE = 16,                     /* TILDE  */
  YYSYMBOL_HASH = 17,                      /* HASH  */
  YYSYMBOL_ASTERISK = 18,                  /* ASTERISK  */
  YYSYMBOL_SLASH = 19,                     /* SLASH  */
  YYSYMBOL_COLON = 20,                     /* COLON  */
  YYSYMBOL_PLUS = 21,                      /* PLUS  */
  YYSYMBOL_MINUS = 22,                     /* MINUS  */
  YYSYMBOL_LPAREN = 23,                    /* LPAREN  */
  YYSYMBOL_RPAREN = 24,                    /* RPAREN  */
  YYSYMBOL_FLAG = 25,                      /* FLAG  */
  YYSYMBOL_TXN = 26,                       /* TXN  */
  YYSYMBOL_BALANCE = 27,                   /* BALANCE  */
  YYSYMBOL_OPEN = 28,                      /* OPEN  */
  YYSYMBOL_CLOSE = 29,                     /* CLOSE  */
  YYSYMBOL_COMMODITY = 30,                 /* COMMODITY  */
  YYSYMBOL_PAD = 31,                       /* PAD  */
  YYSYMBOL_EVENT = 32,                     /* EVENT  */
  YYSYMBOL_PRICE = 33,                     /* PRICE  */
  YYSYMBOL_NOTE = 34,                      /* NOTE  */
  YYSYMBOL_DOCUMENT = 35,                  /* DOCUMENT  */
  YYSYMBOL_QUERY = 36,                     /* QUERY  */
  YYSYMBOL_CUSTOM = 37,                    /* CUSTOM  */
  YYSYMBOL_PUSHTAG = 38,                   /* PUSHTAG  */
  YYSYMBOL_POPTAG = 39,                    /* POPTAG  */
  YYSYMBOL_PUSHMETA = 40,                  /* PUSHMETA  */
  YYSYMBOL_POPMETA = 41,                   /* POPMETA  */
  YYSYMBOL_OPTION = 42,                    /* OPTION  */
  YYSYMBOL_INCLUDE = 43,                   /* INCLUDE  */
  YYSYMBOL_PLUGIN = 44,                    /* PLUGIN  */
  YYSYMBOL_NONE = 45,                      /* NONE  */
  YYSYMBOL_BOOL = 46,                      /* BOOL  */
  YYSYMBOL_DATE = 47,                      /* DATE  */
  YYSYMBOL_ACCOUNT = 48,                   /* ACCOUNT  */
  YYSYMBOL_CURRENCY = 49,                  /* CURRENCY  */
  YYSYMBOL_STRING = 50,                    /* STRING  */
  YYSYMBOL_NUMBER = 51,                    /* NUMBER  */
  YYSYMBOL_TAG = 52,                       /* TAG  */
  YYSYMBOL_LINK = 53,                      /* LINK  */
  YYSYMBOL_KEY = 54,                       /* KEY  */
  YYSYMBOL_NEGATIVE = 55,                  /* NEGATIVE  */
  YYSYMBOL_YYACCEPT = 56,                  /* $accept  */
  YYSYMBOL_empty = 57,                     /* empty  */
  YYSYMBOL_txn = 58,                       /* txn  */
  YYSYMBOL_eol = 59,                       /* eol  */
  YYSYMBOL_empty_line = 60,                /* empty_line  */
  YYSYMBOL_number_expr = 61,               /* number_expr  */
  YYSYMBOL_txn_strings = 62,               /* txn_strings  */
  YYSYMBOL_tags_links = 63,                /* tags_links  */
  YYSYMBOL_transaction = 64,               /* transaction  */
  YYSYMBOL_optflag = 65,                   /* optflag  */
  YYSYMBOL_price_annotation = 66,          /* price_annotation  */
  YYSYMBOL_posting = 67,                   /* posting  */
  YYSYMBOL_key_value = 68,                 /* key_value  */
  YYSYMBOL_key_value_line = 69,            /* key_value_line  */
  YYSYMBOL_key_value_value = 70,           /* key_value_value  */
  YYSYMBOL_posting_or_kv_list = 71,        /* posting_or_kv_list  */
  YYSYMBOL_key_value_list = 72,            /* key_value_list  */
  YYSYMBOL_currency_list = 73,             /* currency_list  */
  YYSYMBOL_pushtag = 74,                   /* pushtag  */
  YYSYMBOL_poptag = 75,                    /* poptag  */
  YYSYMBOL_pushmeta = 76,                  /* pushmeta  */
  YYSYMBOL_popmeta = 77,                   /* popmeta  */
  YYSYMBOL_open = 78,                      /* open  */
  YYSYMBOL_opt_booking = 79,               /* opt_booking  */
  YYSYMBOL_close = 80,                     /* close  */
  YYSYMBOL_commodity = 81,                 /* commodity  */
  YYSYMBOL_pad = 82,                       /* pad  */
  YYSYMBOL_balance = 83,                   /* balance  */
  YYSYMBOL_amount = 84,                    /* amount  */
  YYSYMBOL_amount_tolerance = 85,          /* amount_tolerance  */
  YYSYMBOL_maybe_number = 86,              /* maybe_number  */
  YYSYMBOL_maybe_currency = 87,            /* maybe_currency  */
  YYSYMBOL_compound_amount = 88,           /* compound_amount  */
  YYSYMBOL_incomplete_amount = 89,         /* incomplete_amount  */
  YYSYMBOL_cost_spec = 90,                 /* cost_spec  */
  YYSYMBOL_cost_comp_list = 91,            /* cost_comp_list  */
  YYSYMBOL_cost_comp = 92,                 /* cost_comp  */
  YYSYMBOL_price = 93,                     /* price  */
  YYSYMBOL_event = 94,                     /* event  */
  YYSYMBOL_query = 95,                     /* query  */
  YYSYMBOL_note = 96,                      /* note  */
  YYSYMBOL_filename = 97,                  /* filename  */
  YYSYMBOL_document = 98,                  /* document  */
  YYSYMBOL_custom_value = 99,              /* custom_value  */
  YYSYMBOL_custom_value_list = 100,        /* custom_value_list  */
  YYSYMBOL_custom = 101,                   /* custom  */
  YYSYMBOL_entry = 102,                    /* entry  */
  YYSYMBOL_option = 103,                   /* option  */
  YYSYMBOL_include = 104,                  /* include  */
  YYSYMBOL_plugin = 105,                   /* plugin  */
  YYSYMBOL_directive = 106,                /* directive  */
  YYSYMBOL_declarations = 107,             /* declarations  */
  YYSYMBOL_file = 108                      /* file  */
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
#define YYFINAL  39
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   263

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  56
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  53
/* YYNRULES -- Number of rules.  */
#define YYNRULES  134
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  240

#define YYMAXUTOK   310


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
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   284,   284,   287,   291,   295,   299,   304,   305,   309,
     310,   311,   312,   318,   322,   327,   332,   337,   342,   347,
     351,   356,   361,   366,   373,   381,   386,   392,   398,   402,
     406,   410,   412,   417,   422,   427,   432,   438,   444,   449,
     450,   451,   452,   453,   454,   455,   456,   457,   461,   467,
     472,   476,   481,   486,   492,   497,   503,   508,   513,   519,
     525,   531,   540,   546,   553,   557,   563,   569,   575,   581,
     587,   593,   601,   608,   613,   618,   623,   628,   633,   638,
     645,   651,   656,   661,   667,   672,   677,   683,   687,   691,
     695,   702,   708,   714,   720,   726,   728,   735,   740,   745,
     750,   755,   760,   770,   775,   781,   788,   789,   790,   791,
     792,   793,   794,   795,   796,   797,   798,   799,   804,   810,
     816,   821,   827,   828,   829,   830,   831,   832,   833,   834,
     837,   841,   846,   864,   871
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
  "LCURL", "RCURL", "EQUAL", "COMMA", "TILDE", "HASH", "ASTERISK", "SLASH",
  "COLON", "PLUS", "MINUS", "LPAREN", "RPAREN", "FLAG", "TXN", "BALANCE",
  "OPEN", "CLOSE", "COMMODITY", "PAD", "EVENT", "PRICE", "NOTE",
  "DOCUMENT", "QUERY", "CUSTOM", "PUSHTAG", "POPTAG", "PUSHMETA",
  "POPMETA", "OPTION", "INCLUDE", "PLUGIN", "NONE", "BOOL", "DATE",
  "ACCOUNT", "CURRENCY", "STRING", "NUMBER", "TAG", "LINK", "KEY",
  "NEGATIVE", "$accept", "empty", "txn", "eol", "empty_line",
  "number_expr", "txn_strings", "tags_links", "transaction", "optflag",
  "price_annotation", "posting", "key_value", "key_value_line",
  "key_value_value", "posting_or_kv_list", "key_value_list",
  "currency_list", "pushtag", "poptag", "pushmeta", "popmeta", "open",
  "opt_booking", "close", "commodity", "pad", "balance", "amount",
  "amount_tolerance", "maybe_number", "maybe_currency", "compound_amount",
  "incomplete_amount", "cost_spec", "cost_comp_list", "cost_comp", "price",
  "event", "query", "note", "filename", "document", "custom_value",
  "custom_value_list", "custom", "entry", "option", "include", "plugin",
  "directive", "declarations", "file", YY_NULLPTR
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
     305,   306,   307,   308,   309,   310
};
#endif

#define YYPACT_NINF (-119)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-135)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -119,  -119,   126,    12,  -119,  -119,  -119,  -119,  -119,   -26,
     -22,   -15,    -2,    -8,     9,    29,   226,  -119,  -119,  -119,
    -119,  -119,  -119,  -119,  -119,  -119,  -119,  -119,  -119,  -119,
    -119,  -119,  -119,  -119,  -119,  -119,  -119,  -119,  -119,  -119,
      20,    20,    50,    20,    62,    38,    20,    11,  -119,  -119,
    -119,  -119,    41,    52,    58,    49,    67,    72,    76,    80,
      85,    86,    87,  -119,  -119,   134,  -119,  -119,   190,  -119,
      20,    20,  -119,    20,  -119,    51,    91,    20,    20,   101,
     100,    51,   104,   105,   109,  -119,  -119,    10,  -119,    51,
      51,    51,  -119,  -119,  -119,  -119,  -119,  -119,  -119,  -119,
    -119,   171,  -119,  -119,  -119,  -119,  -119,   123,    20,  -119,
    -119,   -12,  -119,  -119,    20,    20,   171,    20,    20,  -119,
    -119,    20,  -119,   130,  -119,  -119,  -119,     3,  -119,  -119,
     197,    51,    51,    51,    51,  -119,    51,  -119,  -119,   122,
    -119,  -119,    20,  -119,   176,   176,  -119,  -119,  -119,  -119,
       3,  -119,  -119,  -119,  -119,  -119,  -119,   171,  -119,  -119,
    -119,  -119,  -119,  -119,  -119,  -119,    30,    30,   180,   176,
    -119,  -119,   -15,  -119,   176,   176,   176,   176,  -119,   176,
     176,  -119,   179,  -119,   176,    20,   176,    -4,  -119,  -119,
    -119,   170,  -119,  -119,  -119,   135,     1,   136,  -119,  -119,
      24,  -119,  -119,   227,   142,    25,  -119,  -119,  -119,    96,
      96,  -119,    14,  -119,  -119,  -119,    -6,   139,    15,  -119,
      16,  -119,    82,    51,    51,  -119,  -119,    51,  -119,  -119,
      96,  -119,    20,  -119,    20,   145,  -119,  -119,  -119,  -119
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,   133,     0,     0,   132,    11,     9,    10,    12,     0,
       0,     0,     0,     0,     0,     0,     0,   122,   106,   123,
     124,   125,   126,   108,   109,   115,   110,   107,   114,   113,
     116,   112,   111,   117,   131,   127,   128,   129,   130,     1,
       0,     0,     0,     0,     0,     0,     0,     0,     6,     5,
       4,     3,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     2,     7,     0,    59,    60,     2,    61,
       0,     0,   119,     0,   120,     0,     2,     0,     0,     0,
       0,     0,     0,     0,     0,     2,    21,     2,     8,     0,
       0,     0,    45,    44,    41,    40,    42,    39,    13,    43,
      48,    46,    37,    47,    62,   118,   121,     0,     0,    57,
      56,     2,     2,     2,     0,     0,     0,     0,     0,    95,
       2,     0,   103,     0,    23,    22,    24,     0,    19,    18,
       0,     0,     0,     0,     0,    70,     0,    71,     2,     0,
      64,    65,     0,    54,    66,    67,     2,     2,     2,     2,
       0,     2,    99,    98,   102,    97,     2,   101,   100,   104,
      26,    25,     2,    20,    16,    17,    14,    15,     0,    69,
      58,     2,     0,    55,    68,    92,    91,    94,     2,    93,
     105,    49,    27,    72,    63,     0,    96,     2,    53,    52,
      38,     0,    30,    29,    31,    24,     0,     0,    50,    51,
       2,    73,    36,    74,     2,     2,    76,    75,    80,     2,
       2,    83,     0,    90,    88,    89,    84,     2,     0,    87,
       0,    85,     0,     2,     2,    33,    78,     2,    77,    82,
       2,    81,     0,    32,     0,     0,    86,    35,    34,    79
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -119,     0,  -119,   -37,  -119,   -24,  -119,  -118,  -119,  -119,
     -28,  -119,   192,    26,  -119,  -119,   -55,  -119,  -119,  -119,
    -119,  -119,  -119,  -119,  -119,  -119,  -119,  -119,   -61,  -119,
    -106,   -10,  -119,    22,  -119,     4,    -5,  -119,  -119,  -119,
    -119,  -119,  -119,  -119,  -119,  -119,  -119,  -119,  -119,  -119,
    -119,  -119,  -119
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,   143,    63,    66,    17,   203,    87,   127,    18,   197,
     232,   188,   185,   173,   102,   182,   144,   111,    19,    20,
      21,    22,    23,   142,    24,    25,    26,    27,   103,   108,
     204,   208,   219,   233,   212,   220,   221,    28,    29,    30,
      31,   120,    32,   159,   123,    33,    34,    35,    36,    37,
      38,     2,     3
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
       1,   191,   150,   139,    67,   199,    69,    64,    65,    72,
      74,   -73,    39,   192,   193,    64,    65,   124,    64,    65,
     117,   194,   223,   224,    64,    65,    40,   229,    64,    65,
      41,   230,   227,   104,   105,   209,   106,   210,   140,    42,
     112,   113,    45,   -73,   101,    89,    90,    91,   131,   132,
      42,   107,    44,   160,   161,   160,   161,   116,   145,    46,
     125,    73,   158,    86,   228,   128,   129,   130,   100,   196,
      68,   138,    89,    90,    91,    98,   110,   146,   147,    47,
     148,   149,    70,   169,   151,   122,   156,   126,    71,    75,
     162,   174,   175,   176,   177,   231,   179,   230,    78,   157,
      76,   180,    98,   218,   218,   171,    77,   164,   165,   166,
     167,   141,   168,   178,   213,    79,   184,    89,    90,    91,
     126,   235,    80,   186,   218,    81,  -134,     4,    82,     5,
       6,     7,     8,    83,    64,    65,    84,    85,    88,   136,
     109,   131,   132,   214,   133,   134,   215,    98,   190,   114,
     115,    89,    90,    91,   118,   119,   -74,   131,   132,   121,
     133,   134,   181,   202,     9,    10,    11,    12,    13,    14,
      15,   170,   137,    16,   198,   225,   152,   153,   154,   172,
     155,    98,   187,   -28,   200,   217,   217,   195,   206,   131,
     132,   206,   133,   134,   239,   237,   234,   238,   131,   132,
     201,   133,   134,    43,   207,   211,   217,   226,   189,   216,
     216,    89,    90,    91,   222,   131,   132,   207,   133,   134,
     135,   163,   205,   201,   201,   236,     0,   201,     0,   183,
     201,     0,     0,     0,     0,    92,    93,    94,    95,    96,
      97,    98,    99,    48,    49,   131,   132,     0,   133,   134,
       0,    50,    51,    52,    53,    54,    55,    56,    57,    58,
      59,    60,    61,    62
};

static const yytype_int16 yycheck[] =
{
       0,     5,   120,    15,    41,     4,    43,     4,     5,    46,
      47,    17,     0,    17,    18,     4,     5,     7,     4,     5,
      81,    25,     8,     9,     4,     5,    52,    11,     4,     5,
      52,    15,    17,    70,    71,    10,    73,    12,    50,    54,
      77,    78,    50,    49,    68,    21,    22,    23,    18,    19,
      54,    75,    54,    52,    53,    52,    53,    81,   113,    50,
      50,    50,   123,    63,    49,    89,    90,    91,    68,   187,
      20,   108,    21,    22,    23,    51,    76,   114,   115,    50,
     117,   118,    20,   138,   121,    85,   123,    87,    50,    48,
     127,   146,   147,   148,   149,    13,   151,    15,    49,   123,
      48,   156,    51,   209,   210,   142,    48,   131,   132,   133,
     134,   111,   136,   150,    18,    48,   171,    21,    22,    23,
     120,   227,    50,   178,   230,    49,     0,     1,    48,     3,
       4,     5,     6,    48,     4,     5,    50,    50,     4,    16,
      49,    18,    19,    47,    21,    22,    50,    51,   185,    48,
      50,    21,    22,    23,    50,    50,    17,    18,    19,    50,
      21,    22,   162,   200,    38,    39,    40,    41,    42,    43,
      44,    49,    49,    47,     4,   212,    46,    47,    48,     3,
      50,    51,     3,    48,    48,   209,   210,   187,    49,    18,
      19,    49,    21,    22,    49,   232,   224,   234,    18,    19,
     200,    21,    22,    11,   204,   205,   230,   217,   182,   209,
     210,    21,    22,    23,   210,    18,    19,   217,    21,    22,
      49,    24,   200,   223,   224,   230,    -1,   227,    -1,    49,
     230,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    17,    18,    18,    19,    -1,    21,    22,
      -1,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,    57,   107,   108,     1,     3,     4,     5,     6,    38,
      39,    40,    41,    42,    43,    44,    47,    60,    64,    74,
      75,    76,    77,    78,    80,    81,    82,    83,    93,    94,
      95,    96,    98,   101,   102,   103,   104,   105,   106,     0,
      52,    52,    54,    68,    54,    50,    50,    50,    17,    18,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    58,     4,     5,    59,    59,    20,    59,
      20,    50,    59,    50,    59,    48,    48,    48,    49,    48,
      50,    49,    48,    48,    50,    50,    57,    62,     4,    21,
      22,    23,    45,    46,    47,    48,    49,    50,    51,    52,
      57,    61,    70,    84,    59,    59,    59,    61,    85,    49,
      57,    73,    59,    59,    48,    50,    61,    84,    50,    50,
      97,    50,    57,   100,     7,    50,    57,    63,    61,    61,
      61,    18,    19,    21,    22,    49,    16,    49,    59,    15,
      50,    57,    79,    57,    72,    72,    59,    59,    59,    59,
      63,    59,    46,    47,    48,    50,    59,    61,    84,    99,
      52,    53,    59,    24,    61,    61,    61,    61,    61,    72,
      49,    59,     3,    69,    72,    72,    72,    72,    59,    72,
      72,    57,    71,    49,    72,    68,    72,     3,    67,    69,
      59,     5,    17,    18,    25,    57,    63,    65,     4,     4,
      48,    57,    59,    61,    86,    89,    49,    57,    87,    10,
      12,    57,    90,    18,    47,    50,    57,    61,    86,    88,
      91,    92,    91,     8,     9,    59,    87,    17,    49,    11,
      15,    13,    66,    89,    66,    86,    92,    59,    59,    49
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int8 yyr1[] =
{
       0,    56,    57,    58,    58,    58,    58,    59,    59,    60,
      60,    60,    60,    61,    61,    61,    61,    61,    61,    61,
      61,    62,    62,    62,    63,    63,    63,    64,    65,    65,
      65,    65,    66,    67,    67,    67,    67,    68,    69,    70,
      70,    70,    70,    70,    70,    70,    70,    70,    70,    71,
      71,    71,    71,    71,    72,    72,    73,    73,    73,    74,
      75,    76,    77,    78,    79,    79,    80,    81,    82,    83,
      84,    85,    85,    86,    86,    87,    87,    88,    88,    88,
      89,    90,    90,    90,    91,    91,    91,    92,    92,    92,
      92,    93,    94,    95,    96,    97,    98,    99,    99,    99,
      99,    99,    99,   100,   100,   101,   102,   102,   102,   102,
     102,   102,   102,   102,   102,   102,   102,   102,   103,   104,
     105,   105,   106,   106,   106,   106,   106,   106,   106,   106,
     107,   107,   107,   107,   108
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     1,     1,     1,     1,     1,     2,     1,
       1,     1,     1,     1,     3,     3,     3,     3,     2,     2,
       3,     1,     2,     2,     1,     2,     2,     6,     1,     1,
       1,     1,     1,     6,     8,     8,     4,     3,     3,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       4,     4,     2,     2,     1,     2,     1,     1,     3,     3,
       3,     3,     4,     7,     1,     1,     5,     5,     6,     6,
       2,     2,     4,     1,     1,     1,     1,     2,     2,     4,
       2,     3,     3,     1,     1,     1,     3,     1,     1,     1,
       1,     6,     6,     6,     6,     1,     7,     1,     1,     1,
       1,     1,     1,     1,     2,     6,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     4,     3,
       3,     4,     1,     1,     1,     1,     1,     1,     1,     1,
       2,     2,     2,     1,     1
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
  case 3:
#line 288 "beancount/parser/grammar.y"
    {
        (yyval.character) = '*';
    }
#line 1986 "beancount/parser/grammar.c"
    break;

  case 4:
#line 292 "beancount/parser/grammar.y"
    {
        (yyval.character) = (yyvsp[0].character);
    }
#line 1994 "beancount/parser/grammar.c"
    break;

  case 5:
#line 296 "beancount/parser/grammar.y"
    {
        (yyval.character) = '*';
    }
#line 2002 "beancount/parser/grammar.c"
    break;

  case 6:
#line 300 "beancount/parser/grammar.y"
    {
        (yyval.character) = '#';
    }
#line 2010 "beancount/parser/grammar.c"
    break;

  case 13:
#line 319 "beancount/parser/grammar.y"
            {
                (yyval.pyobj) = (yyvsp[0].pyobj);
            }
#line 2018 "beancount/parser/grammar.c"
    break;

  case 14:
#line 323 "beancount/parser/grammar.y"
            {
                (yyval.pyobj) = PyNumber_Add((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
                DECREF2((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 2027 "beancount/parser/grammar.c"
    break;

  case 15:
#line 328 "beancount/parser/grammar.y"
            {
                (yyval.pyobj) = PyNumber_Subtract((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
                DECREF2((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 2036 "beancount/parser/grammar.c"
    break;

  case 16:
#line 333 "beancount/parser/grammar.y"
            {
                (yyval.pyobj) = PyNumber_Multiply((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
                DECREF2((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 2045 "beancount/parser/grammar.c"
    break;

  case 17:
#line 338 "beancount/parser/grammar.y"
            {
                (yyval.pyobj) = PyNumber_TrueDivide((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
                DECREF2((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 2054 "beancount/parser/grammar.c"
    break;

  case 18:
#line 343 "beancount/parser/grammar.y"
            {
                (yyval.pyobj) = PyNumber_Negative((yyvsp[0].pyobj));
                DECREF1((yyvsp[0].pyobj));
            }
#line 2063 "beancount/parser/grammar.c"
    break;

  case 19:
#line 348 "beancount/parser/grammar.y"
            {
                (yyval.pyobj) = (yyvsp[0].pyobj);
            }
#line 2071 "beancount/parser/grammar.c"
    break;

  case 20:
#line 352 "beancount/parser/grammar.y"
            {
                (yyval.pyobj) = (yyvsp[-1].pyobj);
            }
#line 2079 "beancount/parser/grammar.c"
    break;

  case 21:
#line 357 "beancount/parser/grammar.y"
            {
                Py_INCREF(Py_None);
                (yyval.pyobj) = Py_None;
            }
#line 2088 "beancount/parser/grammar.c"
    break;

  case 22:
#line 362 "beancount/parser/grammar.y"
            {
                BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                       (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
            }
#line 2097 "beancount/parser/grammar.c"
    break;

  case 23:
#line 367 "beancount/parser/grammar.y"
            {
                BUILDY(,
                       (yyval.pyobj), "pipe_deprecated_error", "");
                (yyval.pyobj) = (yyvsp[-1].pyobj);
            }
#line 2107 "beancount/parser/grammar.c"
    break;

  case 24:
#line 374 "beancount/parser/grammar.y"
           {
               /* Note: We're passing a bogus value here in order to avoid
                * having to declare a second macro just for this one special
                * case. */
               BUILDY(,
                      (yyval.pyobj), "tag_link_new", "O", Py_None);
           }
#line 2119 "beancount/parser/grammar.c"
    break;

  case 25:
#line 382 "beancount/parser/grammar.y"
           {
               BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                      (yyval.pyobj), "tag_link_LINK", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
           }
#line 2128 "beancount/parser/grammar.c"
    break;

  case 26:
#line 387 "beancount/parser/grammar.y"
           {
               BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                      (yyval.pyobj), "tag_link_TAG", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
           }
#line 2137 "beancount/parser/grammar.c"
    break;

  case 27:
#line 393 "beancount/parser/grammar.y"
            {
                BUILDY(DECREF4((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                       (yyval.pyobj), "transaction", "ObOOO", (yyvsp[-5].pyobj), (yyvsp[-4].character), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 2146 "beancount/parser/grammar.c"
    break;

  case 28:
#line 399 "beancount/parser/grammar.y"
        {
            (yyval.character) = '\0';
        }
#line 2154 "beancount/parser/grammar.c"
    break;

  case 29:
#line 403 "beancount/parser/grammar.y"
        {
            (yyval.character) = '*';
        }
#line 2162 "beancount/parser/grammar.c"
    break;

  case 30:
#line 407 "beancount/parser/grammar.y"
        {
            (yyval.character) = '#';
        }
#line 2170 "beancount/parser/grammar.c"
    break;

  case 32:
#line 413 "beancount/parser/grammar.y"
                 {
                     (yyval.pyobj) = (yyvsp[0].pyobj);
                 }
#line 2178 "beancount/parser/grammar.c"
    break;

  case 33:
#line 418 "beancount/parser/grammar.y"
        {
            BUILDY(DECREF3((yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[-1].pyobj)),
                   (yyval.pyobj), "posting", "OOOOOb", (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[-1].pyobj), Py_None, Py_False, (yyvsp[-4].character));
        }
#line 2187 "beancount/parser/grammar.c"
    break;

  case 34:
#line 423 "beancount/parser/grammar.y"
        {
            BUILDY(DECREF4((yyvsp[-5].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj)),
                   (yyval.pyobj), "posting", "OOOOOb", (yyvsp[-5].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj), Py_False, (yyvsp[-6].character));
        }
#line 2196 "beancount/parser/grammar.c"
    break;

  case 35:
#line 428 "beancount/parser/grammar.y"
        {
            BUILDY(DECREF4((yyvsp[-5].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj)),
                   (yyval.pyobj), "posting", "OOOOOb", (yyvsp[-5].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj), Py_True, (yyvsp[-6].character));
        }
#line 2205 "beancount/parser/grammar.c"
    break;

  case 36:
#line 433 "beancount/parser/grammar.y"
        {
            BUILDY(DECREF1((yyvsp[-1].pyobj)),
                   (yyval.pyobj), "posting", "OOOOOb", (yyvsp[-1].pyobj), missing_obj, Py_None, Py_None, Py_False, (yyvsp[-2].character));
        }
#line 2214 "beancount/parser/grammar.c"
    break;

  case 37:
#line 439 "beancount/parser/grammar.y"
          {
              BUILDY(DECREF2((yyvsp[-1].string), (yyvsp[0].pyobj)),
                     (yyval.pyobj), "key_value", "OO", (yyvsp[-1].string), (yyvsp[0].pyobj));
          }
#line 2223 "beancount/parser/grammar.c"
    break;

  case 38:
#line 445 "beancount/parser/grammar.y"
               {
                   (yyval.pyobj) = (yyvsp[-1].pyobj);
               }
#line 2231 "beancount/parser/grammar.c"
    break;

  case 47:
#line 458 "beancount/parser/grammar.y"
                {
                    (yyval.pyobj) = (yyvsp[0].pyobj);
                }
#line 2239 "beancount/parser/grammar.c"
    break;

  case 48:
#line 462 "beancount/parser/grammar.y"
                {
                    Py_INCREF(Py_None);
                    (yyval.pyobj) = Py_None;
                }
#line 2248 "beancount/parser/grammar.c"
    break;

  case 49:
#line 468 "beancount/parser/grammar.y"
                   {
                       Py_INCREF(Py_None);
                       (yyval.pyobj) = Py_None;
                   }
#line 2257 "beancount/parser/grammar.c"
    break;

  case 50:
#line 473 "beancount/parser/grammar.y"
                   {
                       (yyval.pyobj) = (yyvsp[-3].pyobj);
                   }
#line 2265 "beancount/parser/grammar.c"
    break;

  case 51:
#line 477 "beancount/parser/grammar.y"
                   {
                       BUILDY(DECREF2((yyvsp[-3].pyobj), (yyvsp[-1].pyobj)),
                              (yyval.pyobj), "handle_list", "OO", (yyvsp[-3].pyobj), (yyvsp[-1].pyobj));
                   }
#line 2274 "beancount/parser/grammar.c"
    break;

  case 52:
#line 482 "beancount/parser/grammar.y"
                   {
                       BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                              (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                   }
#line 2283 "beancount/parser/grammar.c"
    break;

  case 53:
#line 487 "beancount/parser/grammar.y"
                   {
                       BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                              (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                   }
#line 2292 "beancount/parser/grammar.c"
    break;

  case 54:
#line 493 "beancount/parser/grammar.y"
               {
                   Py_INCREF(Py_None);
                   (yyval.pyobj) = Py_None;
               }
#line 2301 "beancount/parser/grammar.c"
    break;

  case 55:
#line 498 "beancount/parser/grammar.y"
               {
                   BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                          (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
               }
#line 2310 "beancount/parser/grammar.c"
    break;

  case 56:
#line 504 "beancount/parser/grammar.y"
              {
                  Py_INCREF(Py_None);
                  (yyval.pyobj) = Py_None;
              }
#line 2319 "beancount/parser/grammar.c"
    break;

  case 57:
#line 509 "beancount/parser/grammar.y"
              {
                  BUILDY(DECREF1((yyvsp[0].pyobj)),
                         (yyval.pyobj), "handle_list", "OO", Py_None, (yyvsp[0].pyobj));
              }
#line 2328 "beancount/parser/grammar.c"
    break;

  case 58:
#line 514 "beancount/parser/grammar.y"
              {
                  BUILDY(DECREF2((yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                         (yyval.pyobj), "handle_list", "OO", (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
              }
#line 2337 "beancount/parser/grammar.c"
    break;

  case 59:
#line 520 "beancount/parser/grammar.y"
         {
             BUILDY(DECREF1((yyvsp[-1].pyobj)),
                    (yyval.pyobj), "pushtag", "O", (yyvsp[-1].pyobj));
         }
#line 2346 "beancount/parser/grammar.c"
    break;

  case 60:
#line 526 "beancount/parser/grammar.y"
       {
           BUILDY(DECREF1((yyvsp[-1].pyobj)),
                  (yyval.pyobj), "poptag", "O", (yyvsp[-1].pyobj));
       }
#line 2355 "beancount/parser/grammar.c"
    break;

  case 61:
#line 532 "beancount/parser/grammar.y"
         {
             /* Note: key_value is a tuple, Py_BuildValue() won't wrap it up
              * within a tuple, so expand in the method (it receives two
              * objects). See https://docs.python.org/3.4/c-api/arg.html. */
             BUILDY(DECREF1((yyvsp[-1].pyobj)),
                    (yyval.pyobj), "pushmeta", "O", (yyvsp[-1].pyobj));
         }
#line 2367 "beancount/parser/grammar.c"
    break;

  case 62:
#line 541 "beancount/parser/grammar.y"
        {
            BUILDY(DECREF1((yyvsp[-2].pyobj)),
                   (yyval.pyobj), "popmeta", "O", (yyvsp[-2].pyobj));
        }
#line 2376 "beancount/parser/grammar.c"
    break;

  case 63:
#line 547 "beancount/parser/grammar.y"
     {
         BUILDY(DECREF5((yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                (yyval.pyobj), "open", "OOOOO", (yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
         ;
     }
#line 2386 "beancount/parser/grammar.c"
    break;

  case 64:
#line 554 "beancount/parser/grammar.y"
            {
                (yyval.pyobj) = (yyvsp[0].pyobj);
            }
#line 2394 "beancount/parser/grammar.c"
    break;

  case 65:
#line 558 "beancount/parser/grammar.y"
            {
                Py_INCREF(Py_None);
                (yyval.pyobj) = Py_None;
            }
#line 2403 "beancount/parser/grammar.c"
    break;

  case 66:
#line 564 "beancount/parser/grammar.y"
      {
          BUILDY(DECREF3((yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                 (yyval.pyobj), "close", "OOO", (yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      }
#line 2412 "beancount/parser/grammar.c"
    break;

  case 67:
#line 570 "beancount/parser/grammar.y"
          {
              BUILDY(DECREF3((yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                     (yyval.pyobj), "commodity", "OOO", (yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
          }
#line 2421 "beancount/parser/grammar.c"
    break;

  case 68:
#line 576 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF4((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "pad", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
    }
#line 2430 "beancount/parser/grammar.c"
    break;

  case 69:
#line 582 "beancount/parser/grammar.y"
        {
            BUILDY(DECREF5((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[0].pyobj), (yyvsp[-2].pairobj).pyobj1, (yyvsp[-2].pairobj).pyobj2),
                   (yyval.pyobj), "balance", "OOOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pairobj).pyobj1, (yyvsp[-2].pairobj).pyobj2, (yyvsp[0].pyobj));
        }
#line 2439 "beancount/parser/grammar.c"
    break;

  case 70:
#line 588 "beancount/parser/grammar.y"
       {
           BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                  (yyval.pyobj), "amount", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
       }
#line 2448 "beancount/parser/grammar.c"
    break;

  case 71:
#line 594 "beancount/parser/grammar.y"
                 {
                     BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                            (yyval.pairobj).pyobj1, "amount", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                     (yyval.pairobj).pyobj2 = Py_None;
                     Py_INCREF(Py_None);
                     ;
                 }
#line 2460 "beancount/parser/grammar.c"
    break;

  case 72:
#line 602 "beancount/parser/grammar.y"
                 {
                     BUILDY(DECREF2((yyvsp[-3].pyobj), (yyvsp[0].pyobj)),
                            (yyval.pairobj).pyobj1, "amount", "OO", (yyvsp[-3].pyobj), (yyvsp[0].pyobj));
                     (yyval.pairobj).pyobj2 = (yyvsp[-1].pyobj);
                 }
#line 2470 "beancount/parser/grammar.c"
    break;

  case 73:
#line 609 "beancount/parser/grammar.y"
             {
                 Py_INCREF(missing_obj);
                 (yyval.pyobj) = missing_obj;
             }
#line 2479 "beancount/parser/grammar.c"
    break;

  case 74:
#line 614 "beancount/parser/grammar.y"
             {
                 (yyval.pyobj) = (yyvsp[0].pyobj);
             }
#line 2487 "beancount/parser/grammar.c"
    break;

  case 75:
#line 619 "beancount/parser/grammar.y"
             {
                 Py_INCREF(missing_obj);
                 (yyval.pyobj) = missing_obj;
             }
#line 2496 "beancount/parser/grammar.c"
    break;

  case 76:
#line 624 "beancount/parser/grammar.y"
             {
                 (yyval.pyobj) = (yyvsp[0].pyobj);
             }
#line 2504 "beancount/parser/grammar.c"
    break;

  case 77:
#line 629 "beancount/parser/grammar.y"
                {
                    BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                           (yyval.pyobj), "compound_amount", "OOO", (yyvsp[-1].pyobj), Py_None, (yyvsp[0].pyobj));
                }
#line 2513 "beancount/parser/grammar.c"
    break;

  case 78:
#line 634 "beancount/parser/grammar.y"
                {
                    BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                           (yyval.pyobj), "compound_amount", "OOO", (yyvsp[-1].pyobj), Py_None, (yyvsp[0].pyobj));
                }
#line 2522 "beancount/parser/grammar.c"
    break;

  case 79:
#line 639 "beancount/parser/grammar.y"
                {
                    BUILDY(DECREF3((yyvsp[-3].pyobj), (yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                           (yyval.pyobj), "compound_amount", "OOO", (yyvsp[-3].pyobj), (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                    ;
                }
#line 2532 "beancount/parser/grammar.c"
    break;

  case 80:
#line 646 "beancount/parser/grammar.y"
                  {
                      BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                             (yyval.pyobj), "amount", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                 }
#line 2541 "beancount/parser/grammar.c"
    break;

  case 81:
#line 652 "beancount/parser/grammar.y"
          {
              BUILDY(DECREF1((yyvsp[-1].pyobj)),
                     (yyval.pyobj), "cost_spec", "OO", (yyvsp[-1].pyobj), Py_False);
          }
#line 2550 "beancount/parser/grammar.c"
    break;

  case 82:
#line 657 "beancount/parser/grammar.y"
          {
              BUILDY(DECREF1((yyvsp[-1].pyobj)),
                     (yyval.pyobj), "cost_spec", "OO", (yyvsp[-1].pyobj), Py_True);
          }
#line 2559 "beancount/parser/grammar.c"
    break;

  case 83:
#line 662 "beancount/parser/grammar.y"
          {
              Py_INCREF(Py_None);
              (yyval.pyobj) = Py_None;
          }
#line 2568 "beancount/parser/grammar.c"
    break;

  case 84:
#line 668 "beancount/parser/grammar.y"
               {
                   /* We indicate that there was a cost if there */
                   (yyval.pyobj) = PyList_New(0);
               }
#line 2577 "beancount/parser/grammar.c"
    break;

  case 85:
#line 673 "beancount/parser/grammar.y"
               {
                   BUILDY(DECREF1((yyvsp[0].pyobj)),
                          (yyval.pyobj), "handle_list", "OO", Py_None, (yyvsp[0].pyobj));
               }
#line 2586 "beancount/parser/grammar.c"
    break;

  case 86:
#line 678 "beancount/parser/grammar.y"
               {
                   BUILDY(DECREF2((yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                          (yyval.pyobj), "handle_list", "OO", (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
               }
#line 2595 "beancount/parser/grammar.c"
    break;

  case 87:
#line 684 "beancount/parser/grammar.y"
          {
              (yyval.pyobj) = (yyvsp[0].pyobj);
          }
#line 2603 "beancount/parser/grammar.c"
    break;

  case 88:
#line 688 "beancount/parser/grammar.y"
          {
              (yyval.pyobj) = (yyvsp[0].pyobj);
          }
#line 2611 "beancount/parser/grammar.c"
    break;

  case 89:
#line 692 "beancount/parser/grammar.y"
          {
              (yyval.pyobj) = (yyvsp[0].pyobj);
          }
#line 2619 "beancount/parser/grammar.c"
    break;

  case 90:
#line 696 "beancount/parser/grammar.y"
          {
              BUILDY(,
                     (yyval.pyobj), "cost_merge", "O", Py_None);
          }
#line 2628 "beancount/parser/grammar.c"
    break;

  case 91:
#line 703 "beancount/parser/grammar.y"
      {
          BUILDY(DECREF4((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                 (yyval.pyobj), "price", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      }
#line 2637 "beancount/parser/grammar.c"
    break;

  case 92:
#line 709 "beancount/parser/grammar.y"
      {
          BUILDY(DECREF4((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                 (yyval.pyobj), "event", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      }
#line 2646 "beancount/parser/grammar.c"
    break;

  case 93:
#line 715 "beancount/parser/grammar.y"
         {
             BUILDY(DECREF4((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                    (yyval.pyobj), "query", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
         }
#line 2655 "beancount/parser/grammar.c"
    break;

  case 94:
#line 721 "beancount/parser/grammar.y"
      {
          BUILDY(DECREF4((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                 (yyval.pyobj), "note", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      }
#line 2664 "beancount/parser/grammar.c"
    break;

  case 96:
#line 729 "beancount/parser/grammar.y"
         {
             BUILDY(DECREF5((yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                    (yyval.pyobj), "document", "OOOOO", (yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
         }
#line 2673 "beancount/parser/grammar.c"
    break;

  case 97:
#line 736 "beancount/parser/grammar.y"
             {
                 BUILDY(DECREF1((yyvsp[0].pyobj)),
                        (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
             }
#line 2682 "beancount/parser/grammar.c"
    break;

  case 98:
#line 741 "beancount/parser/grammar.y"
             {
                 BUILDY(DECREF1((yyvsp[0].pyobj)),
                        (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
             }
#line 2691 "beancount/parser/grammar.c"
    break;

  case 99:
#line 746 "beancount/parser/grammar.y"
             {
                 BUILDY(DECREF1((yyvsp[0].pyobj)),
                        (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
             }
#line 2700 "beancount/parser/grammar.c"
    break;

  case 100:
#line 751 "beancount/parser/grammar.y"
             {
                 BUILDY(DECREF1((yyvsp[0].pyobj)),
                        (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
             }
#line 2709 "beancount/parser/grammar.c"
    break;

  case 101:
#line 756 "beancount/parser/grammar.y"
             {
                 BUILDY(DECREF1((yyvsp[0].pyobj)),
                        (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
             }
#line 2718 "beancount/parser/grammar.c"
    break;

  case 102:
#line 761 "beancount/parser/grammar.y"
             {
                 /* Obtain beancount.core.account.TYPE */
                 PyObject* module = PyImport_ImportModule("beancount.core.account");
                 PyObject* dtype = PyObject_GetAttrString(module, "TYPE");
                 Py_DECREF(module);
                 BUILDY(DECREF2((yyvsp[0].pyobj), dtype),
                        (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), dtype);
             }
#line 2731 "beancount/parser/grammar.c"
    break;

  case 103:
#line 771 "beancount/parser/grammar.y"
                  {
                      Py_INCREF(Py_None);
                      (yyval.pyobj) = Py_None;
                  }
#line 2740 "beancount/parser/grammar.c"
    break;

  case 104:
#line 776 "beancount/parser/grammar.y"
                  {
                      BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                             (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                  }
#line 2749 "beancount/parser/grammar.c"
    break;

  case 105:
#line 782 "beancount/parser/grammar.y"
       {
           BUILDY(DECREF4((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                  (yyval.pyobj), "custom", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
       }
#line 2758 "beancount/parser/grammar.c"
    break;

  case 117:
#line 800 "beancount/parser/grammar.y"
      {
          (yyval.pyobj) = (yyvsp[0].pyobj);
      }
#line 2766 "beancount/parser/grammar.c"
    break;

  case 118:
#line 805 "beancount/parser/grammar.y"
       {
           BUILDY(DECREF2((yyvsp[-2].pyobj), (yyvsp[-1].pyobj)),
                  (yyval.pyobj), "option", "OO", (yyvsp[-2].pyobj), (yyvsp[-1].pyobj));
       }
#line 2775 "beancount/parser/grammar.c"
    break;

  case 119:
#line 811 "beancount/parser/grammar.y"
       {
           BUILDY(DECREF1((yyvsp[-1].pyobj)),
                  (yyval.pyobj), "include", "O", (yyvsp[-1].pyobj));
       }
#line 2784 "beancount/parser/grammar.c"
    break;

  case 120:
#line 817 "beancount/parser/grammar.y"
       {
           BUILDY(DECREF1((yyvsp[-1].pyobj)),
                  (yyval.pyobj), "plugin", "OO", (yyvsp[-1].pyobj), Py_None);
       }
#line 2793 "beancount/parser/grammar.c"
    break;

  case 121:
#line 822 "beancount/parser/grammar.y"
       {
           BUILDY(DECREF2((yyvsp[-2].pyobj), (yyvsp[-1].pyobj)),
                  (yyval.pyobj), "plugin", "OO", (yyvsp[-2].pyobj), (yyvsp[-1].pyobj));
       }
#line 2802 "beancount/parser/grammar.c"
    break;

  case 130:
#line 838 "beancount/parser/grammar.y"
             {
                 (yyval.pyobj) = (yyvsp[-1].pyobj);
             }
#line 2810 "beancount/parser/grammar.c"
    break;

  case 131:
#line 842 "beancount/parser/grammar.y"
             {
                 BUILDY(DECREF2((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                        (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
             }
#line 2819 "beancount/parser/grammar.c"
    break;

  case 132:
#line 847 "beancount/parser/grammar.y"
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
#line 2841 "beancount/parser/grammar.c"
    break;

  case 133:
#line 865 "beancount/parser/grammar.y"
             {
                  Py_INCREF(Py_None);
                  (yyval.pyobj) = Py_None;
             }
#line 2850 "beancount/parser/grammar.c"
    break;

  case 134:
#line 872 "beancount/parser/grammar.y"
     {
         BUILDY(DECREF1((yyvsp[0].pyobj)),
                (yyval.pyobj), "store_result", "O", (yyvsp[0].pyobj));
     }
#line 2859 "beancount/parser/grammar.c"
    break;


#line 2863 "beancount/parser/grammar.c"

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

#line 880 "beancount/parser/grammar.y"


/* Get a printable version of a token name. */
const char* token_to_string(int token)
{
    return yytname[YYTRANSLATE(token)];
}

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
#line 53 "beancount/parser/grammar.y"


#include "beancount/parser/macros.h"
#include "beancount/parser/grammar.h"
#include "beancount/parser/lexer.h"

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

#include "beancount/parser/parser.h"

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


#line 224 "beancount/parser/grammar.c"

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
    PIPE = 261,                    /* PIPE  */
    ATAT = 262,                    /* ATAT  */
    AT = 263,                      /* AT  */
    LCURLCURL = 264,               /* LCURLCURL  */
    RCURLCURL = 265,               /* RCURLCURL  */
    LCURL = 266,                   /* LCURL  */
    RCURL = 267,                   /* RCURL  */
    COMMA = 268,                   /* COMMA  */
    TILDE = 269,                   /* TILDE  */
    HASH = 270,                    /* HASH  */
    ASTERISK = 271,                /* ASTERISK  */
    SLASH = 272,                   /* SLASH  */
    COLON = 273,                   /* COLON  */
    PLUS = 274,                    /* PLUS  */
    MINUS = 275,                   /* MINUS  */
    LPAREN = 276,                  /* LPAREN  */
    RPAREN = 277,                  /* RPAREN  */
    FLAG = 278,                    /* FLAG  */
    TXN = 279,                     /* TXN  */
    BALANCE = 280,                 /* BALANCE  */
    OPEN = 281,                    /* OPEN  */
    CLOSE = 282,                   /* CLOSE  */
    COMMODITY = 283,               /* COMMODITY  */
    PAD = 284,                     /* PAD  */
    EVENT = 285,                   /* EVENT  */
    PRICE = 286,                   /* PRICE  */
    NOTE = 287,                    /* NOTE  */
    DOCUMENT = 288,                /* DOCUMENT  */
    QUERY = 289,                   /* QUERY  */
    CUSTOM = 290,                  /* CUSTOM  */
    PUSHTAG = 291,                 /* PUSHTAG  */
    POPTAG = 292,                  /* POPTAG  */
    PUSHMETA = 293,                /* PUSHMETA  */
    POPMETA = 294,                 /* POPMETA  */
    OPTION = 295,                  /* OPTION  */
    INCLUDE = 296,                 /* INCLUDE  */
    PLUGIN = 297,                  /* PLUGIN  */
    NONE = 298,                    /* NONE  */
    BOOL = 299,                    /* BOOL  */
    DATE = 300,                    /* DATE  */
    ACCOUNT = 301,                 /* ACCOUNT  */
    CURRENCY = 302,                /* CURRENCY  */
    STRING = 303,                  /* STRING  */
    NUMBER = 304,                  /* NUMBER  */
    TAG = 305,                     /* TAG  */
    LINK = 306,                    /* LINK  */
    KEY = 307,                     /* KEY  */
    NEGATIVE = 308                 /* NEGATIVE  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 145 "beancount/parser/grammar.y"

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
  YYSYMBOL_PIPE = 6,                       /* PIPE  */
  YYSYMBOL_ATAT = 7,                       /* ATAT  */
  YYSYMBOL_AT = 8,                         /* AT  */
  YYSYMBOL_LCURLCURL = 9,                  /* LCURLCURL  */
  YYSYMBOL_RCURLCURL = 10,                 /* RCURLCURL  */
  YYSYMBOL_LCURL = 11,                     /* LCURL  */
  YYSYMBOL_RCURL = 12,                     /* RCURL  */
  YYSYMBOL_COMMA = 13,                     /* COMMA  */
  YYSYMBOL_TILDE = 14,                     /* TILDE  */
  YYSYMBOL_HASH = 15,                      /* HASH  */
  YYSYMBOL_ASTERISK = 16,                  /* ASTERISK  */
  YYSYMBOL_SLASH = 17,                     /* SLASH  */
  YYSYMBOL_COLON = 18,                     /* COLON  */
  YYSYMBOL_PLUS = 19,                      /* PLUS  */
  YYSYMBOL_MINUS = 20,                     /* MINUS  */
  YYSYMBOL_LPAREN = 21,                    /* LPAREN  */
  YYSYMBOL_RPAREN = 22,                    /* RPAREN  */
  YYSYMBOL_FLAG = 23,                      /* FLAG  */
  YYSYMBOL_TXN = 24,                       /* TXN  */
  YYSYMBOL_BALANCE = 25,                   /* BALANCE  */
  YYSYMBOL_OPEN = 26,                      /* OPEN  */
  YYSYMBOL_CLOSE = 27,                     /* CLOSE  */
  YYSYMBOL_COMMODITY = 28,                 /* COMMODITY  */
  YYSYMBOL_PAD = 29,                       /* PAD  */
  YYSYMBOL_EVENT = 30,                     /* EVENT  */
  YYSYMBOL_PRICE = 31,                     /* PRICE  */
  YYSYMBOL_NOTE = 32,                      /* NOTE  */
  YYSYMBOL_DOCUMENT = 33,                  /* DOCUMENT  */
  YYSYMBOL_QUERY = 34,                     /* QUERY  */
  YYSYMBOL_CUSTOM = 35,                    /* CUSTOM  */
  YYSYMBOL_PUSHTAG = 36,                   /* PUSHTAG  */
  YYSYMBOL_POPTAG = 37,                    /* POPTAG  */
  YYSYMBOL_PUSHMETA = 38,                  /* PUSHMETA  */
  YYSYMBOL_POPMETA = 39,                   /* POPMETA  */
  YYSYMBOL_OPTION = 40,                    /* OPTION  */
  YYSYMBOL_INCLUDE = 41,                   /* INCLUDE  */
  YYSYMBOL_PLUGIN = 42,                    /* PLUGIN  */
  YYSYMBOL_NONE = 43,                      /* NONE  */
  YYSYMBOL_BOOL = 44,                      /* BOOL  */
  YYSYMBOL_DATE = 45,                      /* DATE  */
  YYSYMBOL_ACCOUNT = 46,                   /* ACCOUNT  */
  YYSYMBOL_CURRENCY = 47,                  /* CURRENCY  */
  YYSYMBOL_STRING = 48,                    /* STRING  */
  YYSYMBOL_NUMBER = 49,                    /* NUMBER  */
  YYSYMBOL_TAG = 50,                       /* TAG  */
  YYSYMBOL_LINK = 51,                      /* LINK  */
  YYSYMBOL_KEY = 52,                       /* KEY  */
  YYSYMBOL_NEGATIVE = 53,                  /* NEGATIVE  */
  YYSYMBOL_YYACCEPT = 54,                  /* $accept  */
  YYSYMBOL_txn = 55,                       /* txn  */
  YYSYMBOL_eol = 56,                       /* eol  */
  YYSYMBOL_empty_line = 57,                /* empty_line  */
  YYSYMBOL_number_expr = 58,               /* number_expr  */
  YYSYMBOL_txn_strings = 59,               /* txn_strings  */
  YYSYMBOL_tags_links = 60,                /* tags_links  */
  YYSYMBOL_transaction = 61,               /* transaction  */
  YYSYMBOL_optflag = 62,                   /* optflag  */
  YYSYMBOL_price_annotation = 63,          /* price_annotation  */
  YYSYMBOL_account = 64,                   /* account  */
  YYSYMBOL_posting = 65,                   /* posting  */
  YYSYMBOL_key_value = 66,                 /* key_value  */
  YYSYMBOL_key_value_line = 67,            /* key_value_line  */
  YYSYMBOL_key_value_value = 68,           /* key_value_value  */
  YYSYMBOL_posting_or_kv_list = 69,        /* posting_or_kv_list  */
  YYSYMBOL_key_value_list = 70,            /* key_value_list  */
  YYSYMBOL_currency_list = 71,             /* currency_list  */
  YYSYMBOL_pushtag = 72,                   /* pushtag  */
  YYSYMBOL_poptag = 73,                    /* poptag  */
  YYSYMBOL_pushmeta = 74,                  /* pushmeta  */
  YYSYMBOL_popmeta = 75,                   /* popmeta  */
  YYSYMBOL_open = 76,                      /* open  */
  YYSYMBOL_opt_booking = 77,               /* opt_booking  */
  YYSYMBOL_close = 78,                     /* close  */
  YYSYMBOL_commodity = 79,                 /* commodity  */
  YYSYMBOL_pad = 80,                       /* pad  */
  YYSYMBOL_balance = 81,                   /* balance  */
  YYSYMBOL_amount = 82,                    /* amount  */
  YYSYMBOL_amount_tolerance = 83,          /* amount_tolerance  */
  YYSYMBOL_maybe_number = 84,              /* maybe_number  */
  YYSYMBOL_maybe_currency = 85,            /* maybe_currency  */
  YYSYMBOL_compound_amount = 86,           /* compound_amount  */
  YYSYMBOL_incomplete_amount = 87,         /* incomplete_amount  */
  YYSYMBOL_cost_spec = 88,                 /* cost_spec  */
  YYSYMBOL_cost_comp_list = 89,            /* cost_comp_list  */
  YYSYMBOL_cost_comp = 90,                 /* cost_comp  */
  YYSYMBOL_price = 91,                     /* price  */
  YYSYMBOL_event = 92,                     /* event  */
  YYSYMBOL_query = 93,                     /* query  */
  YYSYMBOL_note = 94,                      /* note  */
  YYSYMBOL_filename = 95,                  /* filename  */
  YYSYMBOL_document = 96,                  /* document  */
  YYSYMBOL_custom_value = 97,              /* custom_value  */
  YYSYMBOL_custom_value_list = 98,         /* custom_value_list  */
  YYSYMBOL_custom = 99,                    /* custom  */
  YYSYMBOL_entry = 100,                    /* entry  */
  YYSYMBOL_option = 101,                   /* option  */
  YYSYMBOL_include = 102,                  /* include  */
  YYSYMBOL_plugin = 103,                   /* plugin  */
  YYSYMBOL_directive = 104,                /* directive  */
  YYSYMBOL_declarations = 105,             /* declarations  */
  YYSYMBOL_file = 106                      /* file  */
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
#define YYFINAL  37
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   267

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  54
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  53
/* YYNRULES -- Number of rules.  */
#define YYNRULES  133
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  226

#define YYMAXUTOK   308


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
      45,    46,    47,    48,    49,    50,    51,    52,    53
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   278,   278,   282,   286,   290,   295,   296,   300,   301,
     302,   308,   312,   317,   322,   327,   332,   337,   341,   346,
     351,   356,   363,   368,   373,   379,   385,   389,   393,   397,
     399,   404,   410,   415,   420,   425,   431,   437,   442,   443,
     444,   445,   446,   447,   448,   449,   450,   454,   460,   465,
     469,   474,   479,   485,   490,   496,   501,   506,   512,   518,
     524,   533,   539,   546,   550,   556,   562,   568,   574,   580,
     586,   594,   601,   606,   611,   616,   621,   626,   631,   638,
     644,   649,   654,   660,   665,   670,   676,   680,   684,   688,
     695,   701,   707,   713,   719,   721,   728,   733,   738,   743,
     748,   753,   763,   768,   774,   781,   782,   783,   784,   785,
     786,   787,   788,   789,   790,   791,   792,   797,   803,   809,
     814,   820,   821,   822,   823,   824,   825,   826,   827,   830,
     834,   839,   857,   864
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
  "COMMENT", "PIPE", "ATAT", "AT", "LCURLCURL", "RCURLCURL", "LCURL",
  "RCURL", "COMMA", "TILDE", "HASH", "ASTERISK", "SLASH", "COLON", "PLUS",
  "MINUS", "LPAREN", "RPAREN", "FLAG", "TXN", "BALANCE", "OPEN", "CLOSE",
  "COMMODITY", "PAD", "EVENT", "PRICE", "NOTE", "DOCUMENT", "QUERY",
  "CUSTOM", "PUSHTAG", "POPTAG", "PUSHMETA", "POPMETA", "OPTION",
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
     305,   306,   307,   308
};
#endif

#define YYPACT_NINF (-110)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-134)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -110,   105,    18,  -110,  -110,  -110,  -110,   -23,   -19,   -13,
     -10,     9,    13,    20,   232,  -110,  -110,  -110,  -110,  -110,
    -110,  -110,  -110,  -110,  -110,  -110,  -110,  -110,  -110,  -110,
    -110,  -110,  -110,  -110,  -110,  -110,  -110,  -110,    29,    29,
      52,    29,    55,    33,    29,    45,  -110,  -110,  -110,  -110,
      38,    38,    38,    47,    38,    49,    51,    38,    38,    56,
      59,  -110,  -110,   109,  -110,  -110,   195,  -110,    29,    29,
    -110,    29,  -110,  -110,   187,    75,    29,    29,    38,    71,
     187,    80,    90,    92,  -110,    -2,  -110,   187,   187,   187,
    -110,  -110,  -110,  -110,  -110,  -110,  -110,   202,  -110,  -110,
    -110,  -110,  -110,  -110,   173,    29,  -110,    10,  -110,  -110,
      29,    29,   202,    29,    29,  -110,  -110,    29,    81,  -110,
    -110,    39,  -110,  -110,   212,   187,   187,   187,   187,  -110,
     187,  -110,  -110,    76,  -110,    29,    96,    96,  -110,  -110,
    -110,  -110,    39,  -110,  -110,  -110,  -110,  -110,   202,  -110,
    -110,  -110,  -110,  -110,  -110,  -110,  -110,  -110,    19,    19,
     207,    96,  -110,  -110,   -13,  -110,    96,    96,    96,    96,
    -110,    96,    96,   145,  -110,    96,    29,    96,    72,  -110,
    -110,  -110,   148,  -110,  -110,  -110,    -3,    38,  -110,  -110,
     112,  -110,    95,   116,    -1,  -110,  -110,   146,   156,   150,
    -110,  -110,  -110,   183,    -6,  -110,     2,  -110,    79,   187,
     187,  -110,  -110,   187,  -110,  -110,   164,  -110,    29,  -110,
      29,   117,  -110,  -110,  -110,  -110
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
     132,     0,     0,   131,    10,     8,     9,     0,     0,     0,
       0,     0,     0,     0,     0,   121,   105,   122,   123,   124,
     125,   107,   108,   114,   109,   106,   113,   112,   115,   111,
     110,   116,   130,   126,   127,   128,   129,     1,     0,     0,
       0,     0,     0,     0,     0,     0,     5,     4,     3,     2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    19,     6,     0,    58,    59,    47,    60,     0,     0,
     118,     0,   119,    31,     0,    55,     0,     0,     0,     0,
       0,     0,     0,     0,   102,    22,     7,     0,     0,     0,
      44,    43,    40,    41,    38,    11,    42,    45,    39,    36,
      46,    61,   117,   120,     0,     0,    56,    64,    53,    53,
       0,     0,     0,     0,     0,    94,    22,     0,     0,    21,
      20,     0,    17,    16,     0,     0,     0,     0,     0,    69,
       0,    70,    53,     0,    63,     0,    65,    66,    53,    53,
      53,    53,     0,    53,    98,    97,    96,    53,   100,   101,
      99,   103,    24,    23,    48,    18,    14,    15,    12,    13,
       0,    68,    57,    53,     0,    54,    67,    91,    90,    93,
      53,    92,   104,    25,    71,    62,     0,    95,    22,    52,
      51,    37,     0,    28,    27,    29,     0,     0,    49,    50,
      72,    35,    73,    74,    82,    75,    79,    72,    72,     0,
      89,    87,    88,    74,     0,    86,     0,    84,     0,    72,
      72,    32,    77,    72,    76,    81,    72,    80,     0,    30,
       0,     0,    85,    34,    33,    78
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -110,  -110,   -39,  -110,   -63,  -110,  -109,  -110,  -110,   -40,
     -38,  -110,   162,     0,  -110,  -110,   -87,  -110,  -110,  -110,
    -110,  -110,  -110,  -110,  -110,  -110,  -110,  -110,   -59,  -110,
     -77,   -29,  -110,   -12,  -110,   -16,   -30,  -110,  -110,  -110,
    -110,  -110,  -110,  -110,  -110,  -110,  -110,  -110,  -110,  -110,
    -110,  -110,  -110
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    61,    64,    15,   192,    85,   121,    16,   187,   218,
      74,   179,   176,   165,    99,   173,   136,   107,    17,    18,
      19,    20,    21,   135,    22,    23,    24,    25,   100,   105,
     193,   196,   205,   219,   199,   206,   207,    26,    27,    28,
      29,   116,    30,   151,   118,    31,    32,    33,    34,    35,
      36,     1,     2
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      65,   189,    67,    97,   119,    70,    72,   142,   197,   213,
     198,   104,   215,    75,    76,   216,    78,   112,    37,    81,
      82,   113,   137,   133,   122,   123,   124,    38,    98,   101,
     102,    39,   103,    62,    63,   125,   126,   108,   109,    40,
     110,   214,    42,    62,    63,   161,   120,   152,   153,    62,
      63,   166,   167,   168,   169,   148,   171,    43,   134,   150,
     172,    44,   156,   157,   158,   159,   132,   160,    45,   186,
      66,   138,   139,    68,   140,   141,   175,   182,   143,   147,
     149,    69,   154,   177,    73,    62,    63,   183,   184,   152,
     153,   217,   216,    71,    77,   185,   163,    79,    80,   164,
      87,    88,    89,   170,    83,  -133,     3,    84,     4,     5,
       6,   125,   126,    86,   127,   128,    62,    63,   -26,   111,
     204,   204,   106,   162,    40,   144,   145,    73,   114,   146,
      95,    87,    88,    89,   203,   203,   221,   181,   115,   204,
     117,     7,     8,     9,    10,    11,    12,    13,   178,   190,
      14,   191,   188,   203,    62,    63,   -83,   209,   210,   -83,
     211,    95,   200,   195,   225,    87,    88,    89,   -83,   -83,
     220,    41,   200,   180,   212,    87,    88,    89,   194,   223,
     200,   224,   208,    87,    88,    89,   222,   130,     0,   125,
     126,   201,   127,   128,   202,    95,     0,     0,   -73,   125,
     126,   201,   127,   128,   202,    95,    87,    88,    89,   201,
       0,     0,   202,    95,    87,    88,    89,     0,   125,   126,
     131,   127,   128,   125,   126,     0,   127,   128,   125,   126,
     195,   127,   128,     0,   155,     0,    95,     0,    90,    91,
      92,    73,    93,    94,    95,    96,     0,    46,    47,   129,
       0,     0,     0,     0,   174,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    57,    58,    59,    60
};

static const yytype_int16 yycheck[] =
{
      39,     4,    41,    66,     6,    44,    45,   116,     9,    15,
      11,    74,    10,    51,    52,    13,    54,    80,     0,    57,
      58,    80,   109,    13,    87,    88,    89,    50,    66,    68,
      69,    50,    71,     4,     5,    16,    17,    76,    77,    52,
      78,    47,    52,     4,     5,   132,    48,    50,    51,     4,
       5,   138,   139,   140,   141,   118,   143,    48,    48,   118,
     147,    48,   125,   126,   127,   128,   105,   130,    48,   178,
      18,   110,   111,    18,   113,   114,   163,     5,   117,   118,
     118,    48,   121,   170,    46,     4,     5,    15,    16,    50,
      51,    12,    13,    48,    47,    23,   135,    48,    47,     3,
      19,    20,    21,   142,    48,     0,     1,    48,     3,     4,
       5,    16,    17,     4,    19,    20,     4,     5,    46,    48,
     197,   198,    47,    47,    52,    44,    45,    46,    48,    48,
      49,    19,    20,    21,   197,   198,   213,   176,    48,   216,
      48,    36,    37,    38,    39,    40,    41,    42,     3,   187,
      45,   190,     4,   216,     4,     5,    10,     7,     8,    13,
     199,    49,    16,    47,    47,    19,    20,    21,    12,    13,
     210,     9,    16,   173,   203,    19,    20,    21,   190,   218,
      16,   220,   198,    19,    20,    21,   216,    14,    -1,    16,
      17,    45,    19,    20,    48,    49,    -1,    -1,    15,    16,
      17,    45,    19,    20,    48,    49,    19,    20,    21,    45,
      -1,    -1,    48,    49,    19,    20,    21,    -1,    16,    17,
      47,    19,    20,    16,    17,    -1,    19,    20,    16,    17,
      47,    19,    20,    -1,    22,    -1,    49,    -1,    43,    44,
      45,    46,    47,    48,    49,    50,    -1,    15,    16,    47,
      -1,    -1,    -1,    -1,    47,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,   105,   106,     1,     3,     4,     5,    36,    37,    38,
      39,    40,    41,    42,    45,    57,    61,    72,    73,    74,
      75,    76,    78,    79,    80,    81,    91,    92,    93,    94,
      96,    99,   100,   101,   102,   103,   104,     0,    50,    50,
      52,    66,    52,    48,    48,    48,    15,    16,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    55,     4,     5,    56,    56,    18,    56,    18,    48,
      56,    48,    56,    46,    64,    64,    64,    47,    64,    48,
      47,    64,    64,    48,    48,    59,     4,    19,    20,    21,
      43,    44,    45,    47,    48,    49,    50,    58,    64,    68,
      82,    56,    56,    56,    58,    83,    47,    71,    56,    56,
      64,    48,    58,    82,    48,    48,    95,    48,    98,     6,
      48,    60,    58,    58,    58,    16,    17,    19,    20,    47,
      14,    47,    56,    13,    48,    77,    70,    70,    56,    56,
      56,    56,    60,    56,    44,    45,    48,    56,    58,    64,
      82,    97,    50,    51,    56,    22,    58,    58,    58,    58,
      58,    70,    47,    56,     3,    67,    70,    70,    70,    70,
      56,    70,    70,    69,    47,    70,    66,    70,     3,    65,
      67,    56,     5,    15,    16,    23,    60,    62,     4,     4,
      64,    56,    58,    84,    87,    47,    85,     9,    11,    88,
      16,    45,    48,    58,    84,    86,    89,    90,    89,     7,
       8,    56,    85,    15,    47,    10,    13,    12,    63,    87,
      63,    84,    90,    56,    56,    47
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int8 yyr1[] =
{
       0,    54,    55,    55,    55,    55,    56,    56,    57,    57,
      57,    58,    58,    58,    58,    58,    58,    58,    58,    59,
      59,    59,    60,    60,    60,    61,    62,    62,    62,    62,
      63,    64,    65,    65,    65,    65,    66,    67,    68,    68,
      68,    68,    68,    68,    68,    68,    68,    68,    69,    69,
      69,    69,    69,    70,    70,    71,    71,    71,    72,    73,
      74,    75,    76,    77,    77,    78,    79,    80,    81,    82,
      83,    83,    84,    84,    85,    85,    86,    86,    86,    87,
      88,    88,    88,    89,    89,    89,    90,    90,    90,    90,
      91,    92,    93,    94,    95,    96,    97,    97,    97,    97,
      97,    97,    98,    98,    99,   100,   100,   100,   100,   100,
     100,   100,   100,   100,   100,   100,   100,   101,   102,   103,
     103,   104,   104,   104,   104,   104,   104,   104,   104,   105,
     105,   105,   105,   106
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     1,     1,     1,     1,     2,     1,     1,
       1,     1,     3,     3,     3,     3,     2,     2,     3,     0,
       2,     2,     0,     2,     2,     6,     0,     1,     1,     1,
       1,     1,     6,     8,     8,     4,     3,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     0,     0,     4,
       4,     2,     2,     0,     2,     0,     1,     3,     3,     3,
       3,     4,     7,     1,     0,     5,     5,     6,     6,     2,
       2,     4,     0,     1,     0,     1,     2,     2,     4,     2,
       3,     3,     0,     0,     1,     3,     1,     1,     1,     1,
       6,     6,     6,     6,     1,     7,     1,     1,     1,     1,
       1,     1,     0,     2,     6,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     4,     3,     3,
       4,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       2,     2,     0,     1
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
#line 1971 "beancount/parser/grammar.c"
    break;

  case 3:
#line 283 "beancount/parser/grammar.y"
    {
        (yyval.character) = (yyvsp[0].character);
    }
#line 1979 "beancount/parser/grammar.c"
    break;

  case 4:
#line 287 "beancount/parser/grammar.y"
    {
        (yyval.character) = '*';
    }
#line 1987 "beancount/parser/grammar.c"
    break;

  case 5:
#line 291 "beancount/parser/grammar.y"
    {
        (yyval.character) = '#';
    }
#line 1995 "beancount/parser/grammar.c"
    break;

  case 11:
#line 309 "beancount/parser/grammar.y"
            {
                (yyval.pyobj) = (yyvsp[0].pyobj);
            }
#line 2003 "beancount/parser/grammar.c"
    break;

  case 12:
#line 313 "beancount/parser/grammar.y"
            {
                (yyval.pyobj) = PyNumber_Add((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
                DECREF((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 2012 "beancount/parser/grammar.c"
    break;

  case 13:
#line 318 "beancount/parser/grammar.y"
            {
                (yyval.pyobj) = PyNumber_Subtract((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
                DECREF((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 2021 "beancount/parser/grammar.c"
    break;

  case 14:
#line 323 "beancount/parser/grammar.y"
            {
                (yyval.pyobj) = PyNumber_Multiply((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
                DECREF((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 2030 "beancount/parser/grammar.c"
    break;

  case 15:
#line 328 "beancount/parser/grammar.y"
            {
                (yyval.pyobj) = PyNumber_TrueDivide((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
                DECREF((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 2039 "beancount/parser/grammar.c"
    break;

  case 16:
#line 333 "beancount/parser/grammar.y"
            {
                (yyval.pyobj) = PyNumber_Negative((yyvsp[0].pyobj));
                DECREF((yyvsp[0].pyobj));
            }
#line 2048 "beancount/parser/grammar.c"
    break;

  case 17:
#line 338 "beancount/parser/grammar.y"
            {
                (yyval.pyobj) = (yyvsp[0].pyobj);
            }
#line 2056 "beancount/parser/grammar.c"
    break;

  case 18:
#line 342 "beancount/parser/grammar.y"
            {
                (yyval.pyobj) = (yyvsp[-1].pyobj);
            }
#line 2064 "beancount/parser/grammar.c"
    break;

  case 19:
#line 347 "beancount/parser/grammar.y"
            {
                Py_INCREF(Py_None);
                (yyval.pyobj) = Py_None;
            }
#line 2073 "beancount/parser/grammar.c"
    break;

  case 20:
#line 352 "beancount/parser/grammar.y"
            {
                BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                       (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
            }
#line 2082 "beancount/parser/grammar.c"
    break;

  case 21:
#line 357 "beancount/parser/grammar.y"
            {
                BUILDY(,
                       (yyval.pyobj), "pipe_deprecated_error", "");
                (yyval.pyobj) = (yyvsp[-1].pyobj);
            }
#line 2092 "beancount/parser/grammar.c"
    break;

  case 22:
#line 364 "beancount/parser/grammar.y"
           {
               BUILDY(,
                      (yyval.pyobj), "tag_link_new", "");
           }
#line 2101 "beancount/parser/grammar.c"
    break;

  case 23:
#line 369 "beancount/parser/grammar.y"
           {
               BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                      (yyval.pyobj), "tag_link_LINK", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
           }
#line 2110 "beancount/parser/grammar.c"
    break;

  case 24:
#line 374 "beancount/parser/grammar.y"
           {
               BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                      (yyval.pyobj), "tag_link_TAG", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
           }
#line 2119 "beancount/parser/grammar.c"
    break;

  case 25:
#line 380 "beancount/parser/grammar.y"
            {
                BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                       (yyval.pyobj), "transaction", "ObOOO", (yyvsp[-5].pyobj), (yyvsp[-4].character), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
            }
#line 2128 "beancount/parser/grammar.c"
    break;

  case 26:
#line 386 "beancount/parser/grammar.y"
        {
            (yyval.character) = '\0';
        }
#line 2136 "beancount/parser/grammar.c"
    break;

  case 27:
#line 390 "beancount/parser/grammar.y"
        {
            (yyval.character) = '*';
        }
#line 2144 "beancount/parser/grammar.c"
    break;

  case 28:
#line 394 "beancount/parser/grammar.y"
        {
            (yyval.character) = '#';
        }
#line 2152 "beancount/parser/grammar.c"
    break;

  case 30:
#line 400 "beancount/parser/grammar.y"
                 {
                     (yyval.pyobj) = (yyvsp[0].pyobj);
                 }
#line 2160 "beancount/parser/grammar.c"
    break;

  case 31:
#line 405 "beancount/parser/grammar.y"
        {
            BUILDY(DECREF((yyvsp[0].pyobj)),
                   (yyval.pyobj), "account", "O", (yyvsp[0].pyobj));
        }
#line 2169 "beancount/parser/grammar.c"
    break;

  case 32:
#line 411 "beancount/parser/grammar.y"
        {
            BUILDY(DECREF((yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[-1].pyobj)),
                   (yyval.pyobj), "posting", "OOOOOb", (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[-1].pyobj), Py_None, Py_False, (yyvsp[-4].character));
        }
#line 2178 "beancount/parser/grammar.c"
    break;

  case 33:
#line 416 "beancount/parser/grammar.y"
        {
            BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj)),
                   (yyval.pyobj), "posting", "OOOOOb", (yyvsp[-5].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj), Py_False, (yyvsp[-6].character));
        }
#line 2187 "beancount/parser/grammar.c"
    break;

  case 34:
#line 421 "beancount/parser/grammar.y"
        {
            BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj)),
                   (yyval.pyobj), "posting", "OOOOOb", (yyvsp[-5].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj), Py_True, (yyvsp[-6].character));
        }
#line 2196 "beancount/parser/grammar.c"
    break;

  case 35:
#line 426 "beancount/parser/grammar.y"
        {
            BUILDY(DECREF((yyvsp[-1].pyobj)),
                   (yyval.pyobj), "posting", "OOOOOb", (yyvsp[-1].pyobj), missing_obj, Py_None, Py_None, Py_False, (yyvsp[-2].character));
        }
#line 2205 "beancount/parser/grammar.c"
    break;

  case 36:
#line 432 "beancount/parser/grammar.y"
          {
              BUILDY(DECREF((yyvsp[-1].string), (yyvsp[0].pyobj)),
                     (yyval.pyobj), "key_value", "OO", (yyvsp[-1].string), (yyvsp[0].pyobj));
          }
#line 2214 "beancount/parser/grammar.c"
    break;

  case 37:
#line 438 "beancount/parser/grammar.y"
               {
                   (yyval.pyobj) = (yyvsp[-1].pyobj);
               }
#line 2222 "beancount/parser/grammar.c"
    break;

  case 46:
#line 451 "beancount/parser/grammar.y"
                {
                    (yyval.pyobj) = (yyvsp[0].pyobj);
                }
#line 2230 "beancount/parser/grammar.c"
    break;

  case 47:
#line 455 "beancount/parser/grammar.y"
                {
                    Py_INCREF(Py_None);
                    (yyval.pyobj) = Py_None;
                }
#line 2239 "beancount/parser/grammar.c"
    break;

  case 48:
#line 461 "beancount/parser/grammar.y"
                   {
                       Py_INCREF(Py_None);
                       (yyval.pyobj) = Py_None;
                   }
#line 2248 "beancount/parser/grammar.c"
    break;

  case 49:
#line 466 "beancount/parser/grammar.y"
                   {
                       (yyval.pyobj) = (yyvsp[-3].pyobj);
                   }
#line 2256 "beancount/parser/grammar.c"
    break;

  case 50:
#line 470 "beancount/parser/grammar.y"
                   {
                       BUILDY(DECREF((yyvsp[-3].pyobj), (yyvsp[-1].pyobj)),
                              (yyval.pyobj), "handle_list", "OO", (yyvsp[-3].pyobj), (yyvsp[-1].pyobj));
                   }
#line 2265 "beancount/parser/grammar.c"
    break;

  case 51:
#line 475 "beancount/parser/grammar.y"
                   {
                       BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                              (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                   }
#line 2274 "beancount/parser/grammar.c"
    break;

  case 52:
#line 480 "beancount/parser/grammar.y"
                   {
                       BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                              (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                   }
#line 2283 "beancount/parser/grammar.c"
    break;

  case 53:
#line 486 "beancount/parser/grammar.y"
               {
                   Py_INCREF(Py_None);
                   (yyval.pyobj) = Py_None;
               }
#line 2292 "beancount/parser/grammar.c"
    break;

  case 54:
#line 491 "beancount/parser/grammar.y"
               {
                   BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                          (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
               }
#line 2301 "beancount/parser/grammar.c"
    break;

  case 55:
#line 497 "beancount/parser/grammar.y"
              {
                  Py_INCREF(Py_None);
                  (yyval.pyobj) = Py_None;
              }
#line 2310 "beancount/parser/grammar.c"
    break;

  case 56:
#line 502 "beancount/parser/grammar.y"
              {
                  BUILDY(DECREF((yyvsp[0].pyobj)),
                         (yyval.pyobj), "handle_list", "OO", Py_None, (yyvsp[0].pyobj));
              }
#line 2319 "beancount/parser/grammar.c"
    break;

  case 57:
#line 507 "beancount/parser/grammar.y"
              {
                  BUILDY(DECREF((yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                         (yyval.pyobj), "handle_list", "OO", (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
              }
#line 2328 "beancount/parser/grammar.c"
    break;

  case 58:
#line 513 "beancount/parser/grammar.y"
         {
             BUILDY(DECREF((yyvsp[-1].pyobj)),
                    (yyval.pyobj), "pushtag", "O", (yyvsp[-1].pyobj));
         }
#line 2337 "beancount/parser/grammar.c"
    break;

  case 59:
#line 519 "beancount/parser/grammar.y"
       {
           BUILDY(DECREF((yyvsp[-1].pyobj)),
                  (yyval.pyobj), "poptag", "O", (yyvsp[-1].pyobj));
       }
#line 2346 "beancount/parser/grammar.c"
    break;

  case 60:
#line 525 "beancount/parser/grammar.y"
         {
             /* Note: key_value is a tuple, Py_BuildValue() won't wrap it up
              * within a tuple, so expand in the method (it receives two
              * objects). See https://docs.python.org/3.4/c-api/arg.html. */
             BUILDY(DECREF((yyvsp[-1].pyobj)),
                    (yyval.pyobj), "pushmeta", "O", (yyvsp[-1].pyobj));
         }
#line 2358 "beancount/parser/grammar.c"
    break;

  case 61:
#line 534 "beancount/parser/grammar.y"
        {
            BUILDY(DECREF((yyvsp[-2].pyobj)),
                   (yyval.pyobj), "popmeta", "O", (yyvsp[-2].pyobj));
        }
#line 2367 "beancount/parser/grammar.c"
    break;

  case 62:
#line 540 "beancount/parser/grammar.y"
     {
         BUILDY(DECREF((yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                (yyval.pyobj), "open", "OOOOO", (yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
         ;
     }
#line 2377 "beancount/parser/grammar.c"
    break;

  case 63:
#line 547 "beancount/parser/grammar.y"
            {
                (yyval.pyobj) = (yyvsp[0].pyobj);
            }
#line 2385 "beancount/parser/grammar.c"
    break;

  case 64:
#line 551 "beancount/parser/grammar.y"
            {
                Py_INCREF(Py_None);
                (yyval.pyobj) = Py_None;
            }
#line 2394 "beancount/parser/grammar.c"
    break;

  case 65:
#line 557 "beancount/parser/grammar.y"
      {
          BUILDY(DECREF((yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                 (yyval.pyobj), "close", "OOO", (yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      }
#line 2403 "beancount/parser/grammar.c"
    break;

  case 66:
#line 563 "beancount/parser/grammar.y"
          {
              BUILDY(DECREF((yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                     (yyval.pyobj), "commodity", "OOO", (yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
          }
#line 2412 "beancount/parser/grammar.c"
    break;

  case 67:
#line 569 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "pad", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
    }
#line 2421 "beancount/parser/grammar.c"
    break;

  case 68:
#line 575 "beancount/parser/grammar.y"
        {
            BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[0].pyobj), (yyvsp[-2].pairobj).pyobj1, (yyvsp[-2].pairobj).pyobj2),
                   (yyval.pyobj), "balance", "OOOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pairobj).pyobj1, (yyvsp[-2].pairobj).pyobj2, (yyvsp[0].pyobj));
        }
#line 2430 "beancount/parser/grammar.c"
    break;

  case 69:
#line 581 "beancount/parser/grammar.y"
       {
           BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                  (yyval.pyobj), "amount", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
       }
#line 2439 "beancount/parser/grammar.c"
    break;

  case 70:
#line 587 "beancount/parser/grammar.y"
                 {
                     BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                            (yyval.pairobj).pyobj1, "amount", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                     (yyval.pairobj).pyobj2 = Py_None;
                     Py_INCREF(Py_None);
                     ;
                 }
#line 2451 "beancount/parser/grammar.c"
    break;

  case 71:
#line 595 "beancount/parser/grammar.y"
                 {
                     BUILDY(DECREF((yyvsp[-3].pyobj), (yyvsp[0].pyobj)),
                            (yyval.pairobj).pyobj1, "amount", "OO", (yyvsp[-3].pyobj), (yyvsp[0].pyobj));
                     (yyval.pairobj).pyobj2 = (yyvsp[-1].pyobj);
                 }
#line 2461 "beancount/parser/grammar.c"
    break;

  case 72:
#line 602 "beancount/parser/grammar.y"
             {
                 Py_INCREF(missing_obj);
                 (yyval.pyobj) = missing_obj;
             }
#line 2470 "beancount/parser/grammar.c"
    break;

  case 73:
#line 607 "beancount/parser/grammar.y"
             {
                 (yyval.pyobj) = (yyvsp[0].pyobj);
             }
#line 2478 "beancount/parser/grammar.c"
    break;

  case 74:
#line 612 "beancount/parser/grammar.y"
             {
                 Py_INCREF(missing_obj);
                 (yyval.pyobj) = missing_obj;
             }
#line 2487 "beancount/parser/grammar.c"
    break;

  case 75:
#line 617 "beancount/parser/grammar.y"
             {
                 (yyval.pyobj) = (yyvsp[0].pyobj);
             }
#line 2495 "beancount/parser/grammar.c"
    break;

  case 76:
#line 622 "beancount/parser/grammar.y"
                {
                    BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                           (yyval.pyobj), "compound_amount", "OOO", (yyvsp[-1].pyobj), Py_None, (yyvsp[0].pyobj));
                }
#line 2504 "beancount/parser/grammar.c"
    break;

  case 77:
#line 627 "beancount/parser/grammar.y"
                {
                    BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                           (yyval.pyobj), "compound_amount", "OOO", (yyvsp[-1].pyobj), Py_None, (yyvsp[0].pyobj));
                }
#line 2513 "beancount/parser/grammar.c"
    break;

  case 78:
#line 632 "beancount/parser/grammar.y"
                {
                    BUILDY(DECREF((yyvsp[-3].pyobj), (yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                           (yyval.pyobj), "compound_amount", "OOO", (yyvsp[-3].pyobj), (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                    ;
                }
#line 2523 "beancount/parser/grammar.c"
    break;

  case 79:
#line 639 "beancount/parser/grammar.y"
                  {
                      BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                             (yyval.pyobj), "amount", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                 }
#line 2532 "beancount/parser/grammar.c"
    break;

  case 80:
#line 645 "beancount/parser/grammar.y"
          {
              BUILDY(DECREF((yyvsp[-1].pyobj)),
                     (yyval.pyobj), "cost_spec", "OO", (yyvsp[-1].pyobj), Py_False);
          }
#line 2541 "beancount/parser/grammar.c"
    break;

  case 81:
#line 650 "beancount/parser/grammar.y"
          {
              BUILDY(DECREF((yyvsp[-1].pyobj)),
                     (yyval.pyobj), "cost_spec", "OO", (yyvsp[-1].pyobj), Py_True);
          }
#line 2550 "beancount/parser/grammar.c"
    break;

  case 82:
#line 655 "beancount/parser/grammar.y"
          {
              Py_INCREF(Py_None);
              (yyval.pyobj) = Py_None;
          }
#line 2559 "beancount/parser/grammar.c"
    break;

  case 83:
#line 661 "beancount/parser/grammar.y"
               {
                   /* We indicate that there was a cost if there */
                   (yyval.pyobj) = PyList_New(0);
               }
#line 2568 "beancount/parser/grammar.c"
    break;

  case 84:
#line 666 "beancount/parser/grammar.y"
               {
                   BUILDY(DECREF((yyvsp[0].pyobj)),
                          (yyval.pyobj), "handle_list", "OO", Py_None, (yyvsp[0].pyobj));
               }
#line 2577 "beancount/parser/grammar.c"
    break;

  case 85:
#line 671 "beancount/parser/grammar.y"
               {
                   BUILDY(DECREF((yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                          (yyval.pyobj), "handle_list", "OO", (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
               }
#line 2586 "beancount/parser/grammar.c"
    break;

  case 86:
#line 677 "beancount/parser/grammar.y"
          {
              (yyval.pyobj) = (yyvsp[0].pyobj);
          }
#line 2594 "beancount/parser/grammar.c"
    break;

  case 87:
#line 681 "beancount/parser/grammar.y"
          {
              (yyval.pyobj) = (yyvsp[0].pyobj);
          }
#line 2602 "beancount/parser/grammar.c"
    break;

  case 88:
#line 685 "beancount/parser/grammar.y"
          {
              (yyval.pyobj) = (yyvsp[0].pyobj);
          }
#line 2610 "beancount/parser/grammar.c"
    break;

  case 89:
#line 689 "beancount/parser/grammar.y"
          {
              BUILDY(,
                     (yyval.pyobj), "cost_merge", "O", Py_None);
          }
#line 2619 "beancount/parser/grammar.c"
    break;

  case 90:
#line 696 "beancount/parser/grammar.y"
      {
          BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                 (yyval.pyobj), "price", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      }
#line 2628 "beancount/parser/grammar.c"
    break;

  case 91:
#line 702 "beancount/parser/grammar.y"
      {
          BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                 (yyval.pyobj), "event", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      }
#line 2637 "beancount/parser/grammar.c"
    break;

  case 92:
#line 708 "beancount/parser/grammar.y"
         {
             BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                    (yyval.pyobj), "query", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
         }
#line 2646 "beancount/parser/grammar.c"
    break;

  case 93:
#line 714 "beancount/parser/grammar.y"
      {
          BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                 (yyval.pyobj), "note", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      }
#line 2655 "beancount/parser/grammar.c"
    break;

  case 95:
#line 722 "beancount/parser/grammar.y"
         {
             BUILDY(DECREF((yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                    (yyval.pyobj), "document", "OOOOO", (yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
         }
#line 2664 "beancount/parser/grammar.c"
    break;

  case 96:
#line 729 "beancount/parser/grammar.y"
             {
                 BUILDY(DECREF((yyvsp[0].pyobj)),
                        (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
             }
#line 2673 "beancount/parser/grammar.c"
    break;

  case 97:
#line 734 "beancount/parser/grammar.y"
             {
                 BUILDY(DECREF((yyvsp[0].pyobj)),
                        (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
             }
#line 2682 "beancount/parser/grammar.c"
    break;

  case 98:
#line 739 "beancount/parser/grammar.y"
             {
                 BUILDY(DECREF((yyvsp[0].pyobj)),
                        (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
             }
#line 2691 "beancount/parser/grammar.c"
    break;

  case 99:
#line 744 "beancount/parser/grammar.y"
             {
                 BUILDY(DECREF((yyvsp[0].pyobj)),
                        (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
             }
#line 2700 "beancount/parser/grammar.c"
    break;

  case 100:
#line 749 "beancount/parser/grammar.y"
             {
                 BUILDY(DECREF((yyvsp[0].pyobj)),
                        (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
             }
#line 2709 "beancount/parser/grammar.c"
    break;

  case 101:
#line 754 "beancount/parser/grammar.y"
             {
                 /* Obtain beancount.core.account.TYPE */
                 PyObject* module = PyImport_ImportModule("beancount.core.account");
                 PyObject* dtype = PyObject_GetAttrString(module, "TYPE");
                 Py_DECREF(module);
                 BUILDY(DECREF((yyvsp[0].pyobj), dtype),
                        (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), dtype);
             }
#line 2722 "beancount/parser/grammar.c"
    break;

  case 102:
#line 764 "beancount/parser/grammar.y"
                  {
                      Py_INCREF(Py_None);
                      (yyval.pyobj) = Py_None;
                  }
#line 2731 "beancount/parser/grammar.c"
    break;

  case 103:
#line 769 "beancount/parser/grammar.y"
                  {
                      BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                             (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
                  }
#line 2740 "beancount/parser/grammar.c"
    break;

  case 104:
#line 775 "beancount/parser/grammar.y"
       {
           BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                  (yyval.pyobj), "custom", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
       }
#line 2749 "beancount/parser/grammar.c"
    break;

  case 116:
#line 793 "beancount/parser/grammar.y"
      {
          (yyval.pyobj) = (yyvsp[0].pyobj);
      }
#line 2757 "beancount/parser/grammar.c"
    break;

  case 117:
#line 798 "beancount/parser/grammar.y"
       {
           BUILDY(DECREF((yyvsp[-2].pyobj), (yyvsp[-1].pyobj)),
                  (yyval.pyobj), "option", "OO", (yyvsp[-2].pyobj), (yyvsp[-1].pyobj));
       }
#line 2766 "beancount/parser/grammar.c"
    break;

  case 118:
#line 804 "beancount/parser/grammar.y"
       {
           BUILDY(DECREF((yyvsp[-1].pyobj)),
                  (yyval.pyobj), "include", "O", (yyvsp[-1].pyobj));
       }
#line 2775 "beancount/parser/grammar.c"
    break;

  case 119:
#line 810 "beancount/parser/grammar.y"
       {
           BUILDY(DECREF((yyvsp[-1].pyobj)),
                  (yyval.pyobj), "plugin", "OO", (yyvsp[-1].pyobj), Py_None);
       }
#line 2784 "beancount/parser/grammar.c"
    break;

  case 120:
#line 815 "beancount/parser/grammar.y"
       {
           BUILDY(DECREF((yyvsp[-2].pyobj), (yyvsp[-1].pyobj)),
                  (yyval.pyobj), "plugin", "OO", (yyvsp[-2].pyobj), (yyvsp[-1].pyobj));
       }
#line 2793 "beancount/parser/grammar.c"
    break;

  case 129:
#line 831 "beancount/parser/grammar.y"
             {
                 (yyval.pyobj) = (yyvsp[-1].pyobj);
             }
#line 2801 "beancount/parser/grammar.c"
    break;

  case 130:
#line 835 "beancount/parser/grammar.y"
             {
                 BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
                        (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
             }
#line 2810 "beancount/parser/grammar.c"
    break;

  case 131:
#line 840 "beancount/parser/grammar.y"
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
#line 2832 "beancount/parser/grammar.c"
    break;

  case 132:
#line 858 "beancount/parser/grammar.y"
             {
                  Py_INCREF(Py_None);
                  (yyval.pyobj) = Py_None;
             }
#line 2841 "beancount/parser/grammar.c"
    break;

  case 133:
#line 865 "beancount/parser/grammar.y"
     {
         BUILDY(DECREF((yyvsp[0].pyobj)),
                (yyval.pyobj), "store_result", "O", (yyvsp[0].pyobj));
     }
#line 2850 "beancount/parser/grammar.c"
    break;


#line 2854 "beancount/parser/grammar.c"

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

#line 873 "beancount/parser/grammar.y"


/* Get a printable version of a token name. */
const char* token_to_string(int token)
{
    return yytname[YYTRANSLATE(token)];
}

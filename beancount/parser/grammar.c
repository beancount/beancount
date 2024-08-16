/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
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
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

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

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 2

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 59 "beancount/parser/grammar.y"


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

#define MISSING_OBJ (yyget_extra(scanner)->missing_obj)

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


#line 150 "beancount/parser/grammar.c"

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

#include "grammar.h"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_INDENT = 3,                     /* INDENT  */
  YYSYMBOL_EOL = 4,                        /* EOL  */
  YYSYMBOL_PIPE = 5,                       /* PIPE  */
  YYSYMBOL_ATAT = 6,                       /* ATAT  */
  YYSYMBOL_AT = 7,                         /* AT  */
  YYSYMBOL_LCURLCURL = 8,                  /* LCURLCURL  */
  YYSYMBOL_RCURLCURL = 9,                  /* RCURLCURL  */
  YYSYMBOL_LCURL = 10,                     /* LCURL  */
  YYSYMBOL_RCURL = 11,                     /* RCURL  */
  YYSYMBOL_COMMA = 12,                     /* COMMA  */
  YYSYMBOL_TILDE = 13,                     /* TILDE  */
  YYSYMBOL_HASH = 14,                      /* HASH  */
  YYSYMBOL_ASTERISK = 15,                  /* ASTERISK  */
  YYSYMBOL_SLASH = 16,                     /* SLASH  */
  YYSYMBOL_COLON = 17,                     /* COLON  */
  YYSYMBOL_PLUS = 18,                      /* PLUS  */
  YYSYMBOL_MINUS = 19,                     /* MINUS  */
  YYSYMBOL_LPAREN = 20,                    /* LPAREN  */
  YYSYMBOL_RPAREN = 21,                    /* RPAREN  */
  YYSYMBOL_FLAG = 22,                      /* FLAG  */
  YYSYMBOL_CAPITAL = 23,                   /* CAPITAL  */
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
  YYSYMBOL_number_expr = 57,               /* number_expr  */
  YYSYMBOL_currency = 58,                  /* currency  */
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

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
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
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
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
#define YYFINAL  35
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   332

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  54
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  53
/* YYNRULES -- Number of rules.  */
#define YYNRULES  135
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  228

/* YYMAXUTOK -- Last valid token kind.  */
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
       0,   289,   289,   293,   297,   301,   305,   310,   310,   317,
     318,   323,   328,   333,   338,   343,   347,   353,   357,   363,
     368,   373,   381,   386,   391,   398,   405,   409,   413,   417,
     418,   421,   424,   431,   436,   441,   446,   453,   460,   466,
     467,   468,   469,   470,   471,   472,   473,   474,   475,   482,
     487,   491,   496,   501,   508,   513,   517,   524,   529,   534,
     541,   548,   555,   565,   572,   579,   580,   587,   594,   601,
     608,   615,   622,   629,   637,   638,   645,   646,   653,   658,
     663,   671,   678,   683,   688,   695,   700,   705,   712,   713,
     714,   715,   722,   729,   736,   743,   749,   752,   759,   764,
     769,   774,   779,   784,   795,   800,   807,   814,   815,   816,
     817,   818,   819,   820,   821,   822,   823,   824,   825,   828,
     835,   842,   847,   854,   855,   856,   857,   858,   859,   860,
     863,   864,   865,   870,   888,   896
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
  "PIPE", "ATAT", "AT", "LCURLCURL", "RCURLCURL", "LCURL", "RCURL",
  "COMMA", "TILDE", "HASH", "ASTERISK", "SLASH", "COLON", "PLUS", "MINUS",
  "LPAREN", "RPAREN", "FLAG", "CAPITAL", "TXN", "BALANCE", "OPEN", "CLOSE",
  "COMMODITY", "PAD", "EVENT", "PRICE", "NOTE", "DOCUMENT", "QUERY",
  "CUSTOM", "PUSHTAG", "POPTAG", "PUSHMETA", "POPMETA", "OPTION",
  "INCLUDE", "PLUGIN", "NONE", "BOOL", "DATE", "ACCOUNT", "CURRENCY",
  "STRING", "NUMBER", "TAG", "LINK", "KEY", "NEGATIVE", "$accept", "txn",
  "eol", "number_expr", "currency", "txn_strings", "tags_links",
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

#define YYPACT_NINF (-114)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-86)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -114,   187,    32,  -114,  -114,  -114,   -27,   -16,    -4,     2,
      10,    21,    27,   297,  -114,  -114,  -114,  -114,  -114,  -114,
    -114,  -114,  -114,  -114,  -114,  -114,  -114,  -114,  -114,  -114,
    -114,  -114,  -114,  -114,  -114,  -114,    25,    25,    55,    25,
      80,    52,    25,     9,  -114,  -114,  -114,  -114,  -114,    65,
      65,    65,    12,    65,    66,    12,    65,    65,    73,    76,
    -114,  -114,  -114,  -114,  -114,   260,  -114,    25,    25,  -114,
      25,  -114,  -114,     0,    12,    25,  -114,  -114,    25,    65,
      82,     0,    89,    94,    96,  -114,     5,     0,     0,     0,
    -114,  -114,  -114,  -114,  -114,  -114,   113,  -114,  -114,  -114,
    -114,  -114,  -114,  -114,   234,    25,  -114,     4,  -114,  -114,
      25,    25,   113,    25,  -114,  -114,  -114,    25,   149,  -114,
    -114,    11,  -114,  -114,    24,     0,     0,     0,     0,  -114,
       0,  -114,  -114,    12,  -114,    25,   110,   110,  -114,  -114,
    -114,    11,    11,  -114,  -114,  -114,  -114,  -114,   113,  -114,
    -114,  -114,  -114,  -114,  -114,  -114,  -114,  -114,    67,    67,
     113,   110,  -114,  -114,     8,  -114,   110,   110,   110,  -114,
    -114,   110,   110,   131,  -114,   110,  -114,    25,   110,   110,
      87,  -114,  -114,  -114,  -114,  -114,  -114,  -114,  -114,    11,
      65,  -114,   162,  -114,   155,    12,    56,  -114,  -114,   194,
     226,    86,  -114,  -114,  -114,   254,    23,  -114,    35,  -114,
      95,     0,     0,  -114,  -114,     0,  -114,  -114,   215,  -114,
      25,  -114,    25,    12,  -114,  -114,  -114,  -114
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
     134,     0,     0,   135,   133,   130,     0,     0,     0,     0,
       0,     0,     0,     0,   107,   123,   124,   125,   126,   109,
     110,   116,   111,   108,   115,   114,   117,   113,   112,   118,
     132,   127,   128,   129,   131,     1,     0,     0,     0,     0,
       0,     0,     0,     0,     5,     4,     3,     6,     2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      19,     8,     7,    60,    61,    48,    62,     0,     0,   120,
       0,   121,    32,     0,    57,     0,    18,    17,     0,     0,
       0,     0,     0,     0,     0,   104,    22,     0,     0,     0,
      45,    44,    41,    39,     9,    43,    46,    42,    40,    37,
      47,    63,   119,   122,     0,     0,    58,    66,    54,    54,
       0,     0,     0,     0,    22,    96,    22,     0,     0,    21,
      20,     0,    15,    14,     0,     0,     0,     0,     0,    71,
       0,    72,    54,     0,    65,     0,    67,    68,    54,    54,
      54,     0,     0,    54,   100,    99,    98,    54,   102,   103,
     101,   105,    24,    23,    49,    16,    12,    13,    10,    11,
       0,    70,    59,    54,     0,    56,    69,    93,    92,    54,
      54,    94,   106,    25,    73,    64,    55,     0,    95,    97,
      22,    53,    52,    38,    28,    27,    29,    30,    50,     0,
       0,    51,    75,    36,    74,    77,    84,    76,    81,    75,
      75,     0,    91,    89,    90,    77,     0,    88,     0,    86,
       0,    75,    75,    33,    79,    75,    78,    83,    75,    82,
       0,    31,     0,     0,    87,    35,    34,    80
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
    -114,  -114,   -37,   -10,   -48,  -114,  -113,  -114,  -114,   -66,
     -29,  -114,   127,   -28,  -114,  -114,   -44,  -114,  -114,  -114,
    -114,  -114,  -114,  -114,  -114,  -114,  -114,  -114,   -67,  -114,
     -77,   -57,  -114,   -42,  -114,   -49,   -64,  -114,  -114,  -114,
    -114,  -114,  -114,  -114,  -114,  -114,  -114,  -114,  -114,  -114,
    -114,  -114,  -114
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_uint8 yydefgoto[] =
{
       0,    60,    63,   194,   129,    86,   121,    14,   190,   220,
      73,   181,   177,   165,    99,   173,   136,   107,    15,    16,
      17,    18,    19,   135,    20,    21,    22,    23,   100,   105,
     195,   198,   207,   221,   201,   208,   209,    24,    25,    26,
      27,   116,    28,   151,   118,    29,    30,    31,    32,    33,
      34,     1,     2
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      64,   141,    66,   142,    78,    69,    71,    81,    61,    61,
     119,    61,    62,    62,   113,    62,   133,    97,    87,    88,
      89,    74,    75,    36,    79,    61,   106,    82,    83,    62,
     101,   102,    35,   103,    37,    76,    98,   215,   108,   125,
     126,   109,   127,   128,   217,   155,    76,   218,    38,    94,
     110,   150,   134,   120,    40,    96,   131,    70,    41,    77,
      38,   152,   153,   104,   199,   137,   200,   189,   132,    42,
      77,   112,    65,   138,   139,    43,   140,   122,   123,   124,
     143,   147,   125,   126,   154,   162,    61,    61,   161,   149,
      62,    62,   211,   212,   166,   167,   168,    67,   163,   171,
      68,   184,   185,   172,   169,   170,   219,   218,   148,   186,
     187,    72,   174,   164,    80,   156,   157,   158,   159,   175,
     160,    84,   206,   206,    85,   178,   179,   176,   125,   126,
     111,   127,   128,   -26,   180,    39,    76,   114,   223,    38,
     183,   206,   115,   188,   117,   182,   222,   197,   214,    61,
     196,   210,   191,    62,   224,   193,     0,   197,   216,     0,
      77,   192,    61,     0,   213,     0,    62,    87,    88,    89,
     125,   126,     0,   127,   128,   227,     0,     0,     0,     0,
      87,    88,    89,   225,     0,   226,     0,     3,     4,   205,
     205,     5,     0,   144,   145,    72,     0,   146,    94,     0,
       0,     0,     0,   -85,     0,     0,   -85,     0,   205,   202,
       0,    94,    87,    88,    89,     0,     0,     0,     0,     0,
       0,     0,     0,     6,     7,     8,     9,    10,    11,    12,
     202,     0,    13,    87,    88,    89,     0,   -85,   -85,   203,
       0,   202,   204,    94,    87,    88,    89,   130,     0,   125,
     126,     0,   127,   128,     0,     0,     0,    76,     0,     0,
     203,     0,     0,   204,    94,     0,     0,     0,   -74,   125,
     126,   203,   127,   128,   204,    94,     0,    76,    87,    88,
      89,    77,     0,    76,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,     0,    90,    91,    92,    72,    77,    93,    94,
      95,    44,    45,     0,     0,     0,     0,     0,     0,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    58,    59
};

static const yytype_int16 yycheck[] =
{
      37,   114,    39,   116,    52,    42,    43,    55,     0,     0,
       5,     0,     4,     4,    81,     4,    12,    65,    18,    19,
      20,    50,    51,    50,    53,     0,    74,    56,    57,     4,
      67,    68,     0,    70,    50,    23,    65,    14,    75,    15,
      16,    78,    18,    19,     9,    21,    23,    12,    52,    49,
      79,   118,    48,    48,    52,    65,   104,    48,    48,    47,
      52,    50,    51,    73,     8,   109,    10,   180,   105,    48,
      47,    81,    17,   110,   111,    48,   113,    87,    88,    89,
     117,   118,    15,    16,   121,   133,     0,     0,   132,   118,
       4,     4,     6,     7,   138,   139,   140,    17,   135,   143,
      48,    14,    15,   147,   141,   142,    11,    12,   118,    22,
      23,    46,   160,     3,    48,   125,   126,   127,   128,   163,
     130,    48,   199,   200,    48,   169,   170,   164,    15,    16,
      48,    18,    19,    46,     3,     8,    23,    48,   215,    52,
     177,   218,    48,   180,    48,   173,   212,   195,   205,     0,
     192,   200,   189,     4,   218,   192,    -1,   205,   206,    -1,
      47,   190,     0,    -1,   201,    -1,     4,    18,    19,    20,
      15,    16,    -1,    18,    19,   223,    -1,    -1,    -1,    -1,
      18,    19,    20,   220,    -1,   222,    -1,     0,     1,   199,
     200,     4,    -1,    44,    45,    46,    -1,    48,    49,    -1,
      -1,    -1,    -1,     9,    -1,    -1,    12,    -1,   218,    15,
      -1,    49,    18,    19,    20,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    36,    37,    38,    39,    40,    41,    42,
      15,    -1,    45,    18,    19,    20,    -1,    11,    12,    45,
      -1,    15,    48,    49,    18,    19,    20,    13,    -1,    15,
      16,    -1,    18,    19,    -1,    -1,    -1,    23,    -1,    -1,
      45,    -1,    -1,    48,    49,    -1,    -1,    -1,    14,    15,
      16,    45,    18,    19,    48,    49,    -1,    23,    18,    19,
      20,    47,    -1,    23,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    47,    -1,    43,    44,    45,    46,    47,    48,    49,
      50,    14,    15,    -1,    -1,    -1,    -1,    -1,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,   105,   106,     0,     1,     4,    36,    37,    38,    39,
      40,    41,    42,    45,    61,    72,    73,    74,    75,    76,
      78,    79,    80,    81,    91,    92,    93,    94,    96,    99,
     100,   101,   102,   103,   104,     0,    50,    50,    52,    66,
      52,    48,    48,    48,    14,    15,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      55,     0,     4,    56,    56,    17,    56,    17,    48,    56,
      48,    56,    46,    64,    64,    64,    23,    47,    58,    64,
      48,    58,    64,    64,    48,    48,    59,    18,    19,    20,
      43,    44,    45,    48,    49,    50,    57,    58,    64,    68,
      82,    56,    56,    56,    57,    83,    58,    71,    56,    56,
      64,    48,    57,    82,    48,    48,    95,    48,    98,     5,
      48,    60,    57,    57,    57,    15,    16,    18,    19,    58,
      13,    58,    56,    12,    48,    77,    70,    70,    56,    56,
      56,    60,    60,    56,    44,    45,    48,    56,    57,    64,
      82,    97,    50,    51,    56,    21,    57,    57,    57,    57,
      57,    70,    58,    56,     3,    67,    70,    70,    70,    56,
      56,    70,    70,    69,    58,    70,    56,    66,    70,    70,
       3,    65,    67,    56,    14,    15,    22,    23,    56,    60,
      62,    56,    64,    56,    57,    84,    87,    58,    85,     8,
      10,    88,    15,    45,    48,    57,    84,    86,    89,    90,
      89,     6,     7,    56,    85,    14,    58,     9,    12,    11,
      63,    87,    63,    84,    90,    56,    56,    58
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    54,    55,    55,    55,    55,    55,    56,    56,    57,
      57,    57,    57,    57,    57,    57,    57,    58,    58,    59,
      59,    59,    60,    60,    60,    61,    62,    62,    62,    62,
      62,    63,    64,    65,    65,    65,    65,    66,    67,    68,
      68,    68,    68,    68,    68,    68,    68,    68,    68,    69,
      69,    69,    69,    69,    70,    70,    70,    71,    71,    71,
      72,    73,    74,    75,    76,    77,    77,    78,    79,    80,
      81,    82,    83,    83,    84,    84,    85,    85,    86,    86,
      86,    87,    88,    88,    88,    89,    89,    89,    90,    90,
      90,    90,    91,    92,    93,    94,    95,    96,    97,    97,
      97,    97,    97,    97,    98,    98,    99,   100,   100,   100,
     100,   100,   100,   100,   100,   100,   100,   100,   100,   101,
     102,   103,   103,   104,   104,   104,   104,   104,   104,   104,
     105,   105,   105,   105,   105,   106
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       3,     3,     3,     3,     2,     2,     3,     1,     1,     0,
       2,     2,     0,     2,     2,     6,     0,     1,     1,     1,
       1,     1,     1,     6,     8,     8,     4,     3,     3,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     0,     0,
       3,     4,     2,     2,     0,     3,     2,     0,     1,     3,
       3,     3,     3,     4,     7,     1,     0,     5,     5,     6,
       6,     2,     2,     4,     1,     0,     1,     0,     2,     2,
       4,     2,     3,     3,     0,     0,     1,     3,     1,     1,
       1,     1,     6,     6,     6,     7,     1,     7,     1,     1,
       1,     1,     1,     1,     0,     2,     6,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     4,
       3,     3,     4,     1,     1,     1,     1,     1,     1,     1,
       2,     2,     2,     2,     0,     2
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


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


/* YYLOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

# ifndef YYLOCATION_PRINT

#  if defined YY_LOCATION_PRINT

   /* Temporary convenience wrapper in case some people defined the
      undocumented and private YY_LOCATION_PRINT macros.  */
#   define YYLOCATION_PRINT(File, Loc)  YY_LOCATION_PRINT(File, *(Loc))

#  elif defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

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

#   define YYLOCATION_PRINT  yy_location_print_

    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT(File, Loc)  YYLOCATION_PRINT(File, &(Loc))

#  else

#   define YYLOCATION_PRINT(File, Loc) ((void) 0)
    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT  YYLOCATION_PRINT

#  endif
# endif /* !defined YYLOCATION_PRINT */


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
  YY_USE (yyoutput);
  YY_USE (yylocationp);
  YY_USE (scanner);
  YY_USE (builder);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
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

  YYLOCATION_PRINT (yyo, yylocationp);
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
  YY_USE (yyvaluep);
  YY_USE (yylocationp);
  YY_USE (scanner);
  YY_USE (builder);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}






/*----------.
| yyparse.  |
`----------*/

int
yyparse (yyscan_t scanner, PyObject* builder)
{
/* Lookahead token kind.  */
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
    int yynerrs = 0;

    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

    /* The location stack: array, bottom, top.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls = yylsa;
    YYLTYPE *yylsp = yyls;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
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
    YYNOMEM;
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
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
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
  case 2: /* txn: TXN  */
#line 290 "beancount/parser/grammar.y"
    {
      (yyval.character) = '*';
    }
#line 1832 "beancount/parser/grammar.c"
    break;

  case 3: /* txn: FLAG  */
#line 294 "beancount/parser/grammar.y"
    {
        (yyval.character) = (yyvsp[0].character);
    }
#line 1840 "beancount/parser/grammar.c"
    break;

  case 4: /* txn: ASTERISK  */
#line 298 "beancount/parser/grammar.y"
    {
        (yyval.character) = '*';
    }
#line 1848 "beancount/parser/grammar.c"
    break;

  case 5: /* txn: HASH  */
#line 302 "beancount/parser/grammar.y"
    {
        (yyval.character) = '#';
    }
#line 1856 "beancount/parser/grammar.c"
    break;

  case 6: /* txn: CAPITAL  */
#line 306 "beancount/parser/grammar.y"
    {
        (yyval.character) = (yyvsp[0].character);
    }
#line 1864 "beancount/parser/grammar.c"
    break;

  case 10: /* number_expr: number_expr PLUS number_expr  */
#line 319 "beancount/parser/grammar.y"
    {
      (yyval.pyobj) = PyNumber_Add((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
      DECREF((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
    }
#line 1873 "beancount/parser/grammar.c"
    break;

  case 11: /* number_expr: number_expr MINUS number_expr  */
#line 324 "beancount/parser/grammar.y"
    {
        (yyval.pyobj) = PyNumber_Subtract((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
        DECREF((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
    }
#line 1882 "beancount/parser/grammar.c"
    break;

  case 12: /* number_expr: number_expr ASTERISK number_expr  */
#line 329 "beancount/parser/grammar.y"
    {
        (yyval.pyobj) = PyNumber_Multiply((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
        DECREF((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
    }
#line 1891 "beancount/parser/grammar.c"
    break;

  case 13: /* number_expr: number_expr SLASH number_expr  */
#line 334 "beancount/parser/grammar.y"
    {
        (yyval.pyobj) = PyNumber_TrueDivide((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
        DECREF((yyvsp[-2].pyobj), (yyvsp[0].pyobj));
    }
#line 1900 "beancount/parser/grammar.c"
    break;

  case 14: /* number_expr: MINUS number_expr  */
#line 339 "beancount/parser/grammar.y"
    {
        (yyval.pyobj) = PyNumber_Negative((yyvsp[0].pyobj));
        DECREF((yyvsp[0].pyobj));
    }
#line 1909 "beancount/parser/grammar.c"
    break;

  case 15: /* number_expr: PLUS number_expr  */
#line 344 "beancount/parser/grammar.y"
    {
        (yyval.pyobj) = (yyvsp[0].pyobj);
    }
#line 1917 "beancount/parser/grammar.c"
    break;

  case 16: /* number_expr: LPAREN number_expr RPAREN  */
#line 348 "beancount/parser/grammar.y"
    {
        (yyval.pyobj) = (yyvsp[-1].pyobj);
    }
#line 1925 "beancount/parser/grammar.c"
    break;

  case 17: /* currency: CURRENCY  */
#line 354 "beancount/parser/grammar.y"
    {
        (yyval.pyobj) = (yyvsp[0].pyobj);
    }
#line 1933 "beancount/parser/grammar.c"
    break;

  case 18: /* currency: CAPITAL  */
#line 358 "beancount/parser/grammar.y"
    {
        (yyval.pyobj) = PyUnicode_FromStringAndSize(&(yyvsp[0].character), 1);
    }
#line 1941 "beancount/parser/grammar.c"
    break;

  case 19: /* txn_strings: %empty  */
#line 364 "beancount/parser/grammar.y"
    {
        Py_INCREF(Py_None);
        (yyval.pyobj) = Py_None;
    }
#line 1950 "beancount/parser/grammar.c"
    break;

  case 20: /* txn_strings: txn_strings STRING  */
#line 369 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
    }
#line 1959 "beancount/parser/grammar.c"
    break;

  case 21: /* txn_strings: txn_strings PIPE  */
#line 374 "beancount/parser/grammar.y"
    {
        BUILDY(,
               (yyval.pyobj), "pipe_deprecated_error", "");
        (yyval.pyobj) = (yyvsp[-1].pyobj);
    }
#line 1969 "beancount/parser/grammar.c"
    break;

  case 22: /* tags_links: %empty  */
#line 382 "beancount/parser/grammar.y"
    {
        BUILDY(,
               (yyval.pyobj), "tag_link_new", "");
    }
#line 1978 "beancount/parser/grammar.c"
    break;

  case 23: /* tags_links: tags_links LINK  */
#line 387 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "tag_link_LINK", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
    }
#line 1987 "beancount/parser/grammar.c"
    break;

  case 24: /* tags_links: tags_links TAG  */
#line 392 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "tag_link_TAG", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
    }
#line 1996 "beancount/parser/grammar.c"
    break;

  case 25: /* transaction: DATE txn txn_strings tags_links eol posting_or_kv_list  */
#line 399 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "transaction", "ObOOO", (yyvsp[-5].pyobj), (yyvsp[-4].character), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
    }
#line 2005 "beancount/parser/grammar.c"
    break;

  case 26: /* optflag: %empty  */
#line 406 "beancount/parser/grammar.y"
    {
        (yyval.character) = '\0';
    }
#line 2013 "beancount/parser/grammar.c"
    break;

  case 27: /* optflag: ASTERISK  */
#line 410 "beancount/parser/grammar.y"
    {
        (yyval.character) = '*';
    }
#line 2021 "beancount/parser/grammar.c"
    break;

  case 28: /* optflag: HASH  */
#line 414 "beancount/parser/grammar.y"
    {
        (yyval.character) = '#';
    }
#line 2029 "beancount/parser/grammar.c"
    break;

  case 32: /* account: ACCOUNT  */
#line 425 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[0].pyobj)),
               (yyval.pyobj), "account", "O", (yyvsp[0].pyobj));
    }
#line 2038 "beancount/parser/grammar.c"
    break;

  case 33: /* posting: INDENT optflag account incomplete_amount cost_spec eol  */
#line 432 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[-1].pyobj)),
               (yyval.pyobj), "posting", "OOOOOb", (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[-1].pyobj), Py_None, Py_False, (yyvsp[-4].character));
    }
#line 2047 "beancount/parser/grammar.c"
    break;

  case 34: /* posting: INDENT optflag account incomplete_amount cost_spec AT price_annotation eol  */
#line 437 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj)),
               (yyval.pyobj), "posting", "OOOOOb", (yyvsp[-5].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj), Py_False, (yyvsp[-6].character));
    }
#line 2056 "beancount/parser/grammar.c"
    break;

  case 35: /* posting: INDENT optflag account incomplete_amount cost_spec ATAT price_annotation eol  */
#line 442 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj)),
               (yyval.pyobj), "posting", "OOOOOb", (yyvsp[-5].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-1].pyobj), Py_True, (yyvsp[-6].character));
    }
#line 2065 "beancount/parser/grammar.c"
    break;

  case 36: /* posting: INDENT optflag account eol  */
#line 447 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-1].pyobj)),
               (yyval.pyobj), "posting", "OOOOOb", (yyvsp[-1].pyobj), MISSING_OBJ, Py_None, Py_None, Py_False, (yyvsp[-2].character));
    }
#line 2074 "beancount/parser/grammar.c"
    break;

  case 37: /* key_value: KEY COLON key_value_value  */
#line 454 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "key_value", "OO", (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
    }
#line 2083 "beancount/parser/grammar.c"
    break;

  case 38: /* key_value_line: INDENT key_value eol  */
#line 461 "beancount/parser/grammar.y"
    {
        (yyval.pyobj) = (yyvsp[-1].pyobj);
    }
#line 2091 "beancount/parser/grammar.c"
    break;

  case 48: /* key_value_value: %empty  */
#line 476 "beancount/parser/grammar.y"
    {
        Py_INCREF(Py_None);
        (yyval.pyobj) = Py_None;
    }
#line 2100 "beancount/parser/grammar.c"
    break;

  case 49: /* posting_or_kv_list: %empty  */
#line 483 "beancount/parser/grammar.y"
    {
        Py_INCREF(Py_None);
        (yyval.pyobj) = Py_None;
    }
#line 2109 "beancount/parser/grammar.c"
    break;

  case 50: /* posting_or_kv_list: posting_or_kv_list INDENT eol  */
#line 488 "beancount/parser/grammar.y"
    {
        (yyval.pyobj) = (yyvsp[-2].pyobj);
    }
#line 2117 "beancount/parser/grammar.c"
    break;

  case 51: /* posting_or_kv_list: posting_or_kv_list INDENT tags_links eol  */
#line 492 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-3].pyobj), (yyvsp[-1].pyobj)),
               (yyval.pyobj), "handle_list", "OO", (yyvsp[-3].pyobj), (yyvsp[-1].pyobj));
    }
#line 2126 "beancount/parser/grammar.c"
    break;

  case 52: /* posting_or_kv_list: posting_or_kv_list key_value_line  */
#line 497 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
    }
#line 2135 "beancount/parser/grammar.c"
    break;

  case 53: /* posting_or_kv_list: posting_or_kv_list posting  */
#line 502 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
    }
#line 2144 "beancount/parser/grammar.c"
    break;

  case 54: /* key_value_list: %empty  */
#line 509 "beancount/parser/grammar.y"
    {
        Py_INCREF(Py_None);
        (yyval.pyobj) = Py_None;
    }
#line 2153 "beancount/parser/grammar.c"
    break;

  case 55: /* key_value_list: key_value_list INDENT eol  */
#line 514 "beancount/parser/grammar.y"
    {
        (yyval.pyobj) = (yyvsp[-2].pyobj);
    }
#line 2161 "beancount/parser/grammar.c"
    break;

  case 56: /* key_value_list: key_value_list key_value_line  */
#line 518 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
    }
#line 2170 "beancount/parser/grammar.c"
    break;

  case 57: /* currency_list: %empty  */
#line 525 "beancount/parser/grammar.y"
    {
        Py_INCREF(Py_None);
        (yyval.pyobj) = Py_None;
    }
#line 2179 "beancount/parser/grammar.c"
    break;

  case 58: /* currency_list: currency  */
#line 530 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[0].pyobj)),
               (yyval.pyobj), "handle_list", "OO", Py_None, (yyvsp[0].pyobj));
    }
#line 2188 "beancount/parser/grammar.c"
    break;

  case 59: /* currency_list: currency_list COMMA currency  */
#line 535 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "handle_list", "OO", (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
    }
#line 2197 "beancount/parser/grammar.c"
    break;

  case 60: /* pushtag: PUSHTAG TAG eol  */
#line 542 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-1].pyobj)),
               (yyval.pyobj), "pushtag", "O", (yyvsp[-1].pyobj));
    }
#line 2206 "beancount/parser/grammar.c"
    break;

  case 61: /* poptag: POPTAG TAG eol  */
#line 549 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-1].pyobj)),
               (yyval.pyobj), "poptag", "O", (yyvsp[-1].pyobj));
    }
#line 2215 "beancount/parser/grammar.c"
    break;

  case 62: /* pushmeta: PUSHMETA key_value eol  */
#line 556 "beancount/parser/grammar.y"
    {
        /* Note: key_value is a tuple, Py_BuildValue() won't wrap it up
         * within a tuple, so expand in the method (it receives two
         * objects). See https://docs.python.org/3.4/c-api/arg.html. */
        BUILDY(DECREF((yyvsp[-1].pyobj)),
               (yyval.pyobj), "pushmeta", "O", (yyvsp[-1].pyobj));
    }
#line 2227 "beancount/parser/grammar.c"
    break;

  case 63: /* popmeta: POPMETA KEY COLON eol  */
#line 566 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-2].pyobj)),
               (yyval.pyobj), "popmeta", "O", (yyvsp[-2].pyobj));
    }
#line 2236 "beancount/parser/grammar.c"
    break;

  case 64: /* open: DATE OPEN account currency_list opt_booking eol key_value_list  */
#line 573 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "open", "OOOOO", (yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
    }
#line 2245 "beancount/parser/grammar.c"
    break;

  case 66: /* opt_booking: %empty  */
#line 581 "beancount/parser/grammar.y"
    {
        Py_INCREF(Py_None);
        (yyval.pyobj) = Py_None;
    }
#line 2254 "beancount/parser/grammar.c"
    break;

  case 67: /* close: DATE CLOSE account eol key_value_list  */
#line 588 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "close", "OOO", (yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
    }
#line 2263 "beancount/parser/grammar.c"
    break;

  case 68: /* commodity: DATE COMMODITY currency eol key_value_list  */
#line 595 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "commodity", "OOO", (yyvsp[-4].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
    }
#line 2272 "beancount/parser/grammar.c"
    break;

  case 69: /* pad: DATE PAD account account eol key_value_list  */
#line 602 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "pad", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
    }
#line 2281 "beancount/parser/grammar.c"
    break;

  case 70: /* balance: DATE BALANCE account amount_tolerance eol key_value_list  */
#line 609 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[0].pyobj), (yyvsp[-2].pairobj).pyobj1, (yyvsp[-2].pairobj).pyobj2),
               (yyval.pyobj), "balance", "OOOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pairobj).pyobj1, (yyvsp[-2].pairobj).pyobj2, (yyvsp[0].pyobj));
    }
#line 2290 "beancount/parser/grammar.c"
    break;

  case 71: /* amount: number_expr currency  */
#line 616 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "amount", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
    }
#line 2299 "beancount/parser/grammar.c"
    break;

  case 72: /* amount_tolerance: number_expr currency  */
#line 623 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
               (yyval.pairobj).pyobj1, "amount", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
        (yyval.pairobj).pyobj2 = Py_None;
        Py_INCREF(Py_None);
    }
#line 2310 "beancount/parser/grammar.c"
    break;

  case 73: /* amount_tolerance: number_expr TILDE number_expr currency  */
#line 630 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-3].pyobj), (yyvsp[0].pyobj)),
               (yyval.pairobj).pyobj1, "amount", "OO", (yyvsp[-3].pyobj), (yyvsp[0].pyobj));
        (yyval.pairobj).pyobj2 = (yyvsp[-1].pyobj);
    }
#line 2320 "beancount/parser/grammar.c"
    break;

  case 75: /* maybe_number: %empty  */
#line 639 "beancount/parser/grammar.y"
    {
        Py_INCREF(MISSING_OBJ);
        (yyval.pyobj) = MISSING_OBJ;
    }
#line 2329 "beancount/parser/grammar.c"
    break;

  case 77: /* maybe_currency: %empty  */
#line 647 "beancount/parser/grammar.y"
    {
        Py_INCREF(MISSING_OBJ);
        (yyval.pyobj) = MISSING_OBJ;
    }
#line 2338 "beancount/parser/grammar.c"
    break;

  case 78: /* compound_amount: maybe_number currency  */
#line 654 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "compound_amount", "OOO", (yyvsp[-1].pyobj), Py_None, (yyvsp[0].pyobj));
    }
#line 2347 "beancount/parser/grammar.c"
    break;

  case 79: /* compound_amount: number_expr maybe_currency  */
#line 659 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "compound_amount", "OOO", (yyvsp[-1].pyobj), Py_None, (yyvsp[0].pyobj));
    }
#line 2356 "beancount/parser/grammar.c"
    break;

  case 80: /* compound_amount: maybe_number HASH maybe_number currency  */
#line 664 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-3].pyobj), (yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "compound_amount", "OOO", (yyvsp[-3].pyobj), (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
        ;
    }
#line 2366 "beancount/parser/grammar.c"
    break;

  case 81: /* incomplete_amount: maybe_number maybe_currency  */
#line 672 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "amount", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
    }
#line 2375 "beancount/parser/grammar.c"
    break;

  case 82: /* cost_spec: LCURL cost_comp_list RCURL  */
#line 679 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-1].pyobj)),
               (yyval.pyobj), "cost_spec", "OO", (yyvsp[-1].pyobj), Py_False);
    }
#line 2384 "beancount/parser/grammar.c"
    break;

  case 83: /* cost_spec: LCURLCURL cost_comp_list RCURLCURL  */
#line 684 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-1].pyobj)),
               (yyval.pyobj), "cost_spec", "OO", (yyvsp[-1].pyobj), Py_True);
    }
#line 2393 "beancount/parser/grammar.c"
    break;

  case 84: /* cost_spec: %empty  */
#line 689 "beancount/parser/grammar.y"
    {
        Py_INCREF(Py_None);
        (yyval.pyobj) = Py_None;
    }
#line 2402 "beancount/parser/grammar.c"
    break;

  case 85: /* cost_comp_list: %empty  */
#line 696 "beancount/parser/grammar.y"
    {
        /* We indicate that there was a cost if there */
        (yyval.pyobj) = PyList_New(0);
    }
#line 2411 "beancount/parser/grammar.c"
    break;

  case 86: /* cost_comp_list: cost_comp  */
#line 701 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[0].pyobj)),
               (yyval.pyobj), "handle_list", "OO", Py_None, (yyvsp[0].pyobj));
    }
#line 2420 "beancount/parser/grammar.c"
    break;

  case 87: /* cost_comp_list: cost_comp_list COMMA cost_comp  */
#line 706 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "handle_list", "OO", (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
    }
#line 2429 "beancount/parser/grammar.c"
    break;

  case 91: /* cost_comp: ASTERISK  */
#line 716 "beancount/parser/grammar.y"
    {
        BUILDY(,
               (yyval.pyobj), "cost_merge", "O", Py_None);
    }
#line 2438 "beancount/parser/grammar.c"
    break;

  case 92: /* price: DATE PRICE currency amount eol key_value_list  */
#line 723 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "price", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
    }
#line 2447 "beancount/parser/grammar.c"
    break;

  case 93: /* event: DATE EVENT STRING STRING eol key_value_list  */
#line 730 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "event", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
    }
#line 2456 "beancount/parser/grammar.c"
    break;

  case 94: /* query: DATE QUERY STRING STRING eol key_value_list  */
#line 737 "beancount/parser/grammar.y"
       {
           BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
                  (yyval.pyobj), "query", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
       }
#line 2465 "beancount/parser/grammar.c"
    break;

  case 95: /* note: DATE NOTE account STRING tags_links eol key_value_list  */
#line 744 "beancount/parser/grammar.y"
    {
      BUILDY(DECREF((yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "note", "OOOOO", (yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
    }
#line 2474 "beancount/parser/grammar.c"
    break;

  case 97: /* document: DATE DOCUMENT account filename tags_links eol key_value_list  */
#line 753 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "document", "OOOOO", (yyvsp[-6].pyobj), (yyvsp[-4].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
    }
#line 2483 "beancount/parser/grammar.c"
    break;

  case 98: /* custom_value: STRING  */
#line 760 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[0].pyobj)),
               (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
    }
#line 2492 "beancount/parser/grammar.c"
    break;

  case 99: /* custom_value: DATE  */
#line 765 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[0].pyobj)),
               (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
    }
#line 2501 "beancount/parser/grammar.c"
    break;

  case 100: /* custom_value: BOOL  */
#line 770 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[0].pyobj)),
               (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
    }
#line 2510 "beancount/parser/grammar.c"
    break;

  case 101: /* custom_value: amount  */
#line 775 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[0].pyobj)),
               (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
    }
#line 2519 "beancount/parser/grammar.c"
    break;

  case 102: /* custom_value: number_expr  */
#line 780 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[0].pyobj)),
               (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), Py_None);
    }
#line 2528 "beancount/parser/grammar.c"
    break;

  case 103: /* custom_value: account  */
#line 785 "beancount/parser/grammar.y"
    {
        /* Obtain beancount.core.account.TYPE */
        PyObject* module = PyImport_ImportModule("beancount.core.account");
        PyObject* dtype = PyObject_GetAttrString(module, "TYPE");
        Py_DECREF(module);
        BUILDY(DECREF((yyvsp[0].pyobj), dtype),
               (yyval.pyobj), "custom_value", "OO", (yyvsp[0].pyobj), dtype);
    }
#line 2541 "beancount/parser/grammar.c"
    break;

  case 104: /* custom_value_list: %empty  */
#line 796 "beancount/parser/grammar.y"
    {
        Py_INCREF(Py_None);
        (yyval.pyobj) = Py_None;
    }
#line 2550 "beancount/parser/grammar.c"
    break;

  case 105: /* custom_value_list: custom_value_list custom_value  */
#line 801 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
    }
#line 2559 "beancount/parser/grammar.c"
    break;

  case 106: /* custom: DATE CUSTOM STRING custom_value_list eol key_value_list  */
#line 808 "beancount/parser/grammar.y"
   {
       BUILDY(DECREF((yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj)),
              (yyval.pyobj), "custom", "OOOO", (yyvsp[-5].pyobj), (yyvsp[-3].pyobj), (yyvsp[-2].pyobj), (yyvsp[0].pyobj));
   }
#line 2568 "beancount/parser/grammar.c"
    break;

  case 119: /* option: OPTION STRING STRING eol  */
#line 829 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-2].pyobj), (yyvsp[-1].pyobj)),
               (yyval.pyobj), "option", "OO", (yyvsp[-2].pyobj), (yyvsp[-1].pyobj));
    }
#line 2577 "beancount/parser/grammar.c"
    break;

  case 120: /* include: INCLUDE STRING eol  */
#line 836 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-1].pyobj)),
               (yyval.pyobj), "include", "O", (yyvsp[-1].pyobj));
    }
#line 2586 "beancount/parser/grammar.c"
    break;

  case 121: /* plugin: PLUGIN STRING eol  */
#line 843 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-1].pyobj)),
               (yyval.pyobj), "plugin", "OO", (yyvsp[-1].pyobj), Py_None);
    }
#line 2595 "beancount/parser/grammar.c"
    break;

  case 122: /* plugin: PLUGIN STRING STRING eol  */
#line 848 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-2].pyobj), (yyvsp[-1].pyobj)),
               (yyval.pyobj), "plugin", "OO", (yyvsp[-2].pyobj), (yyvsp[-1].pyobj));
    }
#line 2604 "beancount/parser/grammar.c"
    break;

  case 132: /* declarations: declarations entry  */
#line 866 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-1].pyobj), (yyvsp[0].pyobj)),
               (yyval.pyobj), "handle_list", "OO", (yyvsp[-1].pyobj), (yyvsp[0].pyobj));
    }
#line 2613 "beancount/parser/grammar.c"
    break;

  case 133: /* declarations: declarations error  */
#line 871 "beancount/parser/grammar.y"
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
#line 2635 "beancount/parser/grammar.c"
    break;

  case 134: /* declarations: %empty  */
#line 889 "beancount/parser/grammar.y"
    {
         Py_INCREF(Py_None);
         (yyval.pyobj) = Py_None;
    }
#line 2644 "beancount/parser/grammar.c"
    break;

  case 135: /* file: declarations $end  */
#line 897 "beancount/parser/grammar.y"
    {
        BUILDY(DECREF((yyvsp[-1].pyobj)),
               (yyval.pyobj), "store_result", "O", (yyvsp[-1].pyobj));
    }
#line 2653 "beancount/parser/grammar.c"
    break;


#line 2657 "beancount/parser/grammar.c"

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
          YYNOMEM;
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
  ++yynerrs;

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
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (&yylloc, scanner, builder, YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
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

#line 905 "beancount/parser/grammar.y"


/* Get a printable version of a token name. */
const char* token_to_string(int token)
{
  return yytname[YYTRANSLATE(token)];
}

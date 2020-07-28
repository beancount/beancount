/* A Bison parser, made by GNU Bison 3.6.4.  */

/* Bison interface for Yacc-like parsers in C

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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

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


#line 89 "beancount/parser/grammar.h"

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
#line 144 "beancount/parser/grammar.y"

    char character;
    const char* string;
    PyObject* pyobj;
    struct {
        PyObject* pyobj1;
        PyObject* pyobj2;
    } pairobj;

#line 169 "beancount/parser/grammar.h"

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

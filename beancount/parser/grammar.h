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
#line 146 "beancount/parser/grammar.y"

    char character;
    const char* string;
    PyObject* pyobj;
    struct {
        PyObject* pyobj1;
        PyObject* pyobj2;
    } pairobj;

#line 170 "beancount/parser/grammar.h"

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

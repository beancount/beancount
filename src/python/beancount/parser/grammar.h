/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

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




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 62 "src/python/beancount/parser/grammar.y"
{
    char character;
    const char* string;
    PyObject* pyobj;
}
/* Line 1529 of yacc.c.  */
#line 129 "src/python/beancount/parser/grammar.h"
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



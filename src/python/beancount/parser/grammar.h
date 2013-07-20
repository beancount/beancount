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




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 68 "src/python/beancount/parser/grammar.y"
{
    char character;
    const char* string;
    PyObject* pyobj;
}
/* Line 1529 of yacc.c.  */
#line 125 "src/python/beancount/parser/grammar.h"
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



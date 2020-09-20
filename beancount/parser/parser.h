#ifndef BEANCOUNT_PARSER_H
#define BEANCOUNT_PARSER_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

/* This typedef is included in the Flex generated lexer.h header, but
 * it is needed in the Bison generated header grammar.h that needs to
 * be included before lexer.h. Repeat it here and make sure to include
 * the headers in the right order. */
typedef void* yyscan_t;

/* Lexer interface required by Bison. */
#define YY_DECL int yylex(YYSTYPE* yylval_param, YYLTYPE* yylloc_param, \
                          yyscan_t yyscanner, PyObject* builder)

/* A global constant pointing to the special 'MISSING' object. */
extern PyObject* missing_obj;

#endif /* BEANCOUNT_PARSER_H */

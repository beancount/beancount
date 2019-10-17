#ifndef BEANCOUNT_BUILDER_H
#define BEANCOUNT_BUILDER_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

/* This typedef is included in the Flex generated lexer.h header, but
 * it is needed in the Bison generted header grammar.h that needs to
 * be included before lexer.h. Repeat it here and make sure to include
 * the headers in the right order. */
typedef void* yyscan_t;

#define YY_DECL int yylex (YYSTYPE * yylval_param, YYLTYPE * yylloc_param, \
                           yyscan_t yyscanner, PyObject* parser, PyObject* builder)

typedef struct {
    PyObject_HEAD
    yyscan_t scanner;
    /* The filename being tokenized. */
    PyObject* filename;
    /* Reporting line offset. This is used like the #line cpp macro */
    int line;
    /* The encoding to use for converting strings. */
    const char* encoding;
    PyObject* builder;
} Parser;

extern PyObject* missing;

#endif /* BEANCOUNT_BUILDER_H */

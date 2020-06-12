#ifndef BEANCOUNT_BUILDER_H
#define BEANCOUNT_BUILDER_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

extern PyObject* builder;
extern PyObject* missing_obj;

/* This typedef is included in the Flex generated lexer.h header, but
 * it is needed in the Bison generted header grammar.h that needs to
 * be included before lexer.h. Repeat it here and make sure to include
 * the headers in the right order. */
typedef void* yyscan_t;

#define YY_DECL int yylex (YYSTYPE * yylval_param, YYLTYPE * yylloc_param, yyscan_t yyscanner)

/* #define DO_TRACE_ERRORS   1 */

/* Error tracing (use for debugging error handling). */
#ifdef DO_TRACE_ERRORS
#  define TRACE_ERROR(...)                              \
    {                                                   \
        fprintf(stdout, "\n");                          \
        fprintf(stdout, "%s:%d: TRACE - In function '%s':\n",   \
                __FILE__, __LINE__, __func__);          \
        fprintf(stdout, __VA_ARGS__);                   \
        fprintf(stdout, "\n");                          \
        fflush(stdout);                                 \
    }
#else
#  define TRACE_ERROR(...)
#endif


#endif /* BEANCOUNT_BUILDER_H */

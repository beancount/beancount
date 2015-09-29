#ifndef BEANCOUNT_BUILDER_H
#define BEANCOUNT_BUILDER_H

#include <Python.h>


extern PyObject* builder;
extern PyObject* missing_obj;

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

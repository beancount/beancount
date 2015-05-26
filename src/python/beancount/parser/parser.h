#ifndef BEANCOUNT_BUILDER_H
#define BEANCOUNT_BUILDER_H

#include <Python.h>


extern PyObject* builder;

PyObject* checkNull(PyObject* o);


#define BUILD(method_name, format, ...)                                 \
    checkNull( PyObject_CallMethod(builder, method_name, format, __VA_ARGS__) );

#define BUILD_NOARGS(method_name)                                       \
    checkNull( PyObject_CallMethod(builder, method_name, NULL) );


// Error tracing (use for debugging error handling).
#define DO_TRACE_ERRORS 1
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


#endif

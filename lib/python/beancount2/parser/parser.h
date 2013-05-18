#ifndef BEANCOUNT_BUILDER_H
#define BEANCOUNT_BUILDER_H

#include <Python.h>


extern PyObject* builder;

PyObject* checkNull(PyObject* o);

#define BUILD(method_name, format, ...)                                                 \
    checkNull( PyObject_CallMethod(builder, method_name, format, __VA_ARGS__) );


#endif

#ifndef BEANCOUNT_BUILDER_H
#define BEANCOUNT_BUILDER_H

#include <Python.h>


extern PyObject* builder;
PyObject* build(PyObject* builder, const char* method_name, const char* string);


#endif

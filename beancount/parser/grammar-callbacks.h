#ifndef BEANCOUNT_PARSER_GRAMMAR_CALLBACKS_H
#define BEANCOUNT_PARSER_GRAMMAR_CALLBACKS_H

#define PY_SSIZE_T_CLEAN
#include <Python.h>

PyObject* beancount_call_builder_method(
    const char* method_name,
    PyObject* builder,
    PyObject* args
);

int beancount_initialize_grammar(void);

#endif /* BEANCOUNT_PARSER_GRAMMAR_CALLBACKS_H */

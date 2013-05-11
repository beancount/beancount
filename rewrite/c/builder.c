#include <Python.h>
#include <moduleobject.h>
/* #include <datetime.h> */
/* #include <string.h> */

#include "builder.h"
#include "beancount_lexer.h"
#include "beancount_parser.h"

/* The bison header file does not contain this... silly. */
int yyparse(void);



/* The current builder during parsing (as a global variable for now). */
PyObject* builder = 0;

/* This function gets invoked by the parser to call methods on its builder.*/
PyObject* build(PyObject* builder, const char* method_name, const char* string)
{
    return PyObject_CallMethod(builder, (char*)method_name, "s", string);
}



PyDoc_STRVAR(parse_doc,
"Parse the filename, calling back methods on the builder.\
Your builder is responsible to accumulating results.");

PyObject* parse(PyObject *self, PyObject *args)
{
    const char* filename = 0;

    /* Unpack and validate arguments */
    if ( !PyArg_ParseTuple(args, "sO", &filename, &builder) ) {
        return NULL;
    }

    /* Open the file. */
    FILE* fp = fopen(filename, "r");

    /* Initialize the parser. */
    yyin = fp;
    /* yydebug = 1; */

    /* Parse! This will call back methods on the builder instance. */
    yyparse();

    /* Finalize the parser. */
    if ( fp != NULL ) {
        fclose(fp);
    }
    yylex_destroy();
    builder = 0;

    /* Return the result as a float */
    Py_RETURN_NONE;
}



static PyMethodDef module_functions[] = {
    {"parse", parse, METH_VARARGS, parse_doc},
    {NULL, NULL, 0, NULL} /* Sentinel */
};

static struct PyModuleDef moduledef = {
    PyModuleDef_HEAD_INIT,
    "_beancount",                         /* m_name */
    "Beancount parser extension module.", /* m_doc */
    -1,                                   /* m_size */
    module_functions,                     /* m_methods */
    NULL,                                 /* m_reload */
    NULL,                                 /* m_traverse */
    NULL,                                 /* m_clear */
    NULL,                                 /* m_free */
};

PyMODINIT_FUNC PyInit__beancount(void)
{
    PyObject* m = PyModule_Create(&moduledef);
    if ( m == NULL )
        Py_RETURN_NONE;

    return m;
}


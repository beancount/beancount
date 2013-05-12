/* A Python extension module that invokes the parser defined in lexer.l and
 * grammar.y. */

#include <Python.h>
#include <moduleobject.h>
#include <ctype.h>

#include "parser.h"
#include "lexer.h"
#include "parser.h"


/* The bison header file does not contain this... silly. */
extern int yyparse(void);

extern const char* getTokenName(int token);


/* The current builder during parsing (as a global variable for now). */
PyObject* builder = 0;



/* FIXME: remove */
#if 0
/* This function gets invoked by the parser to call methods on its builder.*/
PyObject* build(char* rule_name, char* format, ...)
{
    va_list va;
    PyObject* result;

    char method_name[128];
    strcpy(method_name, "parse_");
    strcpy(method_name+6, rule_name);

    /* Note: we use the global variable 'builder' here. Revise this in the
     * future. */
    va_start(va, format);
    result = PyObject_CallMethod(builder, method_name, format, va);
    va_end(va);

    return result;
}
#endif


/* Check if the object is null; if so, report an error. This is used internally
   only, to debug the parser rules, and should never trigger in production. */
PyObject* checkNull(PyObject* o)
{
    if ( o == NULL ) {
        PyErr_Print();
        abort();
    }
    return o;
}



PyDoc_STRVAR(parse_doc,
"Parse the filename, calling back methods on the builder.\
Your builder is responsible to accumulating results.");

PyObject* parse(PyObject *self, PyObject *args)
{
    /* Unpack and validate arguments */
    const char* filename = 0;
    if ( !PyArg_ParseTuple(args, "sO", &filename, &builder) ) {
        return NULL;
    }
    Py_XINCREF(builder);

    /* Open the file. */
    FILE* fp = fopen(filename, "r");
    if ( fp == NULL ) {
        return PyErr_Format(PyExc_IOError, "Cannot open file '%s'.", filename);
    }

    /* Initialize the parser. */
    yyin = fp;
    /* yydebug = 1; */

    /* Parse! This will call back methods on the builder instance. */
    int result = yyparse();

    /* Finalize the parser. */
    if ( fp != NULL ) {
        fclose(fp);
    }
    yylex_destroy();
    Py_XDECREF(builder);
    builder = 0;

    /* Check for parsing errors. */
    if ( result != 0 ) {
        return PyErr_Format(PyExc_RuntimeError, "Parsing error.");
    }

#if 0
    yylval->pyobj;
#endif
    Py_RETURN_NONE;
}

/* FIXME: Make a version of this which takes a string as input! For
 * unit-testing... */



/* Iniitalize the lexer to start running in debug mode. */
PyObject* lexer_init(PyObject *self, PyObject *args)
{
    /* Unpack and validate arguments */
    const char* filename = 0;
    if ( !PyArg_ParseTuple(args, "sO", &filename, &builder) ) {
        return NULL;
    }
    Py_XINCREF(builder);

    /* Open the file. */
    FILE* fp = fopen(filename, "r");
    if ( fp == NULL ) {
        return PyErr_Format(PyExc_IOError, "Cannot open file '%s'.", filename);
    }

    /* Initialize the parser. */
    yyin = fp;

    Py_RETURN_NONE;
}

/* Get the next token; return None if complete. */
PyObject* lexer_next(PyObject *self, PyObject *args)
{
    /* Run the lexer. */
    YYSTYPE yylval;
    YYLTYPE yylloc;
    int token = yylex(&yylval, &yylloc);
    if ( token == 0 ) {
        yylex_destroy();
        Py_RETURN_NONE;
    }

    const char* tokenName = getTokenName(token);
    return Py_BuildValue("(ssi)", tokenName, yytext, yylloc.first_line);
}



static PyMethodDef module_functions[] = {
    {"parse", parse, METH_VARARGS, parse_doc},
    {"lexer_init", lexer_init, METH_VARARGS, NULL},
    {"lexer_next", lexer_next, METH_VARARGS, NULL},
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

PyMODINIT_FUNC PyInit__parser(void)
{
    PyObject* m = PyModule_Create(&moduledef);
    if ( m == NULL )
        Py_RETURN_NONE;

    return m;
}

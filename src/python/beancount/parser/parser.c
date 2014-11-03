/* A Python extension module that invokes the parser defined in lexer.l and
 * grammar.y. */

#include <Python.h>
#include <moduleobject.h>
#include <ctype.h>

#include "parser.h"
#include "lexer.h"


/* The bison header file does not contain this... silly. */
extern int yyparse(void);

extern const char* getTokenName(int token);

extern int yy_firstline;


/* The current builder during parsing (as a global variable for now). */
PyObject* builder = 0;


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



PyDoc_STRVAR(parse_file_doc,
"Parse the filename, calling back methods on the builder.\n\
Your builder is responsible to accumulating results.\n\
If you pass in '-' for filename, stdin is parsed.");

PyDoc_STRVAR(parse_string_doc,
"Parse the given string, calling back methods on the builder.\n\
Your builder is responsible to accumulating results.");


PyObject* parse_file(PyObject *self, PyObject *args, PyObject* kwds)
{
    FILE* fp = NULL;
    int result;

    /* Unpack and validate arguments */
    const char* filename = 0;
    const char* report_filename = 0;
    int report_firstline = 0;
    extern int yydebug;
    static char *kwlist[] = {"filename", "builder",
                             "report_filename", "report_firstline",
                             "yydebug", NULL};
    if ( !PyArg_ParseTupleAndKeywords(args, kwds, "sO|sip", kwlist,
                                      &filename, &builder,
                                      &report_filename, &report_firstline,
                                      &yydebug) ) {
        return NULL;
    }
    Py_XINCREF(builder);

    /* Open the file. */
    if ( strcmp(filename, "-") == 0 ) {
      fp = stdin;
    }
    else {
      fp = fopen(filename, "r");
      if ( fp == NULL ) {
        return PyErr_Format(PyExc_IOError, "Cannot open file '%s'", filename);
      }
    }

    /* Initialize the lexer. */
    /* Note: This needs become part of yylex_init(). See 2.5.37.*/
    yy_eof_times = 0;

    /* Initialize the parser. */
    yyin = fp;
    if ( report_filename != 0 ) {
        yy_filename = report_filename;
    }
    else {
        yy_filename = filename;
    }
    yy_firstline = report_firstline;

    /* Parse! This will call back methods on the builder instance. */
    result = yyparse();

    /* Finalize the parser. */
    if ( fp != NULL ) {
        fclose(fp);
    }
    yylex_destroy();
    Py_XDECREF(builder);
    builder = 0;
    yy_filename = 0;

    /* Check for parsing errors. */
    if ( result != 0 ) {
        return PyErr_Format(PyExc_RuntimeError, "Parsing error");
    }

    Py_RETURN_NONE;
}

PyObject* parse_string(PyObject *self, PyObject *args, PyObject* kwds)
{
    int result;

    /* Unpack and validate arguments */
    const char* input_string = 0;
    const char* report_filename = 0;
    int report_firstline = 0;
    extern int yydebug;
    static char *kwlist[] = {"input_string", "builder",
                             "report_filename", "report_firstline",
                             "yydebug", NULL};
    if ( !PyArg_ParseTupleAndKeywords(args, kwds, "sO|sip", kwlist,
                                      &input_string, &builder,
                                      &report_filename, &report_firstline,
                                      &yydebug) ) {
        return NULL;
    }
    Py_XINCREF(builder);

    yy_switch_to_buffer(yy_scan_string(input_string));

    /* Initialize the lexer. */
    /* Note: This needs become part of yylex_init(). See 2.5.37.*/
    yy_eof_times = 0;

    /* Initialize the parser. */
    if ( report_filename != 0 ) {
        yy_filename = report_filename;
    }
    else {
        yy_filename = "<string>";
    }
    yy_firstline = report_firstline;

    /* Parse! This will call back methods on the builder instance. */
    result = yyparse();

    /* Finalize the parser. */
    yylex_destroy();
    Py_XDECREF(builder);
    builder = 0;
    yy_filename = 0;

    /* Check for parsing errors. */
    if ( result != 0 ) {
        return PyErr_Format(PyExc_RuntimeError, "Parsing error");
    }

    Py_RETURN_NONE;
}

PyObject* get_yyfilename(PyObject *self, PyObject *args)
{
  return PyUnicode_FromString(yy_filename);
}

PyObject* get_yylineno(PyObject *self, PyObject *args)
{
  return PyLong_FromLong(yylineno);
}



/* Inititalize the lexer to start running in debug mode. */
PyObject* lexer_init(PyObject *self, PyObject *args)
{
    FILE* fp = NULL;

    /* Unpack and validate arguments */
    const char* filename = 0;
    if ( !PyArg_ParseTuple(args, "sO", &filename, &builder) ) {
        return NULL;
    }
    Py_XINCREF(builder);

    /* Open the file. */
    fp = fopen(filename, "r");
    if ( fp == NULL ) {
        return PyErr_Format(PyExc_IOError, "Cannot open file '%s'", filename);
    }

    /* Initialize the lexer. */
    /* Note: This needs become part of yylex_init(). See 2.5.37.*/
    yy_eof_times = 0;

    /* Initialize the parser. */
    yyin = fp;
    yy_filename = filename;
    yy_eof_times = 0;

    Py_RETURN_NONE;
}

/* Get the next token; return None if complete. */
PyObject* lexer_next(PyObject *self, PyObject *args)
{
    const char* tokenName = NULL;
    YYSTYPE yylval;
    YYLTYPE yylloc;
    int token;
    PyObject* obj;

    /* Run the lexer. */
    token = yylex(&yylval, &yylloc);
    if ( token == 0 ) {
        yylex_destroy();
        Py_RETURN_NONE;
    }

    obj = Py_None;
    if (token == DATE ||
        token == ACCOUNT ||
        token == CURRENCY ||
        token == STRING ||
        token == NUMBER ||
        token == TAG ||
        token == LINK) {

        obj = yylval.pyobj;
    }

    tokenName = getTokenName(token);
    return Py_BuildValue("(sis#O)", tokenName, yylloc.first_line, yytext, yyleng, obj);
}


static PyMethodDef module_functions[] = {
    {"parse_file", (PyCFunction)parse_file, METH_VARARGS|METH_KEYWORDS, parse_file_doc},
    {"parse_string", (PyCFunction)parse_string, METH_VARARGS|METH_KEYWORDS, parse_string_doc},
    {"get_yyfilename", (PyCFunction)get_yyfilename, METH_VARARGS, NULL},
    {"get_yylineno", (PyCFunction)get_yylineno, METH_VARARGS, NULL},
    {"lexer_init", lexer_init, METH_VARARGS, NULL},
    {"lexer_next", lexer_next, METH_VARARGS, NULL},
    {NULL, NULL, 0, NULL} /* Sentinel */
};

static struct PyModuleDef moduledef = {
    PyModuleDef_HEAD_INIT,
    "_parser",                            /* m_name */
    "Beancount parser extension module",  /* m_doc */
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

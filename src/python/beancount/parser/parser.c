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

/* A reference to a Python-defined constant object used as a placeholder for
   missing cost specifications. */
PyObject* missing_obj = 0;


PyDoc_STRVAR(parse_file_doc,
"Parse the filename, calling back methods on the builder.\n\
Your builder is responsible to accumulating results.\n\
If you pass in '-' for filename, stdin is parsed.");

PyDoc_STRVAR(parse_string_doc,
"Parse the given string, calling back methods on the builder.\n\
Your builder is responsible to accumulating results.");


/* Handle the result of yyparse() {459018e2905c}. */
PyObject* handle_yyparse_result(int result)
{
    /* Check for internal errors during parsing (which would be the result of
     * calling YYABORT, which we don't call), and this should raise an
     * exception. */
    if ( result == 1 ) {
        return PyErr_Format(PyExc_RuntimeError, "Parser aborted (internal error)");
    }
    else if ( result == 2 ) {
        return PyErr_Format(PyExc_MemoryError, "Parser ran out of memory");
    }
    assert(result == 0);
    Py_RETURN_NONE;
}

PyObject* parse_file(PyObject *self, PyObject *args, PyObject* kwds)
{
    FILE* fp = NULL;
    int result;

    /* Unpack and validate arguments */
    const char* filename = 0;
    const char* report_filename = 0;
    int report_firstline = 0;
    extern int yydebug;
    const char* encoding = 0;
    static char *kwlist[] = {"filename", "builder",
                             "report_filename", "report_firstline",
                             "encoding", "yydebug", NULL};
    if ( !PyArg_ParseTupleAndKeywords(args, kwds, "sO|sizp", kwlist,
                                      &filename, &builder,
                                      &report_filename, &report_firstline,
                                      &encoding, &yydebug) ) {
        return NULL;
    }

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
    yylex_initialize(report_filename != NULL ? report_filename : filename,
                     encoding);
    yyin = fp;

    /* Initialize the parser. */
    yy_firstline = report_firstline;

    /* Parse! This will call back methods on the builder instance. */
    result = yyparse();

    /* Finalize the parser. */
    /* Noop. */

    /* Finalize the lexer. */
    if ( fp != NULL ) {
        fclose(fp);
    }
    yylex_finalize();

    builder = 0;

    return handle_yyparse_result(result);
}

PyObject* parse_string(PyObject *self, PyObject *args, PyObject* kwds)
{
    int result;

    /* Unpack and validate arguments */
    const char* input_string = 0;
    Py_ssize_t input_length = 0;
    const char* report_filename = 0;
    const char* encoding = 0;
    int report_firstline = 0;
    extern int yydebug;
    static char *kwlist[] = {"input_string", "builder",
                             "report_filename", "report_firstline",
                             "encoding", "yydebug", NULL};
    if ( !PyArg_ParseTupleAndKeywords(args, kwds, "s#O|sizp", kwlist,
                                      &input_string, &input_length, &builder,
                                      &report_filename, &report_firstline,
                                      &encoding, &yydebug) ) {
        return NULL;
    }

    /* Initialize the lexer. */
    yylex_initialize(report_filename != NULL ? report_filename : "<string>",
                     encoding);
    yy_switch_to_buffer(yy_scan_string(input_string));

    /* Initialize the parser. */
    yy_firstline = report_firstline;

    /* Parse! This will call back methods on the builder instance. */
    result = yyparse();

    /* Finalize the parser. */
    /* Noop. */

    /* Finalize the lexer. */
    yylex_finalize();

    builder = 0;

    return handle_yyparse_result(result);
}

PyObject* get_yyfilename(PyObject *self, PyObject *args)
{
    return PyUnicode_FromString(yy_filename);
}

PyObject* get_yylineno(PyObject *self, PyObject *args)
{
    return PyLong_FromLong(yylineno + yy_firstline);
}



/* Inititalize the lexer to start running in debug mode. */
PyObject* lexer_initialize(PyObject *self, PyObject *args)
{
    FILE* fp = NULL;

    /* Unpack and validate arguments */
    const char* filename = NULL;
    const char* encoding = NULL;
    if ( !PyArg_ParseTuple(args, "sOz", &filename, &builder, &encoding) ) {
        return NULL;
    }
    Py_XINCREF(builder);

    /* Open the file. */
    fp = fopen(filename, "r");
    if ( fp == NULL ) {
        return PyErr_Format(PyExc_IOError, "Cannot open file '%s'", filename);
    }

    /* Initialize the lexer. */
    yylex_initialize(filename, encoding);
    yyin = fp;

    Py_RETURN_NONE;
}

/* Inititalize the lexer to start running in debug mode. */
PyObject* lexer_finalize(PyObject *self, PyObject *args)
{
    /* Finalize the lexer. */
    yylex_finalize();

    /* /\* Close the file. *\/ */
    /* if ( fclose(yyin) != 0 ) { */
    /*     return PyErr_Format(PyExc_IOError, "Cannot close lexer file"); */
    /* } */

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
        token == LINK ||
        token == KEY) {

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
    {"lexer_initialize", lexer_initialize, METH_VARARGS, NULL},
    {"lexer_next", lexer_next, METH_VARARGS, NULL},
    {"lexer_finalize", lexer_finalize, METH_VARARGS, NULL},
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
    PyObject* module = PyModule_Create(&moduledef);
    if ( module == NULL ) {
        Py_RETURN_NONE;
    }

    /* Provide the source hash to the parser module for verification that the
     * extension module is up-to-date. */
    /* uint64_t int_source_hash = #PARSER_SOURCE_HASH; */
    PyObject* source_hash = PyUnicode_FromString(PARSER_SOURCE_HASH);
    PyObject_SetAttrString(module, "SOURCE_HASH", source_hash);

    /* Import the module that defines the missing object constant. */
    PyObject* number_module = PyImport_ImportModule("beancount.core.number");
    if ( number_module == NULL ) {
        Py_RETURN_NONE;
    }
    missing_obj = PyObject_GetAttrString(number_module, "MISSING");
    if ( missing_obj == NULL ) {
        Py_RETURN_NONE;
    }

    return module;
}

/* FIXME: Finalize too, unrefing the constants. {48414425cf78} */

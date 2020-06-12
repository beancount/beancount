/* A Python extension module that invokes the parser defined in lexer.l and
 * grammar.y. */

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "parser.h"
#include "lexer.h"

#define XSTRINGIFY(s) STRINGIFY(s)
#define STRINGIFY(s) #s

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
    const char* report_filename = NULL;
    const char* encoding = NULL;
    int report_firstline = 0;
    PyObject* name = NULL;
    extern int yydebug;
    PyObject* file;
    int rv;

    /* Unpack and validate arguments */
    static char* kwlist[] = {"file", "builder",
                             "report_filename", "report_firstline",
                             "encoding", "yydebug", NULL};
    if (! PyArg_ParseTupleAndKeywords(args, kwds, "OO|zizp", kwlist,
                                      &file, &builder,
                                      &report_filename, &report_firstline,
                                      &encoding, &yydebug)) {
        return NULL;
    }

    if (!report_filename) {
        PyObject* p = PyObject_GetAttrString(file, "name");
        if (p) {
            name = PyObject_Str(p);
            if (name)
                report_filename = PyUnicode_AS_DATA(name);
        }
        PyErr_Clear();
        Py_XDECREF(p);
    }

    /* Initialize the lexer. */
    yylex_initialize(report_filename ?: "", encoding);
    yyset_in((void*)file);

    /* Initialize the parser. */
    yy_firstline = report_firstline;

    /* Parse! This will call back methods on the builder instance. */
    rv = yyparse();

    /* Finalize the parser. */
    /* Noop. */

    /* Finalize the lexer. */
    yylex_finalize();

    Py_XDECREF(name);
    builder = NULL;

    return handle_yyparse_result(rv);
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
PyObject* lexer_initialize(PyObject *self, PyObject *args, PyObject *kwds)
{
    const char* report_filename = NULL;
    const char* encoding = NULL;
    int report_firstline = 0;
    PyObject* name = NULL;
    extern int yydebug;
    PyObject* file;

    /* Unpack and validate arguments */
    static char* kwlist[] = {"file", "builder",
                             "report_filename", "report_firstline",
                             "encoding", "yydebug", NULL};
    if (! PyArg_ParseTupleAndKeywords(args, kwds, "OO|zizp", kwlist,
                                      &file, &builder,
                                      &report_filename, &report_firstline,
                                      &encoding, &yydebug)) {
        return NULL;
    }

    if (!report_filename) {
        PyObject* p = PyObject_GetAttrString(file, "name");
        if (p) {
            name = PyObject_Str(p);
            if (name)
                report_filename = PyUnicode_AS_DATA(name);
        }
        PyErr_Clear();
        Py_XDECREF(p);
    }

    /* Initialize the lexer. */
    yylex_initialize(report_filename ?: "", encoding);
    yyset_in((void*)file);

    /* Initialize the parser. */
    yy_firstline = report_firstline;

    /* We need to keep those objects alive after we live this function. */
    Py_INCREF(file);
    Py_INCREF(builder);

    Py_RETURN_NONE;
}

/* Inititalize the lexer to start running in debug mode. */
PyObject* lexer_finalize(PyObject *self, PyObject *args)
{
    /* Now we can let those objects go. */
    Py_XDECREF((void*)yyget_in());
    Py_XDECREF(builder);

    /* Finalize the lexer. */
    yylex_finalize();

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
    return Py_BuildValue("(sis#O)", tokenName, yylloc.first_line, yytext, (Py_ssize_t)yyleng, obj);
}


static PyMethodDef module_functions[] = {
    {"parse_file", (PyCFunction)parse_file, METH_VARARGS|METH_KEYWORDS, parse_file_doc},
    {"get_yyfilename", (PyCFunction)get_yyfilename, METH_VARARGS, NULL},
    {"get_yylineno", (PyCFunction)get_yylineno, METH_VARARGS, NULL},
    {"lexer_initialize", (PyCFunction)lexer_initialize, METH_VARARGS|METH_KEYWORDS, NULL},
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

void initialize_metadata(PyObject* module) {
    /* Provide the source hash to the parser module for verification that the
     * extension module is up-to-date. */
#if _MSC_VER
    static const char* quoted_hash = "" XSTRINGIFY(PARSER_SOURCE_HASH);
#else
    static const char* quoted_hash = XSTRINGIFY(PARSER_SOURCE_HASH);
#endif
    PyObject* source_hash = PyUnicode_FromString(quoted_hash);
    PyObject_SetAttrString(module, "SOURCE_HASH", source_hash);

    /* Provide the release version from the build, as it can be propagated there
     * from setup.py. */
#if _MSC_VER
    static const char* release_version_str = "" XSTRINGIFY(RELEASE_VERSION);
#else
    static const char* release_version_str = XSTRINGIFY(RELEASE_VERSION);
#endif
    PyObject* release_version = PyUnicode_FromString(release_version_str);
    PyObject_SetAttrString(module, "__version__", release_version);

    /* Provide the Mercurial (or Git mirror) changeset from the build. */
#ifdef _MSC_VER
    static const char* vc_changeset_str = "" XSTRINGIFY(VC_CHANGESET);
#else
    static const char* vc_changeset_str = XSTRINGIFY(VC_CHANGESET);
#endif
    PyObject* vc_changeset = PyUnicode_FromString(vc_changeset_str);
    PyObject_SetAttrString(module, "__vc_changeset__", vc_changeset);

    /* Provide the date of the last changeset. */
    static const int vc_timestamp_int = VC_TIMESTAMP;
    PyObject* vc_timestamp = PyLong_FromLong(vc_timestamp_int);
    PyObject_SetAttrString(module, "__vc_timestamp__", vc_timestamp);
}

PyMODINIT_FUNC PyInit__parser(void)
{
    PyObject* module = PyModule_Create(&moduledef);
    if ( module == NULL ) {
        Py_RETURN_NONE;
    }

    initialize_metadata(module);

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

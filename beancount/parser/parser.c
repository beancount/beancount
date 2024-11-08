/* A Python extension module that invokes the parser defined in lexer.l and
 * grammar.y. */

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "beancount/parser/decimal.h"
#include "beancount/parser/macros.h"
#include "beancount/parser/parser.h"
#include "beancount/parser/grammar.h"
#include "beancount/parser/lexer.h"
#include "beancount/parser/tokens.h"

extern YY_DECL;

/* Placeholder object for missing cost specifications. */
PyObject* missing_obj;

typedef struct {
    PyObject_HEAD
    yyscan_t scanner;
    PyObject* builder;
} Parser;

PyDoc_STRVAR(parser_doc,
             "Parser(builder)\n"
             "\n"
             "Class exposing the interface to the Beancout parser C code.\n"
             "\n"
             "Construct a new parser object using the builder Builder object to\n"
             "intantiate and store parsing results. This class is not intended to be\n"
             "used directly. See the beancount.parser moduel instead.");

/* Allocate a new Parser instance. */
static PyObject* parser_new(PyTypeObject* type, PyObject* args, PyObject* kwds)
{
    Parser* self;

    self = (Parser*)type->tp_alloc(type, 0);
    if (!self) {
        return NULL;
    }

    self->scanner = yylex_new();
    if (!self->scanner) {
        Py_XDECREF(self);
        return NULL;
    }

    self->builder = NULL;
    return (PyObject*)self;
}

/* Constructor for Parser instances.
 *
 * Args:
 *   builder: An instance of a Builder object.
*/
static int parser_init(Parser* self, PyObject* args, PyObject* kwds)
{
    static char* kwlist[] = {"builder", "debug", NULL};
    PyObject* builder;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "O|p", kwlist, &builder, &yydebug)) {
        return -1;
    }

    Py_XDECREF(self->builder);
    self->builder = builder;
    Py_INCREF(builder);

    return 0;
}

/* Destructor. */
static void parser_dealloc(Parser* self)
{
    /* Free the builder. */
    Py_XDECREF(self->builder);

    /* Finalize the scanner state. */
    yylex_free(self->scanner);

    Py_TYPE(self)->tp_free((PyObject*)self);
}

PyDoc_STRVAR(parser_parse_doc,
             "parse(file, filename=None, lineno=1)\n"
             "\n"
             "Parse input from file object. The filename and lineno keyword\n"
             "arguments allow to specify the file name and start line number to be\n"
             "used in error reporting and in the returned metadata objects. If\n"
             "filename is not specified or None, the name attribute of the file\n"
             "object is used, if present. Parsing results are retrieved from the \n"
             "Builder object specified when the Parser object was instantiated.");

static PyObject* parser_parse(Parser* self, PyObject* args, PyObject* kwds)
{
    static char* kwlist[] = {"file", "filename", "lineno", NULL};
    PyObject* filename = NULL;
    PyObject* file;
    int lineno = 1;
    int ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "O|Oi", kwlist,
                                     &file, &filename, &lineno)) {
        return NULL;
    }

    /* Initialize the scanner state. */
    yylex_initialize(file, filename, lineno, missing_obj, self->scanner);

    /* Run the parser. */
    ret = yyparse(self->scanner, self->builder);

    /* Signal if an exception has been raised */
    if (PyErr_Occurred()) {
        return NULL;
    }

    /* Check for internal errors during parsing. */
    switch (ret) {
    case 0:
        Py_RETURN_NONE;
    case 1:
        return PyErr_Format(PyExc_RuntimeError, "Parser internal error");
    case 2:
        return PyErr_Format(PyExc_MemoryError, "Parser ran out of memory");
    default:
        return PyErr_Format(PyExc_ValueError, "Unexpected yyparse() return value: %d", ret);
    }
}

PyDoc_STRVAR(parser_lex_doc,
             "lex(file, filename=None, lineno=1)\n"
             "\n"
             "Run the input file object trough the Beancount tokenizer. filename and\n"
             "lineno keyword arguments allow to specify the file name and start line\n"
             "number to be used in error reporting. If filename is not specified or\n"
             "None, the name attribute of the file object is used, if present. Return \n"
             "an iterable yielding (token name, string value, sematical value) tuples.");

static PyObject* parser_lex(Parser* self, PyObject* args, PyObject* kwds)
{
    static char* kwlist[] = {"file", "filename", "lineno", NULL};
    PyObject* filename = NULL;
    PyObject* file;
    int lineno = 1;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "O|Oi", kwlist,
                                     &file, &filename, &lineno)) {
        return NULL;
    }

    /* Initialize the scanner state. */
    yylex_initialize(file, filename, lineno, missing_obj, self->scanner);

    Py_INCREF(self);
    return (PyObject*)self;
}

/* Implement iterator protocol on the Parser. */
static PyObject* parser_iternext(Parser* self)
{
    YYSTYPE yylval;
    YYLTYPE yylloc;
    int token;
    PyObject* obj;

    /* Ensure the scanner has been initialized. */
    if (!yyget_in(self->scanner)) {
        PyErr_SetString(PyExc_ValueError, "Parser not initialized");
        return NULL;
    }

    /* Get one token. */
    token = yylex(&yylval, &yylloc, self->scanner, self->builder);
    if (PyErr_Occurred() || token == 0) {
        return NULL;
    }

    switch (token) {
    case DATE:
    case ACCOUNT:
    case CURRENCY:
    case STRING:
    case NUMBER:
    case TAG:
    case LINK:
    case KEY:
        obj = yylval.pyobj;
        break;
    default:
        obj = Py_None;
    }

    /* Yield a (token name, line, matched string, token value) tuple. */
    return Py_BuildValue("(siy#O)",
                         token_to_string(token),
                         yylloc.first_line,
                         yyget_text(self->scanner),
                         (Py_ssize_t)yyget_leng(self->scanner),
                         obj);
}

static PyMethodDef parser_methods[] = {
    {"parse", (PyCFunction)parser_parse, METH_VARARGS | METH_KEYWORDS, parser_parse_doc},
    {"lex", (PyCFunction)parser_lex, METH_VARARGS | METH_KEYWORDS, parser_lex_doc},
    {NULL, NULL}
};

PyTypeObject Parser_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    "_parser.Parser",                         /* tp_name*/
    sizeof(Parser),                           /* tp_basicsize*/
    0,                                        /* tp_itemsize*/
    (destructor)parser_dealloc,               /* tp_dealloc*/
    0,                                        /* tp_print*/
    0,                                        /* tp_getattr*/
    0,                                        /* tp_setattr*/
    0,                                        /* tp_compare */
    0,                                        /* tp_repr*/
    0,                                        /* tp_as_number */
    0,                                        /* tp_as_sequence */
    0,                                        /* tp_as_mapping */
    0,                                        /* tp_hash */
    0,                                        /* tp_call */
    0,                                        /* tp_str */
    0,                                        /* tp_getattro */
    0,                                        /* tp_setattro */
    0,                                        /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE, /* tp_flags */
    parser_doc,                               /* tp_doc */
    0,                                        /* tp_traverse */
    0,                                        /* tp_clear */
    0,                                        /* tp_richcompare */
    0,                                        /* tp_weaklistoffset */
    PyObject_SelfIter,                        /* tp_iter */
    (iternextfunc)parser_iternext,            /* tp_iternext */
    parser_methods,                           /* tp_methods */
    0,                                        /* tp_members */
    0,                                        /* tp_getset */
    0,                                        /* tp_base */
    0,                                        /* tp_dict */
    0,                                        /* tp_descr_get */
    0,                                        /* tp_descr_set */
    0,                                        /* tp_dictoffset */
    (initproc)parser_init,                    /* tp_init */
    0,                                        /* tp_alloc */
    parser_new,                               /* tp_new */
    0,                                        /* tp_free */
    0,                                        /* tp_is_gc */
    0,                                        /* tp_bases */
    0,                                        /* tp_mro */
    0,                                        /* tp_cache */
    0,                                        /* tp_subclasses */
    0,                                        /* tp_weaklist */
    0,                                        /* tp_del */
    0,                                        /* tp_version_tag */
    0, /* tp_finalize */
};

static PyMethodDef module_functions[] = {
    {NULL, NULL, 0, NULL}
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
    PyObject* beancount_core_number;
    PyObject* module;
    PyObject* value;

    Py_INCREF(&Parser_Type);

    module = PyModule_Create(&moduledef);
    if (!module) {
        goto error;
    }

    initialize_datetime();
    PyDecimal_IMPORT;

    /* Hash of the this Python extension source code. The hash is used
     * to ensure that the imported extension module is an up-to-date
     * build of the corresponding source code. */
    value = PyUnicode_FromString(STRINGIFY(PARSER_SOURCE_HASH));
    PyObject_SetAttrString(module, "SOURCE_HASH", value);

    /* Release versions as defined in setup.py. */
    value = PyUnicode_FromString(STRINGIFY(RELEASE_VERSION));
    PyObject_SetAttrString(module, "__version__", value);

#ifdef VC_CHANGESET
    /* Git changeset from the build source tree.
     * In the Bazel build, this information is absent. */
    value = PyUnicode_FromString(STRINGIFY(VC_CHANGESET));
    PyObject_SetAttrString(module, "__vc_changeset__", value);
#endif

#ifdef VC_TIMESTAMP
    /* Date of the last changeset.
     * In the Bazel build, this information is absent. */
    value = PyLong_FromLong(VC_TIMESTAMP);
    PyObject_SetAttrString(module, "__vc_timestamp__", value);
#endif

#undef SETATTR

    /* Import the module that defines the missing object constant. */
    beancount_core_number = PyImport_ImportModule("beancount.core.number");
    if (!beancount_core_number) {
        goto error;
    }

    missing_obj = PyObject_GetAttrString(beancount_core_number, "MISSING");
    if (!missing_obj) {
        goto error;
    }

    if (PyType_Ready(&Parser_Type) < 0) {
        goto error;
    }
    if (PyModule_AddObject(module, "Parser", (PyObject *)&Parser_Type) < 0) {
        goto error;
    }

    return module;

error:
    Py_XDECREF(&Parser_Type);
    Py_XDECREF(module);
    return NULL;
}

/* TODO(blais): Finalize the module too, releasing the globals. {48414425cf78} */

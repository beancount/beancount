/* A Python extension module that invokes the parser defined in lexer.l and
 * grammar.y. */

#define PY_SSIZE_T_CLEAN
#include <Python.h>

#include "parser.h"
#include "grammar.h"
#include "lexer.h"

extern YY_DECL;

#define XSTRINGIFY(s) STRINGIFY(s)
#define STRINGIFY(s) #s

extern const char* getTokenName(int token);

/* Placeolder object for missing cost specifications. */
PyObject* missing;

PyDoc_STRVAR(parser_doc, "");

static PyObject* parser_new(PyTypeObject* type, PyObject* args, PyObject* kwds)
{
    Parser* self;

    self = (Parser*)type->tp_alloc(type, 0);
    if (!self)
        return NULL;

    self->scanner = yylex_new();
    if (!self->scanner) {
        Py_XDECREF(self);
        return NULL;
    }

    self->builder = NULL;
    return (PyObject*)self;
}

static int parser___init__(Parser* self, PyObject* args, PyObject* kwds)
{
    static char* kwlist[] = {"builder", "debug", NULL};
    PyObject* builder;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "O|p", kwlist, &builder, &yydebug))
        return -1;

    Py_XDECREF(self->builder);
    self->builder = builder;
    Py_INCREF(builder);

    /* The builder need to know about the parser to report parsing
     * location in some circumstances. However storing a reference to
     * the parser in the builder creates a circular reference. We
     * ignore this issue for the moment as it is probably better
     * solved removing the need for the builder to call into the
     * parser. This can be achieved passing the parser location when
     * calling methods of the builder as it is already done in some
     * cases. */
    PyObject_SetAttrString(builder, "parser", (PyObject*)self);

    return 0;
}

static void parser_dealloc(Parser* self)
{
    Py_XDECREF(self->builder);
    yylex_free(self->scanner);
    Py_TYPE(self)->tp_free((PyObject*)self);
}

PyDoc_STRVAR(parser_location_doc, "");

static PyObject* parser_location(Parser* self)
{
    return Py_BuildValue("Oi",
                         yyget_filename(self->scanner),
                         yyget_firstline(self->scanner) + yyget_lineno(self->scanner));
}

static PyGetSetDef parser_getsetters[] = {
    {"location", (getter)parser_location, NULL, parser_location_doc, NULL},
    {NULL}
};

PyDoc_STRVAR(parser_parse_doc, "");

static PyObject* parser_parse(Parser* self, PyObject* args, PyObject* kwds)
{
    static char* kwlist[] = {"file", "filename", "lineno", "encoding", NULL};
    const char* encoding = NULL;
    PyObject* filename = NULL;
    PyObject* file;
    int lineno = 0;
    int ret;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "O|Oiz", kwlist,
                                     &file, &filename, &lineno, &encoding))
        return NULL;

    yylex_initialize(self->scanner, file, filename, lineno, encoding);
    ret = yyparse(self->scanner, self->builder);

    /* Signal if an exception has been raised */
    if (PyErr_Occurred())
        return NULL;

    /* Check for internal errors during parsing. */
    switch (ret) {
    case 0:
        Py_RETURN_NONE;
    case 2:
        return PyErr_NoMemory();
    default:
        return PyErr_Format(PyExc_RuntimeError, "parser internal error: %d", ret);
    }
}

static PyObject* parser_lex(Parser *self, PyObject* args, PyObject* kwds)
{
    static char* kwlist[] = {"file", "filename", "lineno", "encoding", NULL};
    const char* encoding = NULL;
    PyObject* filename = NULL;
    PyObject* file;
    int lineno = 0;

    if (!PyArg_ParseTupleAndKeywords(args, kwds, "O|Oiz", kwlist,
                                     &file, &filename, &lineno, &encoding))
        return NULL;

    yylex_initialize(self->scanner, file, filename, lineno, encoding);

    Py_INCREF(self);
    return (PyObject*)self;
}

static PyObject* parser_iternext(Parser* self)
{
    const char* name;
    YYSTYPE yylval;
    YYLTYPE yylloc;
    int token = 0;
    PyObject* obj;

    if (!yyget_in(self->scanner)) {
        PyErr_SetString(PyExc_ValueError, "Parser is not initialized");
        return NULL;
    }

    token = yylex(&yylval, &yylloc, self->scanner, self->builder);
    if (!token)
        return NULL;

    if (PyErr_Occurred())
        return NULL;

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

    name = getTokenName(token);

    return Py_BuildValue("(sis#O)",
                         name,
                         yylloc.first_line,
                         yyget_text(self->scanner),
                         (Py_ssize_t)yyget_leng(self->scanner),
                         obj);
}

static PyMethodDef parser_methods[] = {
    {"parse", (PyCFunction)parser_parse, METH_VARARGS | METH_KEYWORDS, parser_parse_doc},
    {"lex", (PyCFunction)parser_lex, METH_VARARGS | METH_KEYWORDS, parser_parse_doc},
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
    parser_getsetters,                        /* tp_getset */
    0,                                        /* tp_base */
    0,                                        /* tp_dict */
    0,                                        /* tp_descr_get */
    0,                                        /* tp_descr_set */
    0,                                        /* tp_dictoffset */
    (initproc)parser___init__,                /* tp_init */
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
    "_parser",                           /* m_name */
    "Beancount parser extension module", /* m_doc */
    -1,                                  /* m_size */
    module_functions,                    /* m_methods */
    NULL,                                /* m_reload */
    NULL,                                /* m_traverse */
    NULL,                                /* m_clear */
    NULL,                                /* m_free */
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
    Py_INCREF(&Parser_Type);

    PyObject* module = PyModule_Create(&moduledef);
    if (!module)
        goto error;

    initialize_metadata(module);

    /* Import the module that defines the missing object constant. */
    PyObject* number_module = PyImport_ImportModule("beancount.core.number");
    if (!number_module)
        goto error;

    missing = PyObject_GetAttrString(number_module, "MISSING");
    if (!missing)
        goto error;

    if (PyType_Ready(&Parser_Type) < 0)
        goto error;
    if (PyModule_AddObject(module, "Parser", (PyObject *)&Parser_Type) < 0)
        goto error;

    return module;

error:
    Py_DECREF(&Parser_Type);
    Py_DECREF(module);
    return NULL;
}

/* FIXME: Finalize too, unrefing the constants. {48414425cf78} */

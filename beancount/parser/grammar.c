#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <datetime.h>

#include "beancount/parser/decimal.h"

static PyObject* g_traceback_format_exception_only;
static PyObject* g_traceback_extract_tb;
static PyObject* g_options_defaults;
static PyObject* g_options_read_only;
static PyObject* g_options_options;
static PyObject* g_display_context_type;
static PyObject* g_entry_sortkey;
static PyObject* g_new_metadata;
static PyObject* g_amount_type;
static PyObject* g_empty_set;
static PyObject* g_balance_type;
static PyObject* g_booking_enum;
static PyObject* g_close_type;
static PyObject* g_commodity_type;
static PyObject* g_custom_type;
static PyObject* g_document_type;
static PyObject* g_event_type;
static PyObject* g_note_type;
static PyObject* g_open_type;
static PyObject* g_pad_type;
static PyObject* g_posting_type;
static PyObject* g_price_type;
static PyObject* g_query_type;
static PyObject* g_transaction_type;
static PyObject* g_missing;
static PyObject* g_zero;
static PyObject* g_cost_spec_type;
static PyObject* g_path_isabs;
static PyObject* g_path_abspath;
static PyObject* g_path_join;
static PyObject* g_path_dirname;

static PyObject* g_grammar_module;
static PyObject* g_parser_error_type;
static PyObject* g_parser_syntax_error_type;
static PyObject* g_deprecated_error_type;
static PyObject* g_key_value_type;
static PyObject* g_value_type_type;
static PyObject* g_compound_amount_type;
static PyObject* g_tags_links_type;
static PyObject* g_merge_cost;
static PyObject* g_valid_account_regexp;

static int
cache_grammar_symbols(void)
{
    if (g_grammar_module != NULL) {
        return 0;
    }

    g_grammar_module = PyImport_ImportModule("beancount.parser.grammar");
    if (g_grammar_module == NULL) {
        return -1;
    }

    g_parser_error_type = PyObject_GetAttrString(g_grammar_module, "ParserError");
    g_parser_syntax_error_type = PyObject_GetAttrString(g_grammar_module, "ParserSyntaxError");
    g_deprecated_error_type = PyObject_GetAttrString(g_grammar_module, "DeprecatedError");
    g_key_value_type = PyObject_GetAttrString(g_grammar_module, "KeyValue");
    g_value_type_type = PyObject_GetAttrString(g_grammar_module, "ValueType");
    g_compound_amount_type = PyObject_GetAttrString(g_grammar_module, "CompoundAmount");
    g_tags_links_type = PyObject_GetAttrString(g_grammar_module, "TagsLinks");
    g_merge_cost = PyObject_GetAttrString(g_grammar_module, "MERGE_COST");
    g_valid_account_regexp = PyObject_GetAttrString(g_grammar_module, "valid_account_regexp");

    if (g_parser_error_type == NULL ||
        g_parser_syntax_error_type == NULL ||
        g_deprecated_error_type == NULL ||
        g_key_value_type == NULL ||
        g_value_type_type == NULL ||
        g_compound_amount_type == NULL ||
        g_tags_links_type == NULL ||
        g_merge_cost == NULL ||
        g_valid_account_regexp == NULL) {
        return -1;
    }

    return 0;
}

static PyObject*
get_attr(PyObject* obj, const char* name)
{
    return PyObject_GetAttrString(obj, name);
}

static int
set_attr(PyObject* obj, const char* name, PyObject* value)
{
    return PyObject_SetAttrString(obj, name, value);
}

static int
append_error(PyObject* self, PyObject* error)
{
    PyObject* errors = get_attr(self, "errors");
    int rc;
    if (errors == NULL) {
        return -1;
    }
    rc = PyList_Append(errors, error);
    Py_DECREF(errors);
    return rc;
}

static PyObject*
new_metadata_call(PyObject* filename, int lineno, PyObject* kvlist)
{
    if (kvlist == NULL) {
        return PyObject_CallFunction(g_new_metadata, "Oi", filename, lineno);
    }
    return PyObject_CallFunction(g_new_metadata, "OiO", filename, lineno, kvlist);
}

static PyObject*
make_error(PyObject* error_type, PyObject* filename, int lineno, PyObject* message, PyObject* entry)
{
    PyObject* meta = new_metadata_call(filename, lineno, NULL);
    PyObject* error;
    if (meta == NULL) {
        return NULL;
    }
    if (entry == NULL) {
        error = PyObject_CallFunctionObjArgs(error_type, meta, message, NULL);
    } else {
        error = PyObject_CallFunctionObjArgs(error_type, meta, message, entry, NULL);
    }
    Py_DECREF(meta);
    return error;
}

static int
append_simple_error(PyObject* self, PyObject* error_type, PyObject* filename, int lineno, PyObject* message, PyObject* entry)
{
    PyObject* error = make_error(error_type, filename, lineno, message, entry);
    int rc;
    if (error == NULL) {
        return -1;
    }
    rc = append_error(self, error);
    Py_DECREF(error);
    return rc;
}

static PyObject*
builder_account(PyObject* module, PyObject* args)
{
    PyObject* self;
    PyObject* filename;
    int lineno;
    PyObject* account;
    PyObject* regexp;
    PyObject* matched;
    PyObject* accounts;
    PyObject* interned;

    if (!PyArg_ParseTuple(args, "OOiO", &self, &filename, &lineno, &account)) {
        return NULL;
    }
    if (cache_grammar_symbols() < 0) {
        return NULL;
    }

    regexp = get_attr(self, "account_regexp");
    if (regexp == NULL) {
        return NULL;
    }
    matched = PyObject_CallMethod(regexp, "match", "O", account);
    Py_DECREF(regexp);
    if (matched == NULL) {
        return NULL;
    }
    if (matched == Py_None) {
        PyObject* msg = PyUnicode_FromFormat("Invalid account name: %S", account);
        Py_DECREF(matched);
        if (msg == NULL) {
            return NULL;
        }
        if (append_simple_error(self, g_parser_error_type, filename, lineno, msg, NULL) < 0) {
            Py_DECREF(msg);
            return NULL;
        }
        Py_DECREF(msg);
    } else {
        Py_DECREF(matched);
    }

    accounts = get_attr(self, "accounts");
    if (accounts == NULL) {
        return NULL;
    }
    interned = PyDict_SetDefault(accounts, account, account);
    Py_DECREF(accounts);
    if (interned == NULL) {
        return NULL;
    }
    Py_INCREF(interned);
    return interned;
}

static PyObject*
builder_pipe_deprecated_error(PyObject* module, PyObject* args)
{
    PyObject* self;
    PyObject* filename;
    int lineno;
    PyObject* options;
    PyObject* allow;
    PyObject* msg;

    if (!PyArg_ParseTuple(args, "OOi", &self, &filename, &lineno)) {
        return NULL;
    }
    if (cache_grammar_symbols() < 0) {
        return NULL;
    }
    options = get_attr(self, "options");
    if (options == NULL) {
        return NULL;
    }
    allow = PyDict_GetItemString(options, "allow_pipe_separator");
    Py_XINCREF(allow);
    Py_DECREF(options);
    if (allow == NULL) {
        return NULL;
    }
    if (PyObject_IsTrue(allow)) {
        Py_DECREF(allow);
        Py_RETURN_NONE;
    }
    Py_DECREF(allow);
    msg = PyUnicode_FromString("Pipe symbol is deprecated.");
    if (msg == NULL) {
        return NULL;
    }
    if (append_simple_error(self, g_parser_syntax_error_type, filename, lineno, msg, NULL) < 0) {
        Py_DECREF(msg);
        return NULL;
    }
    Py_DECREF(msg);
    Py_RETURN_NONE;
}

static PyObject*
builder_pushtag(PyObject* module, PyObject* args)
{
    PyObject* self;
    PyObject* filename;
    int lineno;
    PyObject* tag;
    PyObject* tags;
    int rc;

    if (!PyArg_ParseTuple(args, "OOiO", &self, &filename, &lineno, &tag)) {
        return NULL;
    }
    tags = get_attr(self, "tags");
    if (tags == NULL) {
        return NULL;
    }
    rc = PyList_Append(tags, tag);
    Py_DECREF(tags);
    if (rc < 0) {
        return NULL;
    }
    Py_RETURN_NONE;
}

static PyObject*
builder_poptag(PyObject* module, PyObject* args)
{
    PyObject* self;
    PyObject* filename;
    int lineno;
    PyObject* tag;
    PyObject* tags;
    PyObject* result;

    if (!PyArg_ParseTuple(args, "OOiO", &self, &filename, &lineno, &tag)) {
        return NULL;
    }
    if (cache_grammar_symbols() < 0) {
        return NULL;
    }
    tags = get_attr(self, "tags");
    if (tags == NULL) {
        return NULL;
    }
    result = PyObject_CallMethod(tags, "remove", "O", tag);
    Py_DECREF(tags);
    if (result == NULL) {
        PyErr_Clear();
        PyObject* msg = PyUnicode_FromFormat("Attempting to pop absent tag: '%S'", tag);
        if (msg == NULL) {
            return NULL;
        }
        if (append_simple_error(self, g_parser_error_type, filename, lineno, msg, NULL) < 0) {
            Py_DECREF(msg);
            return NULL;
        }
        Py_DECREF(msg);
        Py_RETURN_NONE;
    }
    Py_DECREF(result);
    Py_RETURN_NONE;
}

static PyObject*
builder_pushmeta(PyObject* module, PyObject* args)
{
    PyObject* self;
    PyObject* filename;
    int lineno;
    PyObject* key_value;
    PyObject* meta;
    PyObject* key;
    PyObject* value;
    PyObject* list;
    int rc;

    if (!PyArg_ParseTuple(args, "OOiO", &self, &filename, &lineno, &key_value)) {
        return NULL;
    }
    if (!PyArg_UnpackTuple(key_value, "", 2, 2, &key, &value)) {
        return NULL;
    }
    meta = get_attr(self, "meta");
    if (meta == NULL) {
        return NULL;
    }
    list = PyDict_GetItemWithError(meta, key);
    if (list == NULL && PyErr_Occurred()) {
        Py_DECREF(meta);
        return NULL;
    }
    if (list == NULL) {
        list = PyList_New(0);
        if (list == NULL) {
            Py_DECREF(meta);
            return NULL;
        }
        rc = PyDict_SetItem(meta, key, list);
        if (rc < 0) {
            Py_DECREF(list);
            Py_DECREF(meta);
            return NULL;
        }
    } else {
        Py_INCREF(list);
    }
    Py_DECREF(meta);
    rc = PyList_Append(list, value);
    Py_DECREF(list);
    if (rc < 0) {
        return NULL;
    }
    Py_RETURN_NONE;
}

static PyObject*
builder_popmeta(PyObject* module, PyObject* args)
{
    PyObject* self;
    PyObject* filename;
    int lineno;
    PyObject* key;
    PyObject* meta;
    PyObject* list;
    Py_ssize_t size;

    if (!PyArg_ParseTuple(args, "OOiO", &self, &filename, &lineno, &key)) {
        return NULL;
    }
    if (cache_grammar_symbols() < 0) {
        return NULL;
    }
    meta = get_attr(self, "meta");
    if (meta == NULL) {
        return NULL;
    }
    list = PyDict_GetItemWithError(meta, key);
    if (list == NULL && PyErr_Occurred()) {
        Py_DECREF(meta);
        return NULL;
    }
    if (list == NULL || (size = PyList_GET_SIZE(list)) == 0) {
        PyObject* msg = PyUnicode_FromFormat("Attempting to pop absent metadata key: '%S'", key);
        Py_DECREF(meta);
        if (msg == NULL) {
            return NULL;
        }
        if (append_simple_error(self, g_parser_error_type, filename, lineno, msg, NULL) < 0) {
            Py_DECREF(msg);
            return NULL;
        }
        Py_DECREF(msg);
        Py_RETURN_NONE;
    }
    if (PySequence_DelItem(list, size - 1) < 0) {
        Py_DECREF(meta);
        return NULL;
    }
    if (PyList_GET_SIZE(list) == 0) {
        if (PyDict_DelItem(meta, key) < 0) {
            Py_DECREF(meta);
            return NULL;
        }
    }
    Py_DECREF(meta);
    Py_RETURN_NONE;
}

static PyObject*
builder_option(PyObject* module, PyObject* args)
{
    PyObject* self;
    PyObject* filename;
    int lineno;
    PyObject* key;
    PyObject* value;
    PyObject* options_map;
    PyObject* option_descriptor;
    PyObject* deprecated;
    PyObject* alias;
    PyObject* converter;
    PyObject* option;

    if (!PyArg_ParseTuple(args, "OOiOO", &self, &filename, &lineno, &key, &value)) {
        return NULL;
    }
    if (cache_grammar_symbols() < 0) {
        return NULL;
    }

    options_map = get_attr(self, "options");
    if (options_map == NULL) {
        return NULL;
    }

    if (!PyDict_Contains(options_map, key)) {
        PyObject* msg = PyUnicode_FromFormat("Invalid option: '%S'", key);
        Py_DECREF(options_map);
        if (msg == NULL) {
            return NULL;
        }
        if (append_simple_error(self, g_parser_error_type, filename, lineno, msg, NULL) < 0) {
            Py_DECREF(msg);
            return NULL;
        }
        Py_DECREF(msg);
        Py_RETURN_NONE;
    }

    if (PySequence_Contains(g_options_read_only, key)) {
        PyObject* msg = PyUnicode_FromFormat("Option '%S' may not be set", key);
        Py_DECREF(options_map);
        if (msg == NULL) {
            return NULL;
        }
        if (append_simple_error(self, g_parser_error_type, filename, lineno, msg, NULL) < 0) {
            Py_DECREF(msg);
            return NULL;
        }
        Py_DECREF(msg);
        Py_RETURN_NONE;
    }

    option_descriptor = PyObject_GetItem(g_options_options, key);
    if (option_descriptor == NULL) {
        Py_DECREF(options_map);
        return NULL;
    }

    deprecated = get_attr(option_descriptor, "deprecated");
    if (deprecated == NULL) {
        Py_DECREF(option_descriptor);
        Py_DECREF(options_map);
        return NULL;
    }
    if (PyObject_IsTrue(deprecated) == 1) {
        if (append_simple_error(self, g_deprecated_error_type, filename, lineno, deprecated, NULL) < 0) {
            Py_DECREF(deprecated);
            Py_DECREF(option_descriptor);
            Py_DECREF(options_map);
            return NULL;
        }
    }
    Py_DECREF(deprecated);

    alias = get_attr(option_descriptor, "alias");
    if (alias == NULL) {
        Py_DECREF(option_descriptor);
        Py_DECREF(options_map);
        return NULL;
    }
    if (PyObject_IsTrue(alias) == 1) {
        Py_INCREF(alias);
        key = alias;
        Py_DECREF(option_descriptor);
        option_descriptor = PyObject_GetItem(g_options_options, key);
        if (option_descriptor == NULL) {
            Py_DECREF(options_map);
            return NULL;
        }
    } else {
        Py_DECREF(alias);
    }

    converter = get_attr(option_descriptor, "converter");
    if (converter == NULL) {
        Py_DECREF(option_descriptor);
        Py_DECREF(options_map);
        return NULL;
    }
    if (converter != Py_None) {
        PyObject* converted = PyObject_CallFunctionObjArgs(converter, value, NULL);
        if (converted == NULL) {
            PyObject *ptype, *pvalue, *ptraceback;
            PyObject* msg;
            PyErr_Fetch(&ptype, &pvalue, &ptraceback);
            PyErr_NormalizeException(&ptype, &pvalue, &ptraceback);
            msg = PyUnicode_FromFormat("Error for option '%S': %S", key, pvalue);
            Py_XDECREF(ptype);
            Py_XDECREF(pvalue);
            Py_XDECREF(ptraceback);
            Py_DECREF(converter);
            Py_DECREF(option_descriptor);
            Py_DECREF(options_map);
            if (msg == NULL) {
                return NULL;
            }
            if (append_simple_error(self, g_parser_error_type, filename, lineno, msg, NULL) < 0) {
                Py_DECREF(msg);
                return NULL;
            }
            Py_DECREF(msg);
            Py_RETURN_NONE;
        }
        value = converted;
    } else {
        Py_INCREF(value);
    }
    Py_DECREF(converter);

    option = PyObject_GetItem(options_map, key);
    if (option == NULL) {
        Py_DECREF(value);
        Py_DECREF(option_descriptor);
        Py_DECREF(options_map);
        return NULL;
    }

    if (PyList_Check(option)) {
        if (PyList_Append(option, value) < 0) {
            Py_DECREF(option);
            Py_DECREF(value);
            Py_DECREF(option_descriptor);
            Py_DECREF(options_map);
            return NULL;
        }
    } else if (PyDict_Check(option)) {
        PyObject* dict_key;
        PyObject* dict_value;
        if (!PyTuple_Check(value) || PyTuple_GET_SIZE(value) != 2) {
            PyObject* msg = PyUnicode_FromFormat("Error for option '%S': %S", key, value);
            Py_DECREF(option);
            Py_DECREF(value);
            Py_DECREF(option_descriptor);
            Py_DECREF(options_map);
            if (msg == NULL) {
                return NULL;
            }
            if (append_simple_error(self, g_parser_error_type, filename, lineno, msg, NULL) < 0) {
                Py_DECREF(msg);
                return NULL;
            }
            Py_DECREF(msg);
            Py_RETURN_NONE;
        }
        dict_key = PyTuple_GET_ITEM(value, 0);
        dict_value = PyTuple_GET_ITEM(value, 1);
        if (PyDict_SetItem(option, dict_key, dict_value) < 0) {
            Py_DECREF(option);
            Py_DECREF(value);
            Py_DECREF(option_descriptor);
            Py_DECREF(options_map);
            return NULL;
        }
    } else if (PyBool_Check(option)) {
        PyObject* new_value = value;
        int truth = PyObject_IsTrue(value);
        if (!PyBool_Check(value)) {
            PyObject* lower = PyObject_CallMethod(value, "lower", NULL);
            int is_true = 0;
            if (lower == NULL) {
                Py_DECREF(option);
                Py_DECREF(value);
                Py_DECREF(option_descriptor);
                Py_DECREF(options_map);
                return NULL;
            }
            is_true = PyUnicode_CompareWithASCIIString(lower, "true") == 0 ||
                      PyUnicode_CompareWithASCIIString(lower, "on") == 0 ||
                      PyUnicode_CompareWithASCIIString(value, "1") == 0;
            Py_DECREF(lower);
            new_value = is_true ? Py_True : Py_False;
            Py_INCREF(new_value);
        } else if (truth < 0) {
            Py_DECREF(option);
            Py_DECREF(value);
            Py_DECREF(option_descriptor);
            Py_DECREF(options_map);
            return NULL;
        } else {
            Py_INCREF(new_value);
        }
        if (PyDict_SetItem(options_map, key, new_value) < 0) {
            Py_DECREF(new_value);
            Py_DECREF(option);
            Py_DECREF(value);
            Py_DECREF(option_descriptor);
            Py_DECREF(options_map);
            return NULL;
        }
        Py_DECREF(new_value);
    } else {
        if (PyDict_SetItem(options_map, key, value) < 0) {
            Py_DECREF(option);
            Py_DECREF(value);
            Py_DECREF(option_descriptor);
            Py_DECREF(options_map);
            return NULL;
        }
    }

    Py_DECREF(option);
    Py_DECREF(value);

    if (PyUnicode_Check(key) && PyUnicode_Tailmatch(key, PyUnicode_FromString("name_"), 0, PY_SSIZE_T_MAX, -1) == 1) {
        PyObject* regexp = PyObject_CallFunctionObjArgs(g_valid_account_regexp, options_map, NULL);
        if (regexp == NULL) {
            Py_DECREF(option_descriptor);
            Py_DECREF(options_map);
            return NULL;
        }
        if (set_attr(self, "account_regexp", regexp) < 0) {
            Py_DECREF(regexp);
            Py_DECREF(option_descriptor);
            Py_DECREF(options_map);
            return NULL;
        }
        Py_DECREF(regexp);
    }

    Py_DECREF(option_descriptor);
    Py_DECREF(options_map);
    Py_RETURN_NONE;
}

static PyObject*
builder_include(PyObject* module, PyObject* args)
{
    PyObject *self, *filename, *include_filename;
    int lineno;
    PyObject* options_map;
    PyObject* include_list;
    int rc;

    if (!PyArg_ParseTuple(args, "OOiO", &self, &filename, &lineno, &include_filename)) {
        return NULL;
    }
    options_map = get_attr(self, "options");
    if (options_map == NULL) {
        return NULL;
    }
    include_list = PyDict_GetItemString(options_map, "include");
    Py_XINCREF(include_list);
    Py_DECREF(options_map);
    if (include_list == NULL) {
        return NULL;
    }
    rc = PyList_Append(include_list, include_filename);
    Py_DECREF(include_list);
    if (rc < 0) {
        return NULL;
    }
    Py_RETURN_NONE;
}

static PyObject*
builder_plugin(PyObject* module, PyObject* args)
{
    PyObject *self, *filename, *plugin_name, *plugin_config;
    int lineno;
    PyObject* options_map;
    PyObject* plugin_list;
    PyObject* item;
    int rc;

    if (!PyArg_ParseTuple(args, "OOiOO", &self, &filename, &lineno, &plugin_name, &plugin_config)) {
        return NULL;
    }
    options_map = get_attr(self, "options");
    if (options_map == NULL) {
        return NULL;
    }
    plugin_list = PyDict_GetItemString(options_map, "plugin");
    Py_XINCREF(plugin_list);
    Py_DECREF(options_map);
    if (plugin_list == NULL) {
        return NULL;
    }
    item = PyTuple_Pack(2, plugin_name, plugin_config);
    if (item == NULL) {
        Py_DECREF(plugin_list);
        return NULL;
    }
    rc = PyList_Append(plugin_list, item);
    Py_DECREF(item);
    Py_DECREF(plugin_list);
    if (rc < 0) {
        return NULL;
    }
    Py_RETURN_NONE;
}

static PyObject*
dcupdate(PyObject* self, PyObject* number, PyObject* currency)
{
    int is_decimal = PyObject_IsInstance(number, decimal_type);
    if (is_decimal < 0) {
        return NULL;
    }
    if (is_decimal && currency != NULL && currency != g_missing) {
        PyObject* updater = get_attr(self, "display_context_update");
        PyObject* rv;
        if (updater == NULL) {
            return NULL;
        }
        rv = PyObject_CallFunctionObjArgs(updater, number, currency, NULL);
        Py_DECREF(updater);
        return rv;
    }
    Py_RETURN_NONE;
}

static PyObject*
builder_amount(PyObject* module, PyObject* args)
{
    PyObject *self, *filename, *number, *currency;
    int lineno;
    PyObject* rv;
    PyObject* amount;

    if (!PyArg_ParseTuple(args, "OOiOO", &self, &filename, &lineno, &number, &currency)) {
        return NULL;
    }
    rv = dcupdate(self, number, currency);
    if (rv == NULL) {
        return NULL;
    }
    Py_DECREF(rv);
    amount = PyObject_CallFunctionObjArgs(g_amount_type, number, currency, NULL);
    return amount;
}

static PyObject*
builder_compound_amount(PyObject* module, PyObject* args)
{
    PyObject *self, *filename, *number_per, *number_total, *currency;
    int lineno;
    PyObject* rv;
    PyObject* amount;

    if (!PyArg_ParseTuple(args, "OOiOOO", &self, &filename, &lineno, &number_per, &number_total, &currency)) {
        return NULL;
    }
    rv = dcupdate(self, number_per, currency);
    if (rv == NULL) {
        return NULL;
    }
    Py_DECREF(rv);
    rv = dcupdate(self, number_total, currency);
    if (rv == NULL) {
        return NULL;
    }
    Py_DECREF(rv);
    if (cache_grammar_symbols() < 0) {
        return NULL;
    }
    amount = PyObject_CallFunctionObjArgs(g_compound_amount_type, number_per, number_total, currency, NULL);
    return amount;
}

static PyObject*
builder_cost_merge(PyObject* module, PyObject* args)
{
    if (cache_grammar_symbols() < 0) {
        return NULL;
    }
    Py_INCREF(g_merge_cost);
    return g_merge_cost;
}

static PyObject*
builder_handle_list(PyObject* module, PyObject* args)
{
    PyObject* self;
    PyObject* filename;
    int lineno;
    PyObject* object_list;
    PyObject* new_object;

    if (!PyArg_ParseTuple(args, "OOiOO", &self, &filename, &lineno, &object_list, &new_object)) {
        return NULL;
    }
    if (object_list == Py_None) {
        object_list = PyList_New(0);
    } else {
        Py_INCREF(object_list);
    }
    if (object_list == NULL) {
        return NULL;
    }
    if (new_object != Py_None) {
        if (PyList_Append(object_list, new_object) < 0) {
            Py_DECREF(object_list);
            return NULL;
        }
    }
    return object_list;
}

static PyObject*
builder_cost_spec(PyObject* module, PyObject* args)
{
    PyObject *self, *filename, *cost_comp_list, *is_total;
    int lineno;
    PyObject *compound_cost = NULL, *date = NULL, *label = NULL;
    int merge = 0;
    int is_total_bool = 0;
    Py_ssize_t i, size;

    if (!PyArg_ParseTuple(args, "OOiOO", &self, &filename, &lineno, &cost_comp_list, &is_total)) {
        return NULL;
    }
    if (cache_grammar_symbols() < 0) {
        return NULL;
    }

    is_total_bool = PyObject_IsTrue(is_total);
    if (is_total_bool < 0) {
        return NULL;
    }

    if (cost_comp_list == Py_None) {
        return PyObject_CallFunctionObjArgs(g_cost_spec_type, g_missing, Py_None, g_missing, Py_None, Py_None, Py_False, NULL);
    }

    size = PyList_Size(cost_comp_list);
    if (size < 0) {
        return NULL;
    }
    for (i = 0; i < size; i++) {
        PyObject* comp = PyList_GetItem(cost_comp_list, i);
        int is_compound;
        if (comp == NULL) {
            return NULL;
        }
        is_compound = PyObject_IsInstance(comp, g_compound_amount_type);
        if (is_compound < 0) {
            return NULL;
        }
        if (is_compound) {
            if (compound_cost == NULL) {
                compound_cost = comp;
                Py_INCREF(compound_cost);
            } else {
                PyObject* msg = PyUnicode_FromFormat("Duplicate cost: '%S'.", comp);
                if (msg == NULL) {
                    return NULL;
                }
                if (append_simple_error(self, g_parser_error_type, filename, lineno, msg, NULL) < 0) {
                    Py_DECREF(msg);
                    return NULL;
                }
                Py_DECREF(msg);
            }
        } else if (PyDate_Check(comp)) {
            if (date == NULL) {
                date = comp;
                Py_INCREF(date);
            } else {
                PyObject* msg = PyUnicode_FromFormat("Duplicate date: '%S'.", comp);
                if (msg == NULL) {
                    return NULL;
                }
                if (append_simple_error(self, g_parser_error_type, filename, lineno, msg, NULL) < 0) {
                    Py_DECREF(msg);
                    return NULL;
                }
                Py_DECREF(msg);
            }
        } else if (comp == g_merge_cost) {
            if (!merge) {
                PyObject* msg = PyUnicode_FromString("Cost merging is not supported yet");
                merge = 1;
                if (msg == NULL) {
                    return NULL;
                }
                if (append_simple_error(self, g_parser_error_type, filename, lineno, msg, NULL) < 0) {
                    Py_DECREF(msg);
                    return NULL;
                }
                Py_DECREF(msg);
            } else {
                PyObject* msg = PyUnicode_FromString("Duplicate merge-cost spec");
                if (msg == NULL) {
                    return NULL;
                }
                if (append_simple_error(self, g_parser_error_type, filename, lineno, msg, NULL) < 0) {
                    Py_DECREF(msg);
                    return NULL;
                }
                Py_DECREF(msg);
            }
        } else {
            if (label == NULL) {
                label = comp;
                Py_INCREF(label);
            } else {
                PyObject* msg = PyUnicode_FromFormat("Duplicate label: '%S'.", comp);
                if (msg == NULL) {
                    return NULL;
                }
                if (append_simple_error(self, g_parser_error_type, filename, lineno, msg, NULL) < 0) {
                    Py_DECREF(msg);
                    return NULL;
                }
                Py_DECREF(msg);
            }
        }
    }

    if (compound_cost == NULL) {
        return PyObject_CallFunctionObjArgs(g_cost_spec_type,
                                            g_missing,
                                            Py_None,
                                            g_missing,
                                            date ? date : Py_None,
                                            label ? label : Py_None,
                                            merge ? Py_True : Py_False,
                                            NULL);
    }

    PyObject* number_per = PyTuple_GetItem(compound_cost, 0);
    PyObject* number_total = PyTuple_GetItem(compound_cost, 1);
    PyObject* currency = PyTuple_GetItem(compound_cost, 2);
    Py_INCREF(number_per);
    Py_INCREF(number_total);
    Py_INCREF(currency);

    if (is_total_bool) {
        if (number_total != Py_None) {
            PyObject* msg = PyUnicode_FromFormat(
                "Per-unit cost may not be specified using total cost syntax: '%S'; ignoring per-unit cost",
                compound_cost);
            if (msg == NULL) {
                Py_DECREF(number_per);
                Py_DECREF(number_total);
                Py_DECREF(currency);
                return NULL;
            }
            if (append_simple_error(self, g_parser_error_type, filename, lineno, msg, NULL) < 0) {
                Py_DECREF(msg);
                Py_DECREF(number_per);
                Py_DECREF(number_total);
                Py_DECREF(currency);
                return NULL;
            }
            Py_DECREF(msg);
            Py_DECREF(number_per);
            number_per = g_zero;
            Py_INCREF(number_per);
        } else {
            Py_DECREF(number_total);
            number_total = number_per;
            Py_INCREF(number_total);
            Py_DECREF(number_per);
            number_per = g_zero;
            Py_INCREF(number_per);
        }
    }

    PyObject* result = PyObject_CallFunctionObjArgs(g_cost_spec_type,
                                                    number_per,
                                                    number_total,
                                                    currency,
                                                    date ? date : Py_None,
                                                    label ? label : Py_None,
                                                    merge ? Py_True : Py_False,
                                                    NULL);
    Py_DECREF(number_per);
    Py_DECREF(number_total);
    Py_DECREF(currency);
    Py_XDECREF(compound_cost);
    Py_XDECREF(date);
    Py_XDECREF(label);
    return result;
}

static PyObject*
builder_open(PyObject* module, PyObject* args)
{
    PyObject *self, *filename, *date, *account, *currencies, *booking_str, *kvlist;
    int lineno;
    PyObject* meta;
    PyObject* booking = Py_None;
    PyObject* entry;
    int error = 0;

    if (!PyArg_ParseTuple(args, "OOiOOOOO", &self, &filename, &lineno, &date, &account, &currencies, &booking_str, &kvlist)) {
        return NULL;
    }
    meta = new_metadata_call(filename, lineno, kvlist);
    if (meta == NULL) {
        return NULL;
    }
    if (booking_str != Py_None) {
        booking = PyObject_GetItem(g_booking_enum, booking_str);
        if (booking == NULL) {
            PyErr_Clear();
            PyObject* options_map = get_attr(self, "options");
            if (options_map == NULL) {
                Py_DECREF(meta);
                return NULL;
            }
            booking = PyObject_GetItem(options_map, PyUnicode_FromString("booking_method"));
            Py_DECREF(options_map);
            if (booking == NULL) {
                Py_DECREF(meta);
                return NULL;
            }
            error = 1;
        }
    } else {
        Py_INCREF(booking);
    }
    entry = PyObject_CallFunctionObjArgs(g_open_type, meta, date, account, currencies, booking == Py_None ? Py_None : booking, NULL);
    Py_DECREF(meta);
    Py_DECREF(booking);
    if (entry == NULL) {
        return NULL;
    }
    if (error) {
        PyObject* msg = PyUnicode_FromFormat("Invalid booking method: %S", booking_str);
        if (msg == NULL) {
            Py_DECREF(entry);
            return NULL;
        }
        if (append_simple_error(self, g_parser_error_type, filename, lineno, msg, entry) < 0) {
            Py_DECREF(msg);
            Py_DECREF(entry);
            return NULL;
        }
        Py_DECREF(msg);
    }
    return entry;
}

static PyObject*
simple_entry(PyObject* type, PyObject* filename, int lineno, PyObject* kvlist, Py_ssize_t nargs, PyObject** values)
{
    PyObject* meta = new_metadata_call(filename, lineno, kvlist);
    PyObject* result;
    PyObject** all_args;
    Py_ssize_t i;
    if (meta == NULL) {
        return NULL;
    }
    all_args = PyMem_New(PyObject*, nargs + 1);
    if (all_args == NULL) {
        Py_DECREF(meta);
        return PyErr_NoMemory();
    }
    all_args[0] = meta;
    for (i = 0; i < nargs; i++) {
        all_args[i + 1] = values[i];
    }
    result = PyObject_Vectorcall(type, all_args, nargs + 1, NULL);
    PyMem_Free(all_args);
    Py_DECREF(meta);
    return result;
}

static PyObject*
builder_close(PyObject* module, PyObject* args)
{
    PyObject *self, *filename, *date, *account, *kvlist;
    int lineno;
    PyObject* values[2];
    if (!PyArg_ParseTuple(args, "OOiOOO", &self, &filename, &lineno, &date, &account, &kvlist)) {
        return NULL;
    }
    values[0] = date;
    values[1] = account;
    return simple_entry(g_close_type, filename, lineno, kvlist, 2, values);
}

static PyObject*
builder_commodity(PyObject* module, PyObject* args)
{
    PyObject *self, *filename, *date, *currency, *kvlist;
    int lineno;
    PyObject* values[2];
    if (!PyArg_ParseTuple(args, "OOiOOO", &self, &filename, &lineno, &date, &currency, &kvlist)) {
        return NULL;
    }
    values[0] = date;
    values[1] = currency;
    return simple_entry(g_commodity_type, filename, lineno, kvlist, 2, values);
}

static PyObject*
builder_pad(PyObject* module, PyObject* args)
{
    PyObject *self, *filename, *date, *account, *source_account, *kvlist;
    int lineno;
    PyObject* values[3];
    if (!PyArg_ParseTuple(args, "OOiOOOO", &self, &filename, &lineno, &date, &account, &source_account, &kvlist)) {
        return NULL;
    }
    values[0] = date;
    values[1] = account;
    values[2] = source_account;
    return simple_entry(g_pad_type, filename, lineno, kvlist, 3, values);
}

static PyObject*
builder_balance(PyObject* module, PyObject* args)
{
    PyObject *self, *filename, *date, *account, *amount, *tolerance, *kvlist;
    int lineno;
    PyObject* meta;
    if (!PyArg_ParseTuple(args, "OOiOOOOO", &self, &filename, &lineno, &date, &account, &amount, &tolerance, &kvlist)) {
        return NULL;
    }
    meta = new_metadata_call(filename, lineno, kvlist);
    if (meta == NULL) {
        return NULL;
    }
    PyObject* rv = PyObject_CallFunctionObjArgs(g_balance_type, meta, date, account, amount, tolerance, Py_None, NULL);
    Py_DECREF(meta);
    return rv;
}

static PyObject*
builder_event(PyObject* module, PyObject* args)
{
    PyObject *self, *filename, *date, *event_type, *description, *kvlist;
    int lineno;
    PyObject* values[3];
    if (!PyArg_ParseTuple(args, "OOiOOOO", &self, &filename, &lineno, &date, &event_type, &description, &kvlist)) {
        return NULL;
    }
    values[0] = date;
    values[1] = event_type;
    values[2] = description;
    return simple_entry(g_event_type, filename, lineno, kvlist, 3, values);
}

static PyObject*
builder_query(PyObject* module, PyObject* args)
{
    PyObject *self, *filename, *date, *query_name, *query_string, *kvlist;
    int lineno;
    PyObject* values[3];
    if (!PyArg_ParseTuple(args, "OOiOOOO", &self, &filename, &lineno, &date, &query_name, &query_string, &kvlist)) {
        return NULL;
    }
    values[0] = date;
    values[1] = query_name;
    values[2] = query_string;
    return simple_entry(g_query_type, filename, lineno, kvlist, 3, values);
}

static PyObject*
builder_price(PyObject* module, PyObject* args)
{
    PyObject *self, *filename, *date, *currency, *amount, *kvlist;
    int lineno;
    PyObject* values[3];
    if (!PyArg_ParseTuple(args, "OOiOOOO", &self, &filename, &lineno, &date, &currency, &amount, &kvlist)) {
        return NULL;
    }
    values[0] = date;
    values[1] = currency;
    values[2] = amount;
    return simple_entry(g_price_type, filename, lineno, kvlist, 3, values);
}

static PyObject*
finalize_tags_links_impl(PyObject* self, PyObject* tags, PyObject* links)
{
    PyObject* stack_tags = get_attr(self, "tags");
    PyObject* result_tags;
    PyObject* result_links;
    if (stack_tags == NULL) {
        return NULL;
    }
    if (PyList_Size(stack_tags) > 0) {
        PyObject* update = PyObject_CallMethod(tags, "update", "O", stack_tags);
        if (update == NULL) {
            Py_DECREF(stack_tags);
            return NULL;
        }
        Py_DECREF(update);
    }
    Py_DECREF(stack_tags);
    result_tags = PyObject_IsTrue(tags) ? PyFrozenSet_New(tags) : Py_NewRef(g_empty_set);
    if (result_tags == NULL) {
        return NULL;
    }
    result_links = PyObject_IsTrue(links) ? PyFrozenSet_New(links) : Py_NewRef(g_empty_set);
    if (result_links == NULL) {
        Py_DECREF(result_tags);
        return NULL;
    }
    return PyTuple_Pack(2, result_tags, result_links);
}

static PyObject*
builder_note(PyObject* module, PyObject* args)
{
    PyObject *self, *filename, *date, *account, *comment, *tags_links, *kvlist;
    int lineno;
    PyObject* meta;
    PyObject* tags;
    PyObject* links;
    PyObject* final_tags_links;
    PyObject* rv;
    if (!PyArg_ParseTuple(args, "OOiOOOOO", &self, &filename, &lineno, &date, &account, &comment, &tags_links, &kvlist)) {
        return NULL;
    }
    meta = new_metadata_call(filename, lineno, kvlist);
    if (meta == NULL) {
        return NULL;
    }
    tags = get_attr(tags_links, "tags");
    links = get_attr(tags_links, "links");
    if (tags == NULL || links == NULL) {
        Py_DECREF(meta);
        Py_XDECREF(tags);
        Py_XDECREF(links);
        return NULL;
    }
    final_tags_links = finalize_tags_links_impl(self, tags, links);
    Py_DECREF(tags);
    Py_DECREF(links);
    if (final_tags_links == NULL) {
        Py_DECREF(meta);
        return NULL;
    }
    rv = PyObject_CallFunctionObjArgs(g_note_type, meta, date, account, comment,
                                      PyTuple_GET_ITEM(final_tags_links, 0),
                                      PyTuple_GET_ITEM(final_tags_links, 1),
                                      NULL);
    Py_DECREF(final_tags_links);
    Py_DECREF(meta);
    return rv;
}

static PyObject*
builder_document(PyObject* module, PyObject* args)
{
    PyObject *self, *filename, *date, *account, *document_filename, *tags_links, *kvlist;
    int lineno;
    PyObject* meta;
    PyObject* tags;
    PyObject* links;
    PyObject* final_tags_links;
    PyObject* doc_filename = NULL;
    PyObject* rv;
    int is_abs;

    if (!PyArg_ParseTuple(args, "OOiOOOOO", &self, &filename, &lineno, &date, &account, &document_filename, &tags_links, &kvlist)) {
        return NULL;
    }
    meta = new_metadata_call(filename, lineno, kvlist);
    if (meta == NULL) {
        return NULL;
    }
    is_abs = PyObject_IsTrue(PyObject_CallFunctionObjArgs(g_path_isabs, document_filename, NULL));
    if (is_abs < 0) {
        Py_DECREF(meta);
        return NULL;
    }
    if (is_abs) {
        doc_filename = Py_NewRef(document_filename);
    } else {
        PyObject* dirname = PyObject_CallFunctionObjArgs(g_path_dirname, filename, NULL);
        PyObject* joined;
        if (dirname == NULL) {
            Py_DECREF(meta);
            return NULL;
        }
        joined = PyObject_CallFunctionObjArgs(g_path_join, dirname, document_filename, NULL);
        Py_DECREF(dirname);
        if (joined == NULL) {
            Py_DECREF(meta);
            return NULL;
        }
        doc_filename = PyObject_CallFunctionObjArgs(g_path_abspath, joined, NULL);
        Py_DECREF(joined);
        if (doc_filename == NULL) {
            Py_DECREF(meta);
            return NULL;
        }
    }
    tags = get_attr(tags_links, "tags");
    links = get_attr(tags_links, "links");
    if (tags == NULL || links == NULL) {
        Py_DECREF(meta);
        Py_DECREF(doc_filename);
        Py_XDECREF(tags);
        Py_XDECREF(links);
        return NULL;
    }
    final_tags_links = finalize_tags_links_impl(self, tags, links);
    Py_DECREF(tags);
    Py_DECREF(links);
    if (final_tags_links == NULL) {
        Py_DECREF(meta);
        Py_DECREF(doc_filename);
        return NULL;
    }
    rv = PyObject_CallFunctionObjArgs(g_document_type, meta, date, account, doc_filename,
                                      PyTuple_GET_ITEM(final_tags_links, 0),
                                      PyTuple_GET_ITEM(final_tags_links, 1),
                                      NULL);
    Py_DECREF(final_tags_links);
    Py_DECREF(doc_filename);
    Py_DECREF(meta);
    return rv;
}

static PyObject*
builder_custom(PyObject* module, PyObject* args)
{
    PyObject *self, *filename, *date, *dir_type, *custom_values, *kvlist;
    int lineno;
    PyObject* values[3];
    if (!PyArg_ParseTuple(args, "OOiOOOO", &self, &filename, &lineno, &date, &dir_type, &custom_values, &kvlist)) {
        return NULL;
    }
    values[0] = date;
    values[1] = dir_type;
    values[2] = custom_values;
    return simple_entry(g_custom_type, filename, lineno, kvlist, 3, values);
}

static PyObject*
builder_custom_value(PyObject* module, PyObject* args)
{
    PyObject *self, *filename, *value, *dtype = Py_None;
    int lineno;
    if (!PyArg_ParseTuple(args, "OOiO|O", &self, &filename, &lineno, &value, &dtype)) {
        return NULL;
    }
    if (cache_grammar_symbols() < 0) {
        return NULL;
    }
    if (dtype == Py_None) {
        dtype = (PyObject*)Py_TYPE(value);
    }
    return PyObject_CallFunctionObjArgs(g_value_type_type, value, dtype, NULL);
}

static PyObject*
builder_key_value(PyObject* module, PyObject* args)
{
    PyObject *self, *filename, *key, *value;
    int lineno;
    if (!PyArg_ParseTuple(args, "OOiOO", &self, &filename, &lineno, &key, &value)) {
        return NULL;
    }
    if (cache_grammar_symbols() < 0) {
        return NULL;
    }
    return PyObject_CallFunctionObjArgs(g_key_value_type, key, value, NULL);
}

static PyObject*
builder_posting(PyObject* module, PyObject* args)
{
    PyObject *self, *filename, *account, *units, *cost, *price, *istotal;
    int lineno;
    signed char flag;
    PyObject* meta;
    int istotal_bool;

    if (!PyArg_ParseTuple(args, "OOiOOOOOb", &self, &filename, &lineno, &account, &units, &cost, &price, &istotal, &flag)) {
        return NULL;
    }
    meta = new_metadata_call(filename, lineno, NULL);
    if (meta == NULL) {
        return NULL;
    }

    if (price != Py_None) {
        PyObject* price_number = get_attr(price, "number");
        if (price_number == NULL) {
            Py_DECREF(meta);
            return NULL;
        }
        if (PyObject_IsInstance(price_number, decimal_type) && PyObject_RichCompareBool(price_number, g_zero, Py_LT) == 1) {
            PyObject* abs_number;
            PyObject* currency;
            PyObject* msg = PyUnicode_FromFormat(
                "Negative prices are not allowed: %S (see http://furius.ca/beancount/doc/bug-negative-prices for workaround)",
                price);
            if (msg == NULL) {
                Py_DECREF(price_number);
                Py_DECREF(meta);
                return NULL;
            }
            if (append_simple_error(self, g_parser_error_type, filename, lineno, msg, NULL) < 0) {
                Py_DECREF(msg);
                Py_DECREF(price_number);
                Py_DECREF(meta);
                return NULL;
            }
            Py_DECREF(msg);
            abs_number = PyNumber_Absolute(price_number);
            currency = get_attr(price, "currency");
            Py_DECREF(price_number);
            if (abs_number == NULL || currency == NULL) {
                Py_XDECREF(abs_number);
                Py_XDECREF(currency);
                Py_DECREF(meta);
                return NULL;
            }
            price = PyObject_CallFunctionObjArgs(g_amount_type, abs_number, currency, NULL);
            Py_DECREF(abs_number);
            Py_DECREF(currency);
            if (price == NULL) {
                Py_DECREF(meta);
                return NULL;
            }
        } else {
            Py_DECREF(price_number);
            Py_INCREF(price);
        }
    } else {
        Py_INCREF(price);
    }

    istotal_bool = PyObject_IsTrue(istotal);
    if (istotal_bool < 0) {
        Py_DECREF(price);
        Py_DECREF(meta);
        return NULL;
    }
    if (istotal_bool) {
        PyObject* units_number = get_attr(units, "number");
        if (units_number == NULL) {
            Py_DECREF(price);
            Py_DECREF(meta);
            return NULL;
        }
        if (units_number == g_missing) {
            PyObject* msg = PyUnicode_FromFormat("Total price on a posting without units: %S.", price);
            Py_DECREF(units_number);
            if (msg == NULL) {
                Py_DECREF(price);
                Py_DECREF(meta);
                return NULL;
            }
            if (append_simple_error(self, g_parser_error_type, filename, lineno, msg, NULL) < 0) {
                Py_DECREF(msg);
                Py_DECREF(price);
                Py_DECREF(meta);
                return NULL;
            }
            Py_DECREF(msg);
            Py_DECREF(price);
            price = Py_None;
            Py_INCREF(price);
        } else {
            PyObject* price_number = get_attr(price, "number");
            if (price_number == NULL) {
                Py_DECREF(units_number);
                Py_DECREF(price);
                Py_DECREF(meta);
                return NULL;
            }
            if (price_number != g_missing) {
                PyObject* denom;
                PyObject* effective;
                PyObject* currency;
                if (PyObject_RichCompareBool(units_number, g_zero, Py_EQ) == 1) {
                    effective = Py_NewRef(g_zero);
                } else {
                    denom = PyNumber_Absolute(units_number);
                    if (denom == NULL) {
                        Py_DECREF(units_number);
                        Py_DECREF(price_number);
                        Py_DECREF(price);
                        Py_DECREF(meta);
                        return NULL;
                    }
                    effective = PyNumber_TrueDivide(price_number, denom);
                    Py_DECREF(denom);
                    if (effective == NULL) {
                        Py_DECREF(units_number);
                        Py_DECREF(price_number);
                        Py_DECREF(price);
                        Py_DECREF(meta);
                        return NULL;
                    }
                }
                currency = get_attr(price, "currency");
                if (currency == NULL) {
                    Py_DECREF(effective);
                    Py_DECREF(units_number);
                    Py_DECREF(price_number);
                    Py_DECREF(price);
                    Py_DECREF(meta);
                    return NULL;
                }
                Py_DECREF(price);
                price = PyObject_CallFunctionObjArgs(g_amount_type, effective, currency, NULL);
                Py_DECREF(effective);
                Py_DECREF(currency);
                if (price == NULL) {
                    Py_DECREF(units_number);
                    Py_DECREF(price_number);
                    Py_DECREF(meta);
                    return NULL;
                }
            }
            Py_DECREF(price_number);
            Py_DECREF(units_number);
        }
    }

    if (cost != Py_None && price != Py_None) {
        PyObject* cost_currency = get_attr(cost, "currency");
        PyObject* price_currency = get_attr(price, "currency");
        int mismatch = 0;
        if (cost_currency == NULL || price_currency == NULL) {
            Py_XDECREF(cost_currency);
            Py_XDECREF(price_currency);
            Py_DECREF(price);
            Py_DECREF(meta);
            return NULL;
        }
        if (PyUnicode_Check(cost_currency) && PyUnicode_Check(price_currency)) {
            mismatch = PyObject_RichCompareBool(cost_currency, price_currency, Py_NE);
        }
        if (mismatch == 1) {
            PyObject* msg = PyUnicode_FromFormat("Cost and price currencies must match: %S != %S",
                                                 cost_currency, price_currency);
            if (msg == NULL) {
                Py_DECREF(cost_currency);
                Py_DECREF(price_currency);
                Py_DECREF(price);
                Py_DECREF(meta);
                return NULL;
            }
            if (append_simple_error(self, g_parser_error_type, filename, lineno, msg, NULL) < 0) {
                Py_DECREF(msg);
                Py_DECREF(cost_currency);
                Py_DECREF(price_currency);
                Py_DECREF(price);
                Py_DECREF(meta);
                return NULL;
            }
            Py_DECREF(msg);
        } else if (mismatch < 0) {
            Py_DECREF(cost_currency);
            Py_DECREF(price_currency);
            Py_DECREF(price);
            Py_DECREF(meta);
            return NULL;
        }
        Py_DECREF(cost_currency);
        Py_DECREF(price_currency);
    }

    PyObject* flag_obj = flag ? PyUnicode_FromStringAndSize((const char*)&flag, 1) : Py_None;
    if (flag_obj == NULL) {
        Py_DECREF(price);
        Py_DECREF(meta);
        return NULL;
    }
    if (flag == 0) {
        Py_INCREF(flag_obj);
    }
    PyObject* posting = PyObject_CallFunctionObjArgs(g_posting_type, account, units, cost, price, flag_obj, meta, NULL);
    Py_DECREF(flag_obj);
    Py_DECREF(price);
    Py_DECREF(meta);
    return posting;
}

static PyObject*
builder_tag_link_new(PyObject* module, PyObject* args)
{
    if (cache_grammar_symbols() < 0) {
        return NULL;
    }
    PyObject* tags = PySet_New(NULL);
    PyObject* links = PySet_New(NULL);
    PyObject* rv;
    if (tags == NULL || links == NULL) {
        Py_XDECREF(tags);
        Py_XDECREF(links);
        return NULL;
    }
    rv = PyObject_CallFunctionObjArgs(g_tags_links_type, tags, links, NULL);
    Py_DECREF(tags);
    Py_DECREF(links);
    return rv;
}

static PyObject*
builder_tag_link_tag(PyObject* module, PyObject* args)
{
    PyObject *self, *filename, *tags_links, *tag;
    int lineno;
    PyObject* tags;
    if (!PyArg_ParseTuple(args, "OOiOO", &self, &filename, &lineno, &tags_links, &tag)) {
        return NULL;
    }
    tags = get_attr(tags_links, "tags");
    if (tags == NULL) {
        return NULL;
    }
    if (PySet_Add(tags, tag) < 0) {
        Py_DECREF(tags);
        return NULL;
    }
    Py_DECREF(tags);
    Py_INCREF(tags_links);
    return tags_links;
}

static PyObject*
builder_tag_link_link(PyObject* module, PyObject* args)
{
    PyObject *self, *filename, *tags_links, *link;
    int lineno;
    PyObject* links;
    if (!PyArg_ParseTuple(args, "OOiOO", &self, &filename, &lineno, &tags_links, &link)) {
        return NULL;
    }
    links = get_attr(tags_links, "links");
    if (links == NULL) {
        return NULL;
    }
    if (PySet_Add(links, link) < 0) {
        Py_DECREF(links);
        return NULL;
    }
    Py_DECREF(links);
    Py_INCREF(tags_links);
    return tags_links;
}

static PyObject*
builder_unpack_txn_strings(PyObject* module, PyObject* args)
{
    PyObject *self, *txn_strings, *meta;
    Py_ssize_t num_strings;
    if (!PyArg_ParseTuple(args, "OOO", &self, &txn_strings, &meta)) {
        return NULL;
    }
    if (cache_grammar_symbols() < 0) {
        return NULL;
    }
    num_strings = txn_strings == Py_None ? 0 : PyList_Size(txn_strings);
    if (num_strings < 0) {
        return NULL;
    }
    if (num_strings == 1) {
        return PyTuple_Pack(2, Py_None, PyList_GetItem(txn_strings, 0));
    }
    if (num_strings == 2) {
        return PyTuple_Pack(2, PyList_GetItem(txn_strings, 0), PyList_GetItem(txn_strings, 1));
    }
    if (num_strings == 0) {
        PyObject* empty = PyUnicode_FromString("");
        PyObject* rv;
        if (empty == NULL) {
            return NULL;
        }
        rv = PyTuple_Pack(2, Py_None, empty);
        Py_DECREF(empty);
        return rv;
    }
    PyObject* msg = PyUnicode_FromFormat("Too many strings on transaction description: %S", txn_strings);
    if (msg == NULL) {
        return NULL;
    }
    if (append_error(self, PyObject_CallFunctionObjArgs(g_parser_error_type, meta, msg, NULL)) < 0) {
        Py_DECREF(msg);
        return NULL;
    }
    Py_DECREF(msg);
    Py_RETURN_NONE;
}

static PyObject*
builder_finalize_tags_links(PyObject* module, PyObject* args)
{
    PyObject *self, *tags, *links;
    if (!PyArg_ParseTuple(args, "OOO", &self, &tags, &links)) {
        return NULL;
    }
    return finalize_tags_links_impl(self, tags, links);
}

static PyObject*
builder_transaction(PyObject* module, PyObject* args)
{
    PyObject *self, *filename, *date, *txn_strings, *tags_links, *posting_or_kv_list;
    int lineno;
    signed char flag;
    PyObject* meta;
    PyObject* explicit_meta;
    PyObject* postings;
    PyObject* tags;
    PyObject* links;
    PyObject* last_posting = NULL;
    PyObject* final_tags_links;
    PyObject* payee_narration;
    Py_ssize_t i, size;

    if (!PyArg_ParseTuple(args, "OOiObOOO", &self, &filename, &lineno, &date, &flag, &txn_strings, &tags_links, &posting_or_kv_list)) {
        return NULL;
    }
    if (cache_grammar_symbols() < 0) {
        return NULL;
    }
    meta = new_metadata_call(filename, lineno, NULL);
    if (meta == NULL) {
        return NULL;
    }
    explicit_meta = PyDict_New();
    postings = PyList_New(0);
    tags = get_attr(tags_links, "tags");
    links = get_attr(tags_links, "links");
    if (explicit_meta == NULL || postings == NULL || tags == NULL || links == NULL) {
        Py_DECREF(meta);
        Py_XDECREF(explicit_meta);
        Py_XDECREF(postings);
        Py_XDECREF(tags);
        Py_XDECREF(links);
        return NULL;
    }

    if (posting_or_kv_list != Py_None) {
        size = PyList_Size(posting_or_kv_list);
        if (size < 0) {
            goto error;
        }
        for (i = 0; i < size; i++) {
            PyObject* item = PyList_GetItem(posting_or_kv_list, i);
            int is_posting = PyObject_IsInstance(item, g_posting_type);
            if (is_posting < 0) {
                goto error;
            }
            if (is_posting) {
                if (PyList_Append(postings, item) < 0) {
                    goto error;
                }
                Py_XSETREF(last_posting, Py_NewRef(item));
                continue;
            }

            int is_tags_links = PyObject_IsInstance(item, g_tags_links_type);
            if (is_tags_links < 0) {
                goto error;
            }
            if (is_tags_links) {
                if (PyList_Size(postings) > 0) {
                    PyObject* msg = PyUnicode_FromFormat("Tags or links not allowed after first Posting: %S", item);
                    if (msg == NULL) {
                        goto error;
                    }
                    if (append_simple_error(self, g_parser_error_type, filename, lineno, msg, NULL) < 0) {
                        Py_DECREF(msg);
                        goto error;
                    }
                    Py_DECREF(msg);
                } else {
                    PyObject* rv = PyObject_CallMethod(tags, "update", "O", get_attr(item, "tags"));
                    PyObject* rv2 = PyObject_CallMethod(links, "update", "O", get_attr(item, "links"));
                    if (rv == NULL || rv2 == NULL) {
                        Py_XDECREF(rv);
                        Py_XDECREF(rv2);
                        goto error;
                    }
                    Py_DECREF(rv);
                    Py_DECREF(rv2);
                }
                continue;
            }

            PyObject* key = get_attr(item, "key");
            PyObject* value = get_attr(item, "value");
            PyObject* existing;
            if (key == NULL || value == NULL) {
                Py_XDECREF(key);
                Py_XDECREF(value);
                goto error;
            }
            if (last_posting == NULL) {
                existing = PyDict_SetDefault(explicit_meta, key, value);
                if (existing == NULL) {
                    Py_DECREF(key);
                    Py_DECREF(value);
                    goto error;
                }
                if (existing != value) {
                    PyObject* msg = PyUnicode_FromFormat("Duplicate metadata field on entry: %S", item);
                    if (msg == NULL) {
                        Py_DECREF(key);
                        Py_DECREF(value);
                        goto error;
                    }
                    if (append_simple_error(self, g_parser_error_type, filename, lineno, msg, NULL) < 0) {
                        Py_DECREF(msg);
                        Py_DECREF(key);
                        Py_DECREF(value);
                        goto error;
                    }
                    Py_DECREF(msg);
                }
            } else {
                PyObject* posting_meta = get_attr(last_posting, "meta");
                if (posting_meta == Py_None) {
                    PyObject* replacement = PyDict_New();
                    PyObject* updated;
                    if (replacement == NULL) {
                        Py_DECREF(posting_meta);
                        Py_DECREF(key);
                        Py_DECREF(value);
                        goto error;
                    }
                    updated = PyObject_CallMethod(last_posting, "_replace", "sO", "meta", replacement);
                    Py_DECREF(replacement);
                    Py_DECREF(posting_meta);
                    if (updated == NULL) {
                        Py_DECREF(key);
                        Py_DECREF(value);
                        goto error;
                    }
                    if (PySequence_DelItem(postings, PyList_Size(postings) - 1) < 0) {
                        Py_DECREF(updated);
                        Py_DECREF(key);
                        Py_DECREF(value);
                        goto error;
                    }
                    if (PyList_Append(postings, updated) < 0) {
                        Py_DECREF(updated);
                        Py_DECREF(key);
                        Py_DECREF(value);
                        goto error;
                    }
                    Py_XSETREF(last_posting, updated);
                    posting_meta = get_attr(last_posting, "meta");
                } else {
                    Py_DECREF(posting_meta);
                    posting_meta = get_attr(last_posting, "meta");
                }
                if (posting_meta == NULL) {
                    Py_DECREF(key);
                    Py_DECREF(value);
                    goto error;
                }
                existing = PyDict_SetDefault(posting_meta, key, value);
                Py_DECREF(posting_meta);
                if (existing == NULL) {
                    Py_DECREF(key);
                    Py_DECREF(value);
                    goto error;
                }
                if (existing != value) {
                    PyObject* msg = PyUnicode_FromFormat("Duplicate posting metadata field: %S", item);
                    if (msg == NULL) {
                        Py_DECREF(key);
                        Py_DECREF(value);
                        goto error;
                    }
                    if (append_simple_error(self, g_parser_error_type, filename, lineno, msg, NULL) < 0) {
                        Py_DECREF(msg);
                        Py_DECREF(key);
                        Py_DECREF(value);
                        goto error;
                    }
                    Py_DECREF(msg);
                }
            }
            Py_DECREF(key);
            Py_DECREF(value);
        }
    }

    final_tags_links = finalize_tags_links_impl(self, tags, links);
    if (final_tags_links == NULL) {
        goto error;
    }

    PyObject* active_meta = get_attr(self, "meta");
    if (active_meta == NULL) {
        Py_DECREF(final_tags_links);
        goto error;
    }
    if (PyObject_IsTrue(active_meta)) {
        PyObject *key, *value_list;
        Py_ssize_t pos = 0;
        while (PyDict_Next(active_meta, &pos, &key, &value_list)) {
            PyObject* value = PyList_GetItem(value_list, PyList_Size(value_list) - 1);
            if (value == NULL || PyDict_SetItem(meta, key, value) < 0) {
                Py_DECREF(active_meta);
                Py_DECREF(final_tags_links);
                goto error;
            }
        }
    }
    Py_DECREF(active_meta);

    if (PyObject_IsTrue(explicit_meta)) {
        PyObject* rv = PyObject_CallMethod(meta, "update", "O", explicit_meta);
        if (rv == NULL) {
            Py_DECREF(final_tags_links);
            goto error;
        }
        Py_DECREF(rv);
    }

    payee_narration = builder_unpack_txn_strings(module, Py_BuildValue("OOO", self, txn_strings, meta));
    if (payee_narration == NULL) {
        Py_DECREF(final_tags_links);
        goto error;
    }
    if (payee_narration == Py_None) {
        Py_DECREF(payee_narration);
        Py_DECREF(final_tags_links);
        goto error_none;
    }

    PyObject* flag_obj = PyUnicode_FromStringAndSize((const char*)&flag, 1);
    PyObject* rv = PyObject_CallFunctionObjArgs(g_transaction_type,
                                                meta,
                                                date,
                                                flag_obj,
                                                PyTuple_GET_ITEM(payee_narration, 0),
                                                PyTuple_GET_ITEM(payee_narration, 1),
                                                PyTuple_GET_ITEM(final_tags_links, 0),
                                                PyTuple_GET_ITEM(final_tags_links, 1),
                                                postings,
                                                NULL);
    Py_DECREF(flag_obj);
    Py_DECREF(payee_narration);
    Py_DECREF(final_tags_links);
    Py_DECREF(tags);
    Py_DECREF(links);
    Py_DECREF(explicit_meta);
    Py_DECREF(postings);
    Py_DECREF(meta);
    Py_XDECREF(last_posting);
    return rv;

error:
    Py_DECREF(tags);
    Py_DECREF(links);
error_none:
    Py_DECREF(explicit_meta);
    Py_DECREF(postings);
    Py_DECREF(meta);
    Py_XDECREF(last_posting);
    Py_RETURN_NONE;
}

static PyMethodDef module_functions[] = {
    {"account", builder_account, METH_VARARGS, NULL},
    {"pipe_deprecated_error", builder_pipe_deprecated_error, METH_VARARGS, NULL},
    {"pushtag", builder_pushtag, METH_VARARGS, NULL},
    {"poptag", builder_poptag, METH_VARARGS, NULL},
    {"pushmeta", builder_pushmeta, METH_VARARGS, NULL},
    {"popmeta", builder_popmeta, METH_VARARGS, NULL},
    {"option", builder_option, METH_VARARGS, NULL},
    {"include", builder_include, METH_VARARGS, NULL},
    {"plugin", builder_plugin, METH_VARARGS, NULL},
    {"amount", builder_amount, METH_VARARGS, NULL},
    {"compound_amount", builder_compound_amount, METH_VARARGS, NULL},
    {"cost_merge", builder_cost_merge, METH_VARARGS, NULL},
    {"cost_spec", builder_cost_spec, METH_VARARGS, NULL},
    {"handle_list", builder_handle_list, METH_VARARGS, NULL},
    {"open", builder_open, METH_VARARGS, NULL},
    {"close", builder_close, METH_VARARGS, NULL},
    {"commodity", builder_commodity, METH_VARARGS, NULL},
    {"pad", builder_pad, METH_VARARGS, NULL},
    {"balance", builder_balance, METH_VARARGS, NULL},
    {"event", builder_event, METH_VARARGS, NULL},
    {"query", builder_query, METH_VARARGS, NULL},
    {"price", builder_price, METH_VARARGS, NULL},
    {"note", builder_note, METH_VARARGS, NULL},
    {"document", builder_document, METH_VARARGS, NULL},
    {"custom", builder_custom, METH_VARARGS, NULL},
    {"custom_value", builder_custom_value, METH_VARARGS, NULL},
    {"key_value", builder_key_value, METH_VARARGS, NULL},
    {"posting", builder_posting, METH_VARARGS, NULL},
    {"tag_link_new", builder_tag_link_new, METH_VARARGS, NULL},
    {"tag_link_TAG", builder_tag_link_tag, METH_VARARGS, NULL},
    {"tag_link_LINK", builder_tag_link_link, METH_VARARGS, NULL},
    {"_unpack_txn_strings", builder_unpack_txn_strings, METH_VARARGS, NULL},
    {"_finalize_tags_links", builder_finalize_tags_links, METH_VARARGS, NULL},
    {"transaction", builder_transaction, METH_VARARGS, NULL},
    {NULL, NULL, 0, NULL}
};

static struct PyModuleDef moduledef = {
    PyModuleDef_HEAD_INIT,
    "_grammar",
    "Beancount grammar helpers",
    -1,
    module_functions,
    NULL,
    NULL,
    NULL,
    NULL,
};

static PyObject*
import_attr(const char* module_name, const char* attr_name)
{
    PyObject* module = PyImport_ImportModule(module_name);
    PyObject* attr;
    if (module == NULL) {
        return NULL;
    }
    attr = PyObject_GetAttrString(module, attr_name);
    Py_DECREF(module);
    return attr;
}

PyMODINIT_FUNC
PyInit__grammar(void)
{
    PyObject* module;
    PyDateTime_IMPORT;
    PyDecimal_IMPORT;

    g_traceback_format_exception_only = import_attr("traceback", "format_exception_only");
    g_traceback_extract_tb = import_attr("traceback", "extract_tb");
    g_options_defaults = import_attr("beancount.parser.options", "OPTIONS_DEFAULTS");
    g_options_read_only = import_attr("beancount.parser.options", "READ_ONLY_OPTIONS");
    g_options_options = import_attr("beancount.parser.options", "OPTIONS");
    g_display_context_type = import_attr("beancount.core.display_context", "DisplayContext");
    g_entry_sortkey = import_attr("beancount.core.data", "entry_sortkey");
    g_new_metadata = import_attr("beancount.core.data", "new_metadata");
    g_amount_type = import_attr("beancount.core.amount", "Amount");
    g_empty_set = import_attr("beancount.core.data", "EMPTY_SET");
    g_balance_type = import_attr("beancount.core.data", "Balance");
    g_booking_enum = import_attr("beancount.core.data", "Booking");
    g_close_type = import_attr("beancount.core.data", "Close");
    g_commodity_type = import_attr("beancount.core.data", "Commodity");
    g_custom_type = import_attr("beancount.core.data", "Custom");
    g_document_type = import_attr("beancount.core.data", "Document");
    g_event_type = import_attr("beancount.core.data", "Event");
    g_note_type = import_attr("beancount.core.data", "Note");
    g_open_type = import_attr("beancount.core.data", "Open");
    g_pad_type = import_attr("beancount.core.data", "Pad");
    g_posting_type = import_attr("beancount.core.data", "Posting");
    g_price_type = import_attr("beancount.core.data", "Price");
    g_query_type = import_attr("beancount.core.data", "Query");
    g_transaction_type = import_attr("beancount.core.data", "Transaction");
    g_missing = import_attr("beancount.core.number", "MISSING");
    g_zero = import_attr("beancount.core.number", "ZERO");
    g_cost_spec_type = import_attr("beancount.core.position", "CostSpec");
    g_path_isabs = import_attr("os.path", "isabs");
    g_path_abspath = import_attr("os.path", "abspath");
    g_path_join = import_attr("os.path", "join");
    g_path_dirname = import_attr("os.path", "dirname");

    if (g_options_defaults == NULL || g_options_read_only == NULL || g_options_options == NULL ||
        g_new_metadata == NULL || g_amount_type == NULL || g_empty_set == NULL ||
        g_balance_type == NULL || g_booking_enum == NULL || g_close_type == NULL ||
        g_commodity_type == NULL || g_custom_type == NULL || g_document_type == NULL ||
        g_event_type == NULL || g_note_type == NULL || g_open_type == NULL ||
        g_pad_type == NULL || g_posting_type == NULL || g_price_type == NULL ||
        g_query_type == NULL || g_transaction_type == NULL || g_missing == NULL ||
        g_zero == NULL || g_cost_spec_type == NULL || g_path_isabs == NULL ||
        g_path_abspath == NULL || g_path_join == NULL || g_path_dirname == NULL) {
        return NULL;
    }

    module = PyModule_Create(&moduledef);
    return module;
}

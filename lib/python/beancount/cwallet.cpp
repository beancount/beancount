/**
 * A fast wallet object implemented in C
 * Most of the slowless in beancount is due to the wallet operations.
 *
 * Author: Martin Blais <blais@furius.ca>
 */

#include <Python.h>


/*PyObject* Wallet_getter_mid(Object* self, void* args)
{
    return toPython(((Wallet*)(Object_get(self)))->mid);
}

int Wallet_setter_mid(Object* self, PyObject* value, void* closure)
{
    return fromPython(value, ((Wallet*)(Object_get(self)))->mid);
}
*/

PyGetSetDef Wallet_getset[] = {
    /*{(char*)"mid", (getter)Wallet_getter_mid, (setter)Wallet_setter_mid, NULL},*/
    {NULL}
};

PyObject* Wallet_new(PyTypeObject* type, PyObject* args, PyObject* kwds)
{
    PyObject* self = _PyObject_New(type);
    if (self == NULL) {
        return NULL;
    }
    return self;
}

void Wallet_dealloc(PyObject* self)
{
    PyObject_Del(self);
}

PyObject* Wallet_str(PyObject* self)
{
    return PyString_FromStringAndSize("<Wallet>", 8);
}

#if 0 
PyObject* Wallet_dump(PyObject* self, PyObject* offset_)
{
    if ( !PyInt_Check(offset_) ) {
        return NULL;
    }
    long offset = PyInt_AS_LONG(offset_);
    return Wallet_render(self, true, offset);
}
#endif

int Wallet_cmp(PyObject* o1, PyObject* o2)
{
/*FIXME: TODO*/
    if ( o1 == o2 ) {
        return 0;
    }
    return 1;
}

PyMethodDef Wallet_methods[] = {
    /*{"_dump", (PyCFunction)Wallet_dump, METH_O, "Render a text version of this message."},*/
    {NULL}
};

PyTypeObject Wallet_Type = {
    PyObject_HEAD_INIT(NULL)
    0,                      /* ob_size */
    "Wallet",               /* tp_name */
    sizeof(PyObject),       /* tp_basicsize */
    0,                      /* tp_itemsize */
    Wallet_dealloc,         /* tp_dealloc */
    0,                      /* tp_print */
    0,                      /* tp_getattr */
    0,                      /* tp_setattr */
    Wallet_cmp,             /* tp_compare */
    Wallet_str,             /* tp_repr */
    0,                      /* tp_as_number */
    0,                      /* tp_as_sequence */
    0,                      /* tp_as_mapping */
    0,                      /* tp_hash  */
    0,                      /* tp_call */
    Wallet_str,             /* tp_str */
    0,                      /* tp_getattro */
    0,                      /* tp_setattro */
    0,                      /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,     /* tp_flags */
    "A fast Wallet class.", /* tp_doc */
    0,                      /* tp_traverse */
    0,                      /* tp_clear */
    0,                      /* tp_richcompare */
    0,                      /* tp_weaklistoffset */
    0,                      /* tp_iter */
    0,                      /* tp_iternext */
    Wallet_methods,         /* tp_methods */
    0,                      /* tp_members */
    Wallet_getset,          /* tp_getset */
    0,                      /* tp_base */
    0,                      /* tp_dict */
    0,                      /* tp_descr_get */
    0,                      /* tp_descr_set */
    0,                      /* tp_dictoffset */
    0,                      /* tp_init */
    0,                      /* tp_alloc */
    Wallet_new,             /* tp_new */
};

static PyMethodDef module_methods[] = {
    {NULL, NULL, 0, NULL}
};

PyMODINIT_FUNC initcwallet(void)
{
    PyObject* module = Py_InitModule("cwallet", module_methods);

    if (PyType_Ready(&Wallet_Type) < 0)
        return;
    Py_INCREF((PyObject*)&Wallet_Type);
    PyModule_AddObject(module, "Wallet", (PyObject*)&Wallet_Type);
}


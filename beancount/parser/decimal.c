#define PY_SSIZE_T_CLEAN
#include <Python.h>

/**
 * A reference to the Decimal type object used to instantiate Decimal
 * objects. This avoid having to import the Python module and look up
 * the type object at each object instantiation.
 */
const PyObject* decimal_type;

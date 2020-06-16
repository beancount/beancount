#ifndef BEANCOUNT_DECIMAL_H
#define BEANCOUNT_DECIMAL_H

/**
 * Python does not expose a C-API to create @Decimal objects as it
 * does for example for the @date and @datetime objects.  This code
 * tries to implement something similar.  The exposed API is similar
 * to the one implemented in the _decimal module implementation.
 * Before calling these functions the macro @PyDecimal_IMPORT must be
 * invoked.
 */

static PyObject* PyDec_Type = NULL;

#define PyDecimal_IMPORT do {                                           \
        PyObject* decimal = PyImport_ImportModule("decimal");           \
        PyDec_Type = PyObject_GetAttrString(decimal, "Decimal");        \
    } while (0)

#define PyDec_FromCString(str, len) PyObject_CallFunction(PyDec_Type, "s#", str, len)

#endif /* BEANCOUNT_DECIMAL_H */

#ifndef BEANCOUNT_DECIMAL_H
#define BEANCOUNT_DECIMAL_H

/**
 * Python does not expose a C-API to create Decimal objects. This code
 * implements a system similar to what Python provides for @date and
 * @datetime objects from the datetime module. The exposed API is
 * similar to the one implemented (but not exposed) by the _decimal
 * extension module. Before using any of these functions, the macro
 * PyDateTime_IMPORT must be invoked.
 */

extern PyObject* decimal_type;

/**
 * Initialization.
 */
#define PyDecimal_IMPORT do {                                           \
        PyObject* decimal = PyImport_ImportModule("decimal");           \
        decimal_type = PyObject_GetAttrString(decimal, "Decimal");      \
    } while (0)

/**
 * Return a Decimal object from a C-string representation.
 */
#define PyDec_FromCString(str, len) PyObject_CallFunction(decimal_type, "s#", str, len)

#endif /* BEANCOUNT_DECIMAL_H */

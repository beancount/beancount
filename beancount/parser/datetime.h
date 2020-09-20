#ifndef BEANCOUNT_DATETIME_H
#define BEANCOUNT_DATETIME_H

#include <Python.h>
#include <datetime.h>

/** 
 * PyDateTimeAPI is a static variable used in the Python C-API to the
 * datetime module. This forces PyDateTime_IMPORT() to be called in
 * every compilation module using the C datetime API.  Redefine
 * PyDateTimeAPI to point to a global variable, which needs to be
 * initialized only once.
 */
#define PyDateTimeAPI datetime_api

extern PyDateTime_CAPI* datetime_api;

#endif /* BEANCOUNT_DECIMAL_H */

#ifndef _BEANCOUNT_CCORE_ACCOUNT_MOD_H_
#define _BEANCOUNT_CCORE_ACCOUNT_MOD_H_

#include "pybind11/pybind11.h"

namespace beancount {

void ExportAccount(pybind11::module& mod);

}  // namespace beancount

#endif // _BEANCOUNT_CCORE_ACCOUNT_MOD_H_

#include "beancount/ccore/account_mod.h"

#include "pybind11/pybind11.h"

PYBIND11_MODULE(_core, mod) {
  mod.doc() = "Python bindings for Beancount core";
  beancount::ExportAccount(mod);
}

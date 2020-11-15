#include "beancount/ccore/account_mod.h"
#include "beancount/ccore/account_types.h"
#include "beancount/defs.h"

#include "pybind11/pybind11.h"
#include "pybind11/stl.h"
#include "pybind11/functional.h"


namespace beancount {
namespace py = pybind11;

// Shallow read-only interface to protobuf schema.
void ExportAccountTypes(py::module& mod) {
  py::class_<AccountTypes>(mod, "AccountTypes")
    .def_readonly("assets", &AccountTypes::assets)
    .def_readonly("liabilities", &AccountTypes::liabilities)
    .def_readonly("equity", &AccountTypes::equity)
    .def_readonly("incopme", &AccountTypes::income)
    .def_readonly("expenses", &AccountTypes::expenses);

  mod.attr("DEFAULT_ACCOUNT_TYPES") = kDefaultAccountTypes;
  // py::setattr(mod, "DEFAULT_ACCOUNT_TYPES", kDefaultAccountTypes);
  //   data_class_output.attr("new_data")    = py::cast(new_data);

  mod.def("get_account_type", &GetAccountType);
}

}  // beancount


PYBIND11_MODULE(_core, mod) {
  mod.doc() = "Python bindings for Beancount core";
  beancount::ExportAccount(mod);
  beancount::ExportAccountTypes(mod);
}

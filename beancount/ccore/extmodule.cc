#include "beancount/ccore/account.h"

#include "pybind11/pybind11.h"
#include "pybind11/stl.h"
#include "pybind11/functional.h"

namespace beancount {
namespace py = pybind11;

// Shallow read-only interface to protobuf schema.
void ExportAccount(py::module& mod) {
  mod.def("is_valid", &beancount::IsAccountValid,
          "Return true if the given string is a valid account name.");
  mod.def("join", &beancount::JoinAccount,
          "Join the names with the account separator.");
  mod.def("split", &beancount::SplitAccount,
          "Split an account's name into its components.");
}

}
  // namespace beancount


PYBIND11_MODULE(extmodule, mod) {
  mod.doc() = "Python bindings for Beancount";
  beancount::ExportAccount(mod);
}

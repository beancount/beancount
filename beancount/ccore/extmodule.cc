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
  mod.def("parent", &beancount::ParentAccount,
          "Return the name of the parent account of the given account.");
  mod.def("leaf", &beancount::LeafAccount,
          "Get the name of the leaf of this account.");
  mod.def("sans_root", &beancount::AccountSansRoot,
          "Get the name of the account without the root.");
  mod.def("root", &beancount::AccountRoot,
          "Return the first few components of an account's name.");
  mod.def("has_component", &beancount::HasAccountComponent,
          "Return true if one of the account contains a given component.");
  mod.def("commonprefix", &beancount::CommonPrefix,
          "Return the common prefix of a list of account names.");
}

}
  // namespace beancount


PYBIND11_MODULE(extmodule, mod) {
  mod.doc() = "Python bindings for Beancount";
  beancount::ExportAccount(mod);
}

#include "beancount/ccore/account.h"

#include <vector>
#include <string>

#include "pybind11/pybind11.h"
#include "pybind11/stl.h"
#include "pybind11/functional.h"

#include "beancount/defs.h"

namespace beancount {
namespace py = pybind11;

namespace {

// Adaptations of Python versions of C++ implementations.

string JoinAccount_(const py::args args) {
  vector<string_view> vargs;
  for (const auto& arg : args) {
    string_view sarg = arg.cast<string_view>();
    vargs.push_back(sarg);
  }
  return JoinAccount(vargs);
}

string_view AccountRoot_(int num_components, string_view account) {
  return AccountRoot(account, num_components);
}

optional<string> LeafAccount_(string_view account) {
  string result = LeafAccount(account);
  if (result.empty())
    return std::nullopt;
  return optional<string>{result};
}

optional<string> ParentAccount_(string_view account) {
  if (account.empty())
    return std::nullopt;
  string result = ParentAccount(account);
  return optional<string>{result};
}

}  // namespace

// Shallow read-only interface to protobuf schema.
void ExportAccount(py::module& mod) {
  mod.def("is_valid", &IsAccountValid,
          "Return true if the given string is a valid account name.");
  mod.def("join", &JoinAccount_,
          "Join the names with the account separator.");
  mod.def("split", &SplitAccount,
          "Split an account's name into its components.");
  mod.def("parent", &ParentAccount_,
          "Return the name of the parent account of the given account.");
  mod.def("leaf", &LeafAccount_,
          "Get the name of the leaf of this account.");
  mod.def("sans_root", &AccountSansRoot,
          "Get the name of the account without the root.");
  mod.def("root", &AccountRoot_,
          "Return the first few components of an account's name.");
  mod.def("has_component", &HasAccountComponent,
          "Return true if one of the account contains a given component.");
  mod.def("commonprefix", &CommonPrefix,
          "Return the common prefix of a list of account names.");
}

}  // beancount

#include "beancount/ccore/account.h"
#include "beancount/ccore/account_types.h"
#include "beancount/ccore/inventory.h"

#include <vector>
#include <string>

#include "pybind11/pybind11.h"
#include "pybind11/stl.h"
#include "pybind11/functional.h"


namespace beancount {
namespace py = pybind11;
using std::string;
using std::string_view;
using std::vector;
using std::optional;

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

namespace {

}  // namespace

void ExportAccountTypes(py::module& mod) {
  py::class_<AccountTypes>(mod, "AccountTypes")
    .def_property_readonly("assets", &AccountTypes::assets)
    .def_property_readonly("liabilities", &AccountTypes::liabilities)
    .def_property_readonly("equity", &AccountTypes::equity)
    .def_property_readonly("income", &AccountTypes::income)
    .def_property_readonly("expenses", &AccountTypes::expenses);

  mod.attr("DEFAULT_ACCOUNT_TYPES") = kDefaultAccountTypes;
  // py::setattr(mod, "DEFAULT_ACCOUNT_TYPES", kDefaultAccountTypes);
  //   data_class_output.attr("new_data")    = py::cast(new_data);

  mod.def("get_account_type", &GetAccountType);
  mod.def("get_account_sort_key", &GetAccountSortKey);
  mod.def("is_account_type", &IsAccountType);
  mod.def("is_root_account", &IsRootAccount);
  mod.def("is_balance_sheet_account", &IsBalanceSheetAccount);
  mod.def("is_income_statement_account", &IsIncomeStatementAccount);
  mod.def("is_equity_account", &IsEquityAccount);
  mod.def("get_account_sign", &GetAccountSign);
}

void ExportInventory(py::module& mod) {
  py::enum_<MatchResult>(mod, "MatchResult")
    .value("CREATED", MatchResult::CREATED)
    .value("REDUCED", MatchResult::REDUCED)
    .value("AUGMENTED", MatchResult::AUGMENTED)
    .value("IGNORED", MatchResult::IGNORED);

  py::class_<Inventory>(mod, "Inventory")
    .def(py::init<>())
    .def("__eq__", &Inventory::operator==);

  mod.def("from_string", &InventoryFromString);
}

}  // beancount


PYBIND11_MODULE(_core, mod) {
  mod.doc() = "Python bindings for Beancount core";
  beancount::ExportAccount(mod);
  beancount::ExportAccountTypes(mod);
  beancount::ExportInventory(mod);
}

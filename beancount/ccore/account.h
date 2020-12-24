// Functions that operate on account strings.
//
// These account objects are rather simple and dumb; they do not contain the list
// of their associated postings. This is achieved by building a realization; see
// realization.py for details.
//
// Copyright (C) 2013-2016,2020  Martin Blais
// License: "GNU GPLv2"

#ifndef BEANCOUNT_CCORE_ACCOUNT_H_
#define BEANCOUNT_CCORE_ACCOUNT_H_

#include "re2/re2.h"

#include <vector>
#include <string>

namespace beancount {

// Component separator for account names.
extern const char* kSep;

// Regular expression string that matches a valid account.
// TODO(blais): Can I bury this?
extern RE2 kAccountRE;

// Return true if the given string is a valid account name.
// This does not check for the root account types, just the general syntax.
bool IsAccountValid(std::string_view account);

// Join the names with the account separator.
std::string JoinAccount(const std::vector<std::string_view>& components);

// Split an account's name into its components.
std::vector<std::string> SplitAccount(std::string_view account);

// Return the name of the parent account of the given account.
std::string ParentAccount(std::string_view account);

// Get the name of the leaf of this account.
std::string LeafAccount(std::string_view account);

// Get the name of the account without the root. For example, an input of
// 'Assets:BofA:Checking' will produce 'BofA:Checking'.
std::string AccountSansRoot(std::string_view account);

// Return the first few components of an account's name. num_components is an
// integer, the number of components to return. Note that the lifetime of the
// returned value is tied to that of the "account" argument.
std::string_view AccountRoot(std::string_view account, int num_components);

// Return true if one of the account contains a given component. 'component' is
// A string, a component of an account name. For instance, ``Food`` in
// ``Expenses:Food:Restaurant``. All components are considered. Returns true if
// the component is in the account. Note that a component name must be whole,
// that is ``NY`` is not in ``Expenses:Taxes:StateNY``.
bool HasAccountComponent(std::string_view account, std::string_view component);

// Return the common prefix of a list of account names.
std::string CommonPrefix(const std::vector<std::string_view>& accounts);

// TODO(blais): Continue.



}  // namespace beancount

#endif // BEANCOUNT_CCORE_ACCOUNT_H_

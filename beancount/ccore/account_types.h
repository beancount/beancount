// Definition for global account types.
//
// This is where we keep the global account types value and definition.
//
// Note that it's unfortunate that we're using globals and side-effect here, but
// this is the best solution in the short-term, the account types are used
// in too many places to pass around that state everywhere. Maybe we change
// this later on.
//
// Copyright (C) 2020  Martin Blais
// License: "GNU GPLv2"

#ifndef _BEANCOUNT_CCORE_ACCOUNT_TYPES_H_
#define _BEANCOUNT_CCORE_ACCOUNT_TYPES_H_

#include <vector>
#include <string>

#include "beancount/defs.h"

namespace beancount {

// Container for account types.
// TODO(blais): Storing this in data.proto might make more sense?
struct AccountTypes {
  string assets;
  string liabilities;
  string equity;
  string income;
  string expenses;
};

// Default values for root accounts.
extern AccountTypes kDefaultAccountTypes;

// Return the type of this account's name.
//
// Warning: No check is made on the validity of the account type. This merely
// returns the root account of the corresponding account name.
string GetAccountType(string_view account_name);

// TODO(blais): Continue.

}  // namespace beancount

#endif // _BEANCOUNT_CCORE_ACCOUNT_TYPES_H_

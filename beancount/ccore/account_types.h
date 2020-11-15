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

#include "beancount/ccore/data.pb.h"
#include "beancount/defs.h"

namespace beancount {

// Default values for root accounts.
extern AccountTypes kDefaultAccountTypes;

// Return the type of this account's name.
//
// Warning: No check is made on the validity of the account type. This merely
// returns the root account of the corresponding account name.
string_view GetAccountType(string_view account_name);

// Return a tuple that can be used to order/sort account names.
pair<int, string_view> GetAccountSortKey(const AccountTypes& account_types,
                                         string_view account_name);

// Return the type of this account's name.
//
// Warning: No check is made on the validity of the account type. This merely
// returns the root account of the corresponding account name.
bool IsAccountType(string_view account_type, string_view account_name);

// Return true if the account name is a root account.
//
// This function does not verify whether the account root is a valid
// one, just that it is a root account or not.
bool IsRootAccount(string_view account_name);

// Return true if the given account is a balance sheet account.
// Assets, liabilities and equity accounts are balance sheet accounts.
bool IsBalanceSheetAccount(string_view account_name, const AccountTypes& account_types);


// Return true if the given account is an income statement account.
// Income and expense accounts are income statement accounts.
bool IsIncomeStatementAccount(string_view account_name, const AccountTypes& account_types);

// Return true if the given account is an equity account.
bool IsEquityAccount(string_view account_name, const AccountTypes& account_types);

// Return the sign of the normal balance of a particular account. +1 or -1,
// depending on the account's type.
int GetAccountSign(string_view account_name, const AccountTypes& account_types);

}  // namespace beancount

#endif // _BEANCOUNT_CCORE_ACCOUNT_TYPES_H_

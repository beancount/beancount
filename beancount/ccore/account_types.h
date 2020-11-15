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






// TODO(blais): Continue.
#if 0





def is_balance_sheet_account(account_name: Account, account_types: AccountTypes):
    """Return true if the given account is a balance sheet account.
    Assets, liabilities and equity accounts are balance sheet accounts.

    Args:
      account_name: A string, an account name.
      account_types: An instance of AccountTypes.
    Returns:
      A boolean, true if the account is a balance sheet account.
    """
    assert isinstance(account_name, str), "Account is not a string: {}".format(account_name)
    assert isinstance(account_types, AccountTypes), (
        "Account types has invalid type: {}".format(account_types))
    account_type = get_account_type(account_name)
    return account_type in (account_types.assets,
                            account_types.liabilities,
                            account_types.equity)


def is_income_statement_account(account_name: Account, account_types: AccountTypes):
    """Return true if the given account is an income statement account.
    Income and expense accounts are income statement accounts.

    Args:
      account_name: A string, an account name.
      account_types: An instance of AccountTypes.
    Returns:
      A boolean, true if the account is an income statement account.
    """
    assert isinstance(account_name, str), "Account is not a string: {}".format(account_name)
    assert isinstance(account_types, AccountTypes), (
        "Account types has invalid type: {}".format(account_types))
    account_type = get_account_type(account_name)
    return account_type in (account_types.income,
                            account_types.expenses)


def is_equity_account(account_name: Account, account_types: AccountTypes):
    """Return true if the given account is an equity account.

    Args:
      account_name: A string, an account name.
      account_types: An instance of AccountTypes.
    Returns:
      A boolean, true if the account is an equity account.
    """
    assert isinstance(account_name, str), "Account is not a string: {}".format(account_name)
    assert isinstance(account_types, AccountTypes), (
        "Account types has invalid type: {}".format(account_types))
    account_type = get_account_type(account_name)
    return account_type == account_types.equity


def get_account_sign(account_name: Account, account_types: AccountTypes=None):
    """Return the sign of the normal balance of a particular account.

    Args:
      account_name: A string, the name of the account whose sign is to return.
      account_types: An optional instance of the current account_types.
    Returns:
      +1 or -1, depending on the account's type.
    """
    if account_types is None:
        account_types = DEFAULT_ACCOUNT_TYPES
    assert isinstance(account_name, str), "Account is not a string: {}".format(account_name)
    account_type = get_account_type(account_name)
    return (+1
            if account_type in (account_types.assets,
                                account_types.expenses)
            else -1)
#endif

}  // namespace beancount

#endif // _BEANCOUNT_CCORE_ACCOUNT_TYPES_H_

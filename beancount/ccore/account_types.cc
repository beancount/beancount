// Copyright (C) 2020  Martin Blais
// License: "GNU GPLv2"

#include "beancount/ccore/account_types.h"
#include "beancount/ccore/account.h"

#include "absl/strings/str_split.h"

namespace beancount {

// Default values for root accounts.
const char* kAssets = "Assets";
const char* kLiabilities = "Liabilities";
const char* kEquity = "Equity";
const char* kIncome = "Income";
const char* kExpenses = "Expenses";

AccountTypes kDefaultAccountTypes = {
  .assets = kAssets,
  .liabilities = kLiabilities,
  .equity = kEquity,
  .income = kIncome,
  .expenses = kExpenses,
};

string GetAccountType(string_view account) {
  return string(*absl::StrSplit(account, kSep).begin());
}

// TODO(blais): Continue.

}  // namespace beancount

package core

import (
	"strings"
)

// AccountTypes contains the names of the root accounts.
type AccountTypes struct {
	Assets      string
	Liabilities string
	Equity      string
	Income      string
	Expenses    string
}

// DEFAULT_ACCOUNT_TYPES defines the default root account names.
var DEFAULT_ACCOUNT_TYPES = AccountTypes{
	Assets:      "Assets",
	Liabilities: "Liabilities",
	Equity:      "Equity",
	Income:      "Income",
	Expenses:    "Expenses",
}

// Index returns the index of the account type.
func (a AccountTypes) Index(accountType string) int {
	switch accountType {
	case a.Assets:
		return 0
	case a.Liabilities:
		return 1
	case a.Equity:
		return 2
	case a.Income:
		return 3
	case a.Expenses:
		return 4
	default:
		return -1
	}
}

// GetAccountType returns the root account of the given account name.
func GetAccountType(accountName string) string {
	components := Split(accountName)
	if len(components) == 0 {
		return ""
	}
	return components[0]
}

// GetAccountSortKey returns a value that can be used to sort account names.
type AccountSortKey struct {
	Index       int
	AccountName string
}

func GetAccountSortKey(types AccountTypes, accountName string) AccountSortKey {
	return AccountSortKey{
		Index:       types.Index(GetAccountType(accountName)),
		AccountName: accountName,
	}
}

// IsAccountType returns true if the account is of the given type.
func IsAccountType(accountType, accountName string) bool {
	return strings.HasPrefix(accountName, accountType+AccountSeparator)
}

// IsRootAccount returns true if the account name is a root account.
func IsRootAccount(accountName string) bool {
	return IsValidRoot(accountName)
}

// IsBalanceSheetAccount returns true if the account is Assets, Liabilities, or Equity.
func IsBalanceSheetAccount(accountName string, types AccountTypes) bool {
	t := GetAccountType(accountName)
	return t == types.Assets || t == types.Liabilities || t == types.Equity
}

// IsIncomeStatementAccount returns true if the account is Income or Expenses.
func IsIncomeStatementAccount(accountName string, types AccountTypes) bool {
	t := GetAccountType(accountName)
	return t == types.Income || t == types.Expenses
}

// IsEquityAccount returns true if the account is an Equity account.
func IsEquityAccount(accountName string, types AccountTypes) bool {
	return GetAccountType(accountName) == types.Equity
}

// IsInvertedAccount returns true if the account normally has a credit balance.
func IsInvertedAccount(accountName string, types AccountTypes) bool {
	t := GetAccountType(accountName)
	return t == types.Liabilities || t == types.Income || t == types.Equity
}

// GetAccountSign returns +1 or -1 based on the account's normal balance.
func GetAccountSign(accountName string, types *AccountTypes) int {
	if types == nil {
		types = &DEFAULT_ACCOUNT_TYPES
	}
	t := GetAccountType(accountName)
	if t == types.Assets || t == types.Expenses {
		return 1
	}
	return -1
}

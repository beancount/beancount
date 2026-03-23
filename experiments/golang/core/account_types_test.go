package core

import (
	"sort"
	"testing"
)

func TestAccountTypesBasics(t *testing.T) {
	if DEFAULT_ACCOUNT_TYPES.Assets != "Assets" {
		t.Errorf("DEFAULT_ACCOUNT_TYPES.Assets = %q, expected \"Assets\"", DEFAULT_ACCOUNT_TYPES.Assets)
	}
}

func TestGetAccountType(t *testing.T) {
	tests := []struct {
		account string
		expected string
	}{
		{"Assets:US:RBS:Checking", "Assets"},
		{"Assets:US:RBS:Savings", "Assets"},
		{"Liabilities:US:RBS:MortgageLoan", "Liabilities"},
		{"Equity:NetIncome", "Equity"},
		{"Equity:Opening-Balances", "Equity"},
		{"Income:US:ETrade:Dividends", "Income"},
		{"Income:US:Intel", "Income"},
		{"Expenses:Toys:Computer", "Expenses"},
		{"Invalid:Toys:Computer", "Invalid"},
	}
	for _, tc := range tests {
		if got := GetAccountType(tc.account); got != tc.expected {
			t.Errorf("GetAccountType(%q) = %q, expected %q", tc.account, got, tc.expected)
		}
	}
}

func TestGetAccountSortKey(t *testing.T) {
	types := DEFAULT_ACCOUNT_TYPES
	input := []string{
		"Expenses:Toys:Computer",
		"Income:US:Intel",
		"Income:US:ETrade:Dividends",
		"Equity:Opening-Balances",
		"Liabilities:US:RBS:MortgageLoan",
		"Equity:NetIncome",
		"Assets:US:RBS:Savings",
		"Assets:US:RBS:Checking",
	}
	expected := []string{
		"Assets:US:RBS:Checking",
		"Assets:US:RBS:Savings",
		"Liabilities:US:RBS:MortgageLoan",
		"Equity:NetIncome",
		"Equity:Opening-Balances",
		"Income:US:ETrade:Dividends",
		"Income:US:Intel",
		"Expenses:Toys:Computer",
	}

	sort.Slice(input, func(i, j int) bool {
		ki := GetAccountSortKey(types, input[i])
		kj := GetAccountSortKey(types, input[j])
		if ki.Index != kj.Index {
			return ki.Index < kj.Index
		}
		return ki.AccountName < kj.AccountName
	})

	for i := range expected {
		if input[i] != expected[i] {
			t.Errorf("At index %d, got %q, expected %q", i, input[i], expected[i])
		}
	}
}

func TestIsAccountType(t *testing.T) {
	if !IsAccountType("Assets", "Assets:US:RBS:Checking") {
		t.Errorf("IsAccountType(\"Assets\", \"Assets:US:RBS:Checking\") should be true")
	}
	if IsAccountType("Expenses", "Assets:US:RBS:Checking") {
		t.Errorf("IsAccountType(\"Expenses\", \"Assets:US:RBS:Checking\") should be false")
	}
	if IsAccountType("Assets", "AssetsUS:RBS:Checking") {
		t.Errorf("IsAccountType(\"Assets\", \"AssetsUS:RBS:Checking\") should be false")
	}
}

func TestIsRootAccount(t *testing.T) {
	tests := []struct {
		account string
		expected bool
	}{
		{"Assets:US:RBS:Checking", false},
		{"Equity:Opening-Balances", false},
		{"Income:US:ETrade:Dividends-USD", false},
		{"Assets", true},
		{"Liabilities", true},
		{"Equity", true},
		{"Income", true},
		{"Expenses", true},
		{"_invalid_", false},
		{"Invalid", true},
	}
	for _, tc := range tests {
		if got := IsRootAccount(tc.account); got != tc.expected {
			t.Errorf("IsRootAccount(%q) = %v, expected %v", tc.account, got, tc.expected)
		}
	}
}

func TestAccountCategories(t *testing.T) {
	types := DEFAULT_ACCOUNT_TYPES
	
	tests := []struct {
		account string
		isBS    bool
		inv     bool
		sign    int
	}{
		{"Assets:US:RBS:Savings", true, false, 1},
		{"Liabilities:US:RBS:MortgageLoan", true, true, -1},
		{"Equity:Opening-Balances", true, true, -1},
		{"Income:US:ETrade:Dividends", false, true, -1},
		{"Expenses:Toys:Computer", false, false, 1},
	}

	for _, tc := range tests {
		if got := IsBalanceSheetAccount(tc.account, types); got != tc.isBS {
			t.Errorf("IsBalanceSheetAccount(%q) = %v, expected %v", tc.account, got, tc.isBS)
		}
		if got := IsIncomeStatementAccount(tc.account, types); got != !tc.isBS {
			t.Errorf("IsIncomeStatementAccount(%q) = %v, expected %v", tc.account, got, !tc.isBS)
		}
		if got := IsInvertedAccount(tc.account, types); got != tc.inv {
			t.Errorf("IsInvertedAccount(%q) = %v, expected %v", tc.account, got, tc.inv)
		}
		if got := GetAccountSign(tc.account, &types); got != tc.sign {
			t.Errorf("GetAccountSign(%q) = %v, expected %v", tc.account, got, tc.sign)
		}
	}
}

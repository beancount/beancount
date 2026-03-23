package core

import (
	"reflect"
	"testing"
)

func TestAccountValidation(t *testing.T) {
	tests := []struct {
		input string
		root  bool
		valid bool
	}{
		{"Assets:US:RBS:Checking", false, true},
		{"Equity:Opening-Balances", false, true},
		{"Income:US:ETrade:Dividends-USD", false, true},
		{"Assets:US:RBS", false, true},
		{"Assets:US", false, true},
		{"Assets", true, false},
		{"Invalid", true, false},
		{"Other", true, false},
		{"Assets:US:RBS*Checking", false, false},
		{"Assets:US:RBS:Checking&", false, false},
		{"Assets:US:RBS:checking", false, false},
		{"Assets:us:RBS:checking", false, false},
		{"Assets:Checking123", false, true},
		{"Assets:123Checking", false, true},
	}

	for _, tc := range tests {
		if got := IsValidRoot(tc.input); got != tc.root {
			t.Errorf("IsValidRoot(%q) = %v, expected %v", tc.input, got, tc.root)
		}
		if got := IsValid(tc.input); got != tc.valid {
			t.Errorf("IsValid(%q) = %v, expected %v", tc.input, got, tc.valid)
		}
	}
}

func TestAccountJoinSplit(t *testing.T) {
	// Join
	if got := Join("Expenses", "Toys", "Computer"); got != "Expenses:Toys:Computer" {
		t.Errorf("Join = %q, expected \"Expenses:Toys:Computer\"", got)
	}
	if got := Join("Expenses"); got != "Expenses" {
		t.Errorf("Join = %q, expected \"Expenses\"", got)
	}
	if got := Join(); got != "" {
		t.Errorf("Join = %q, expected \"\"", got)
	}

	// Split
	if got := Split("Expenses:Toys:Computer"); !reflect.DeepEqual(got, []string{"Expenses", "Toys", "Computer"}) {
		t.Errorf("Split = %v, expected [Expenses Toys Computer]", got)
	}
	if got := Split("Expenses"); !reflect.DeepEqual(got, []string{"Expenses"}) {
		t.Errorf("Split = %v, expected [Expenses]", got)
	}
	if got := Split(""); got != nil {
		t.Errorf("Split(\"\") = %v, expected nil", got)
	}
}

func TestAccountHierarchy(t *testing.T) {
	acc := "Expenses:Toys:Computer"
	
	if got := Parent(acc); got != "Expenses:Toys" {
		t.Errorf("Parent(%q) = %q, expected \"Expenses:Toys\"", acc, got)
	}
	if got := Parent("Expenses:Toys"); got != "Expenses" {
		t.Errorf("Parent(\"Expenses:Toys\") = %q, expected \"Expenses\"", got)
	}
	if got := Parent("Expenses"); got != "" {
		t.Errorf("Parent(\"Expenses\") = %q, expected \"\"", got)
	}
	if got := Parent(""); got != "" {
		t.Errorf("Parent(\"\") = %q, expected \"\"", got)
	}

	if got := Leaf(acc); got != "Computer" {
		t.Errorf("Leaf(%q) = %q, expected \"Computer\"", acc, got)
	}
	if got := Leaf("Expenses:Toys"); got != "Toys" {
		t.Errorf("Leaf(\"Expenses:Toys\") = %q, expected \"Toys\"", got)
	}
	if got := Leaf("Expenses"); got != "Expenses" {
		t.Errorf("Leaf(\"Expenses\") = %q, expected \"Expenses\"", got)
	}
	if got := Leaf(""); got != "" {
		t.Errorf("Leaf(\"\") = %q, expected \"\"", got)
	}

	expectedParents := []string{"Assets:Bank:Checking", "Assets:Bank", "Assets"}
	if got := Parents("Assets:Bank:Checking"); !reflect.DeepEqual(got, expectedParents) {
		t.Errorf("Parents = %v, expected %v", got, expectedParents)
	}
}

func TestSansRoot(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"Expenses:Toys:Computer", "Toys:Computer"},
		{"Assets:US:BofA:Checking", "US:BofA:Checking"},
		{"Assets", ""},
		{"", ""},
	}

	for _, tc := range tests {
		if got := SansRoot(tc.input); got != tc.expected {
			t.Errorf("SansRoot(%q) = %q, expected %q", tc.input, got, tc.expected)
		}
	}
}

func TestRoot(t *testing.T) {
	name := "Liabilities:US:Credit-Card:Blue"
	tests := []struct {
		depth    int
		expected string
	}{
		{0, ""},
		{1, "Liabilities"},
		{2, "Liabilities:US"},
		{3, "Liabilities:US:Credit-Card"},
		{4, "Liabilities:US:Credit-Card:Blue"},
		{5, "Liabilities:US:Credit-Card:Blue"},
	}
	for _, tc := range tests {
		if got := Root(tc.depth, name); got != tc.expected {
			t.Errorf("Root(%d, %q) = %q, expected %q", tc.depth, name, got, tc.expected)
		}
	}
}

func TestCommonPrefix(t *testing.T) {
	tests := []struct {
		accounts []string
		expected string
	}{
		{[]string{"Assets:US:TD:Checking", "Assets:US:TD:Savings"}, "Assets:US:TD"},
		{[]string{"Assets:US:TD:Checking", "Assets:US:BofA:Checking"}, "Assets:US"},
		{[]string{"Assets:US:TD:Checking", "Assets:CA:RBC:Savings"}, "Assets"},
		{[]string{"Assets:US:TD:Checking", "Liabilities:US:CreditCard"}, ""},
		{[]string{""}, ""},
	}
	for _, tc := range tests {
		if got := CommonPrefix(tc.accounts); got != tc.expected {
			t.Errorf("CommonPrefix(%v) = %q, expected %q", tc.accounts, got, tc.expected)
		}
	}
}

func TestParentMatcher(t *testing.T) {
	isChild := ParentMatcher("Assets:Bank:Checking")
	if !isChild("Assets:Bank:Checking") {
		t.Errorf("ParentMatcher true fail")
	}
	if !isChild("Assets:Bank:Checking:SubAccount") {
		t.Errorf("ParentMatcher sub fail")
	}
	if isChild("Assets:Bank:CheckingOld") {
		t.Errorf("ParentMatcher suffix fail")
	}
	if isChild("Assets:Bank:Checking-Old") {
		t.Errorf("ParentMatcher hyphen fail")
	}
}

func TestAccountTransformer(t *testing.T) {
	xfr := NewAccountTransformer("__")
	acc := "Assets:US:BofA:Checking"
	if got := xfr.Render(acc); got != "Assets__US__BofA__Checking" {
		t.Errorf("Render = %q", got)
	}
	if got := xfr.Parse("Assets__US__BofA__Checking"); got != acc {
		t.Errorf("Parse = %q", got)
	}

	xfrNoop := NewAccountTransformer("")
	if got := xfrNoop.Render(acc); got != acc {
		t.Errorf("Render Noop = %q", got)
	}
	if got := xfrNoop.Parse(acc); got != acc {
		t.Errorf("Parse Noop = %q", got)
	}
}

func TestHasComponent(t *testing.T) {
	acc := "Liabilities:US:Credit-Card"
	tests := []struct {
		comp     string
		expected bool
	}{
		{"US", true},
		{"CA", false},
		{"Credit-Card", true},
		{"Liabilities", true},
		{"Credit", false},
		{"Card", false},
	}
	for _, tc := range tests {
		if got := HasComponent(acc, tc.comp); got != tc.expected {
			t.Errorf("HasComponent(%q, %q) = %v, expected %v", acc, tc.comp, got, tc.expected)
		}
	}
}

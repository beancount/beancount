package parser

import (
	"testing"

	"github.com/beancount/beancount/v3/core"
)

func TestCategorizeByCurrency(t *testing.T) {
	input := `
2020-01-01 * "Test"
  Assets:Bank:USD  100.00 USD
  Assets:Bank:CAD  -100.00 CAD
`
	directives, errs, _ := ParseString(input)
	if len(errs) > 0 {
		t.Fatalf("Parse failed: %v", errs)
	}
	txn := directives[0].(*core.Transaction)
	
	groups, errs := CategorizeByCurrency(txn, nil)
	if len(errs) > 0 {
		t.Fatalf("Categorize failed: %v", errs)
	}
	
	if len(groups) != 2 {
		t.Errorf("Expected 2 groups, got %d", len(groups))
	}
}

func TestReplaceCurrencies(t *testing.T) {
	input := `
2020-01-01 * "Test"
  Assets:Bank:USD
  Assets:Bank:CAD  -100.00 CAD
`
	directives, errs, _ := ParseString(input)
	if len(errs) > 0 {
		t.Fatalf("Parse failed: %v", errs)
	}
	txn := directives[0].(*core.Transaction)
	
	groups := []GroupEntry{
		{Currency: "USD", Refers: []Refer{{Index: 0, UnitsCurrency: "USD"}}},
		{Currency: "CAD", Refers: []Refer{{Index: 1, UnitsCurrency: "CAD"}}},
	}
	
	replaced := ReplaceCurrencies(txn.Postings, groups)
	if len(replaced) != 2 {
		t.Errorf("Expected 2 replaced groups, got %d", len(replaced))
	}
	
	if replaced[0].Postings[0].Units.Currency != "USD" {
		t.Errorf("Currency replacement failed for USD")
	}
}

package parser

import (
	"testing"

	"github.com/beancount/beancount/v3/core"
)

func TestParseBasic(t *testing.T) {
	input := `option "title" "Test Ledger"

2020-01-01 open Assets:Bank:Checking
2020-01-02 close Assets:Bank:Checking

2020-01-05 * "Store" "Groceries"
  Expenses:Food          100.00 USD
  Assets:Bank:Checking  -100.00 USD
`
	directives, errs, options := ParseString(input)
	if len(errs) > 0 {
		t.Fatalf("Parse failed with errors: %v", errs)
	}

	if len(options["title"]) == 0 || options["title"][0] != "Test Ledger" {
		t.Errorf("Expected option title='Test Ledger', got %v", options["title"])
	}

	if len(directives) != 3 {
		t.Fatalf("Expected 3 directives, got %d", len(directives))
	}

	d1, ok := directives[0].(*core.Open)
	if !ok || d1.Account != "Assets:Bank:Checking" || d1.Date.Format("2006-01-02") != "2020-01-01" {
		t.Errorf("Unexpected first directive: %v", d1)
	}

	d2, ok := directives[1].(*core.Close)
	if !ok || d2.Account != "Assets:Bank:Checking" {
		t.Errorf("Unexpected second directive: %v", d2)
	}

	d3, ok := directives[2].(*core.Transaction)
	if !ok || d3.Payee != "Store" || d3.Narration != "Groceries" || len(d3.Postings) != 2 {
		t.Fatalf("Unexpected third directive: %v", d3)
	}

	if d3.Postings[0].Account != "Expenses:Food" || d3.Postings[0].Units.Number.Cmp(core.D("100.00")) != 0 {
		t.Errorf("Unexpected posting 0: %v", d3.Postings[0])
	}

	if d3.Postings[1].Account != "Assets:Bank:Checking" || d3.Postings[1].Units.Number.Cmp(core.D("-100.00")) != 0 {
		t.Errorf("Unexpected posting 1: %v", d3.Postings[1])
	}
}

func TestParseComplexTxn(t *testing.T) {
	input := `2020-01-01 ! "Test" #tag ^link
  Assets:Investments   10 AAPL {150.00 USD} @@ 1600.00 USD
  Assets:Bank        -1600.00 USD
`
	directives, errs, _ := ParseString(input)
	if len(errs) > 0 {
		t.Fatalf("Parse failed: %v", errs)
	}
	
	if len(directives) != 1 {
		t.Fatalf("Expected 1 directive, got %d", len(directives))
	}

	txn := directives[0].(*core.Transaction)
	if txn.Flag != "!" || txn.Narration != "Test" {
		t.Errorf("Wrong flag/narration: %v", txn)
	}
	if _, ok := txn.Tags["tag"]; !ok {
		t.Errorf("Tag not found")
	}
	if _, ok := txn.Links["link"]; !ok {
		t.Errorf("Link not found")
	}

	p0 := txn.Postings[0]
	if p0.Account != "Assets:Investments" {
		t.Errorf("Wrong account: %s", p0.Account)
	}
	if p0.CostSpec == nil || p0.CostSpec.NumberPer.Cmp(core.D("150.00")) != 0 {
		t.Errorf("Wrong CostSpec: %v", p0.CostSpec)
	}
	if p0.Price.Number == nil || p0.Price.Number.Cmp(core.D("1600.00")) != 0 {
		t.Errorf("Wrong price: %v", p0.Price)
	}
}

func TestParseDirectives(t *testing.T) {
	input := `
2020-01-01 commodity USD
  name: "US Dollar"

2020-01-02 pad Assets:Bank:Savings Assets:Bank:Checking

2020-01-03 balance Assets:Bank:Savings 1000.00 USD ~ 0.01

2020-01-04 note Assets:Bank:Savings "Checking balance"

2020-01-05 document Assets:Bank:Savings "statement.pdf" #tag1 ^link1
  key: "val"

2020-01-06 price AAPL 150.00 USD

2020-01-07 event "location" "New York"

2020-01-08 query "assets" "SELECT account WHERE account ~ 'Assets'"

2020-01-09 custom "budget" "Food" 500.00 USD
`
	directives, errs, _ := ParseString(input)
	if len(errs) > 0 {
		t.Fatalf("Parse failed: %v", errs)
	}

	if len(directives) != 9 {
		t.Fatalf("Expected 9 directives, got %d", len(directives))
	}

	if d, ok := directives[0].(*core.Commodity); ok {
		if d.Currency != "USD" || d.Meta["name"] != "US Dollar" {
			t.Errorf("Wrong commodity: %v", d)
		}
	} else {
		t.Errorf("Expected Commodity")
	}

	if d, ok := directives[1].(*core.Pad); ok {
		if d.Account != "Assets:Bank:Savings" || d.SourceAccount != "Assets:Bank:Checking" {
			t.Errorf("Wrong pad: %v", d)
		}
	} else {
		t.Errorf("Expected Pad")
	}

	if d, ok := directives[2].(*core.Balance); ok {
		if d.Account != "Assets:Bank:Savings" || d.Amount.Number.Cmp(core.D("1000.00")) != 0 {
			t.Errorf("Wrong balance: %v", d)
		}
	} else {
		t.Errorf("Expected Balance")
	}

	if d, ok := directives[3].(*core.Note); ok {
		if d.Account != "Assets:Bank:Savings" || d.Comment != "Checking balance" {
			t.Errorf("Wrong note: %v", d)
		}
	} else {
		t.Errorf("Expected Note")
	}

	if d, ok := directives[4].(*core.Document); ok {
		if d.Account != "Assets:Bank:Savings" || d.Filename != "statement.pdf" || d.Meta["key"] != "val" {
			t.Errorf("Wrong document: %v", d)
		}
		if _, ok := d.Tags["tag1"]; !ok {
			t.Errorf("Missing tag1")
		}
	} else {
		t.Errorf("Expected Document")
	}

	if d, ok := directives[8].(*core.Custom); ok {
		if d.TypeStr != "budget" || len(d.Values) != 2 {
			t.Errorf("Wrong custom: %v", d)
		}
		if v, ok := d.Values[1].(core.Amount); ok {
			if v.Number.Cmp(core.D("500.00")) != 0 || v.Currency != "USD" {
				t.Errorf("Wrong custom amount value: %v", v)
			}
		} else {
			t.Errorf("Expected Amount as second value of custom, got %T", d.Values[1])
		}
	} else {
		t.Errorf("Expected Custom, got %T", directives[8])
	}
}

func TestParseStack(t *testing.T) {
	input := `
pushtag #tag1
pushmeta key: "val"

2020-01-01 * "Txn"
  Expenses:Food  10 USD
  Assets:Bank

popmeta key:
poptag #tag1

2020-01-02 * "No tag"
  Expenses:Food  10 USD
  Assets:Bank
`
	directives, errs, _ := ParseString(input)
	if len(errs) > 0 {
		t.Fatalf("Parse failed: %v", errs)
	}

	if len(directives) != 2 {
		t.Fatalf("Expected 2 directives, got %d", len(directives))
	}

	txn1 := directives[0].(*core.Transaction)
	if _, ok := txn1.Tags["tag1"]; !ok {
		t.Errorf("Missing tag1 in txn1")
	}
	if txn1.Meta["key"] != "val" {
		t.Errorf("Missing key meta in txn1: %v", txn1.Meta["key"])
	}

	txn2 := directives[1].(*core.Transaction)
	if _, ok := txn2.Tags["tag1"]; ok {
		t.Errorf("Unexpected tag1 in txn2")
	}
	if txn2.Meta["key"] != nil {
		t.Errorf("Unexpected key meta in txn2")
	}
}
